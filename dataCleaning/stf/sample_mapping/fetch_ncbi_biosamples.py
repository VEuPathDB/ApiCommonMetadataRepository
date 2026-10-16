#!/usr/bin/env python3
"""Fetch NCBI BioSample attributes for the sra-run rows of a sample mapping.

usage: fetch_ncbi_biosamples.py <dnaseq_sample_metadata_mapping.txt> <cacheDir>

Writes <cacheDir>/runs.tsv (run -> BioSample, BioProject) and
<cacheDir>/biosample_attributes.tsv (BioSample, attribute, value). Accessions
already in the cache are not fetched again; delete the cache to refresh it.
Set NCBI_API_KEY to raise the request rate limit.
"""
import csv, io, os, sys, time, urllib.parse, urllib.request
import xml.etree.ElementTree as ET

RUNINFO = 'https://trace.ncbi.nlm.nih.gov/Traces/sra-db-be/runinfo'
EUTILS = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils'
API_KEY = os.environ.get('NCBI_API_KEY', '')
PAUSE = 0.12 if API_KEY else 0.4
RUN_COLUMNS = ['run', 'biosample', 'bioproject', 'scientific_name']
ATTR_COLUMNS = ['biosample', 'attribute', 'value']


def get(url, params, tries=5):
    if API_KEY and url.startswith(EUTILS):
        params = {**params, 'api_key': API_KEY}
    data = urllib.parse.urlencode(params).encode()
    for attempt in range(tries):
        time.sleep(PAUSE)
        try:
            with urllib.request.urlopen(url, data=data, timeout=120) as r:
                return r.read().decode('utf-8')
        except OSError as e:
            if attempt == tries - 1:
                raise
            print(f'retrying after {e}', file=sys.stderr)
            time.sleep(2 ** attempt)


def batches(items, n):
    items = sorted(items)
    for i in range(0, len(items), n):
        yield items[i:i + n]


def read_cache(path):
    if not os.path.exists(path):
        return []
    with open(path, newline='') as f:
        return list(csv.DictReader(f, delimiter='\t'))


def append(path, columns, rows):
    new = not os.path.exists(path)
    with open(path, 'a', newline='') as f:
        w = csv.DictWriter(f, columns, delimiter='\t', lineterminator='\n')
        if new:
            w.writeheader()
        w.writerows(rows)


def fetch_runs(runs):
    for batch in batches(runs, 100):
        text = get(RUNINFO, {'acc': ','.join(batch)})
        for r in csv.DictReader(io.StringIO(text)):
            if r.get('Run') in batch:
                yield {'run': r['Run'], 'biosample': r['BioSample'], 'bioproject': r['BioProject'],
                       'scientific_name': r['ScientificName']}


def fetch_biosamples(accessions):
    for batch in batches(accessions, 200):
        term = ' OR '.join(f'{a}[accn]' for a in batch)
        found = ET.fromstring(get(f'{EUTILS}/esearch.fcgi', {'db': 'biosample', 'term': term,
                                                             'retmax': len(batch) * 2}))
        uids = [e.text for e in found.iter('Id')]
        if not uids:
            continue
        root = ET.fromstring(get(f'{EUTILS}/efetch.fcgi', {'db': 'biosample', 'id': ','.join(uids),
                                                            'retmode': 'xml'}))
        for bs in root.iter('BioSample'):
            acc = bs.get('accession')
            if acc not in batch:
                continue
            org = bs.find('Description/Organism')
            if org is not None and org.get('taxonomy_name'):
                yield {'biosample': acc, 'attribute': 'Organism', 'value': org.get('taxonomy_name')}
            for a in bs.iter('Attribute'):
                name = a.get('harmonized_name') or a.get('attribute_name')
                yield {'biosample': acc, 'attribute': name, 'value': (a.text or '').strip()}


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__.strip())
    mapping, cache = sys.argv[1], sys.argv[2]
    os.makedirs(cache, exist_ok=True)
    runs_path = os.path.join(cache, 'runs.tsv')
    attrs_path = os.path.join(cache, 'biosample_attributes.tsv')

    with open(mapping, newline='') as f:
        wanted = {run for r in csv.DictReader(f, delimiter='\t') if r['matchType'] == 'sra-run'
                  for run in r['joinKey'].split(',')}
    have = {r['run'] for r in read_cache(runs_path)}
    append(runs_path, RUN_COLUMNS, list(fetch_runs(wanted - have)))
    runs = read_cache(runs_path)
    missing_runs = wanted - {r['run'] for r in runs}

    biosamples = {r['biosample'] for r in runs if r['run'] in wanted and r['biosample']}
    have = {r['biosample'] for r in read_cache(attrs_path)}
    append(attrs_path, ATTR_COLUMNS, list(fetch_biosamples(biosamples - have)))
    missing_bs = biosamples - {r['biosample'] for r in read_cache(attrs_path)}

    for r in sorted(missing_runs):
        print(f'MISSING run\t{r}', file=sys.stderr)
    for b in sorted(missing_bs):
        print(f'MISSING biosample\t{b}', file=sys.stderr)
    print(f'{len(wanted)} runs, {len(biosamples)} biosamples; missing {len(missing_runs)} runs, '
          f'{len(missing_bs)} biosamples', file=sys.stderr)
    sys.exit(1 if missing_runs or missing_bs else 0)


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Map every dnaseq workflow sample to its metadata source.

usage: build_sample_mapping.py <VEuPathDatasets dir> <manualDelivery cache dir> <out.tsv>

Workflow samples come from each dataset's samplesheet.csv. A dataset whose ISA
study names it (as <org>_HTS_SNP_<name>_RSRC or <org>_copyNumberVariations_<name>_RSRC,
or the same with a trailing _N chunk number dropped) gets one row per sample per
ISA dataset. Otherwise an SRA-backed sample is marked for NCBI lookup by run.
Anything else is a gap, listed on stderr; the exit status is 1 if there are gaps.
"""
import collections, csv, glob, os, re, sys
import xml.etree.ElementTree as ET

from dnaseq_datasets import dnaseq_datasets

HERE = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
ISA_PREFIX = 'ApiCommonMetadataRepository/'
COLUMNS = ['metaDataRsrcName', 'metaRepoSampleName', 'workflowDataRsrcName', 'workflowSample',
           'matchType', 'metaDataSource', 'joinKey', 'projectName', 'organismAbbrev']
RUN = re.compile(r'^[SED]RR\d+$')


def read_tsv(path):
    with open(path, newline='') as f:
        return list(csv.DictReader(f, delimiter='\t'))


def isa_studies():
    """ISA dataset name -> [(metaDataSource, sample names)]; a dataset can be in several studies."""
    studies = collections.defaultdict(list)
    for inv in sorted(glob.glob(f'{REPO_ROOT}/ISA/metadata/*/i_Investigation.xml')):
        study_dir = os.path.dirname(inv)
        for study in ET.parse(inv).getroot().iter('study'):
            rel = os.path.relpath(os.path.join(study_dir, study.get('fileName')), REPO_ROOT)
            with open(os.path.join(REPO_ROOT, rel), encoding='utf-8', errors='replace') as f:
                rows = list(csv.reader(f, delimiter='\t'))
            names = [r[0].strip() for r in rows[1:] if r and r[0].strip()]
            for ds in study.iter('dataset'):
                studies[ds.text.strip()].append((ISA_PREFIX + rel, names))
    return studies


def isa_links(d, studies, extra_links):
    org, name = d['organismAbbrev'], d['name']
    candidates = [name]
    chunk = re.match(r'^(.*)_\d+$', name)
    if chunk:
        candidates.append(chunk.group(1))
    for n in candidates:
        found = [f'{org}_{kind}_{n}_RSRC' for kind in ('HTS_SNP', 'copyNumberVariations')
                 if f'{org}_{kind}_{n}_RSRC' in studies]
        if found:
            break
    return sorted(set(found) | extra_links.get(d['datasetName'], set()))


def homoglyph_key(s):
    return s.replace('I', 'l').lower()


def match(sample, names):
    if sample in names:
        return sample, 'exact'
    hits = [n for n in names if homoglyph_key(n) == homoglyph_key(sample)]
    if len(hits) == 1:
        return hits[0], 'normalized'
    stripped = re.sub(r'_(resequence\d+|reference_reads)$', '', sample)
    if stripped != sample and stripped in names:
        return stripped, 'suffix-stripped'
    return None, None


HEADER_ALIASES = {'id': 'sample', 'fastq1': 'fastq_1', 'fastq2': 'fastq_2'}


def samplesheet(path):
    runs = collections.OrderedDict()
    with open(path, newline='', encoding='utf-8-sig') as f:
        reader = csv.reader(f)
        header = [HEADER_ALIASES.get(h.strip(), h.strip()) for h in next(reader)]
        for values in reader:
            r = dict(zip(header, (v.strip() for v in values)))
            if r.get('sample'):
                runs.setdefault(r['sample'], []).append(r['fastq_1'])
    return runs


def build(veupath_datasets_dir, cache_dir):
    studies = isa_studies()
    extra_links = collections.defaultdict(set)
    for r in read_tsv(f'{HERE}/isa_dataset_links.tsv'):
        extra_links[r['workflowDataRsrcName']].add(r['metaDataRsrcName'])
    overrides = collections.defaultdict(list)
    for r in read_tsv(f'{HERE}/sample_overrides.tsv'):
        overrides[(r['workflowDataRsrcName'], r['workflowSample'])].append(r)

    rows, gaps, used = [], [], set()
    for d in dnaseq_datasets(veupath_datasets_dir):
        ds = d['datasetName']
        links = isa_links(d, studies, extra_links)
        for sample, fastqs in samplesheet(os.path.join(cache_dir, d['samplesheet'])).items():
            base = {'workflowDataRsrcName': ds, 'workflowSample': sample, 'joinKey': '',
                    'projectName': d['projectName'], 'organismAbbrev': d['organismAbbrev']}
            fixed = overrides.get((ds, sample), [])
            used.update((ds, sample, o['metaDataRsrcName']) for o in fixed)
            if fixed and not links:
                for o in fixed:
                    rows.append({**base, **{k: o[k] for k in ('metaDataRsrcName', 'metaRepoSampleName', 'matchType')},
                                 'metaDataSource': ''})
                continue
            for isa in links:
                o = next((o for o in fixed if o['metaDataRsrcName'] == isa), None)
                if o:
                    hits = [(src, o['metaRepoSampleName'], o['matchType']) for src, names in studies[isa]
                            if o['metaRepoSampleName'] in names]
                else:
                    hits = [(src, *match(sample, names)) for src, names in studies[isa]]
                    hits = [h for h in hits if h[1] is not None]
                if len(hits) != 1:
                    gaps.append((ds, sample, f'{len(hits)} sample matches in {isa}'))
                    continue
                src, meta, how = hits[0]
                rows.append({**base, 'metaDataRsrcName': isa, 'metaRepoSampleName': meta,
                             'matchType': how, 'metaDataSource': src})
            if links:
                continue
            if all(RUN.match(f) for f in fastqs):
                rows.append({**base, 'metaDataRsrcName': 'NCBI', 'metaRepoSampleName': sample,
                             'matchType': 'sra-run', 'metaDataSource': 'NCBI', 'joinKey': ','.join(fastqs)})
            else:
                gaps.append((ds, sample, 'no ISA study and not from SRA'))

    stale = {(o['workflowDataRsrcName'], o['workflowSample'], o['metaDataRsrcName'])
             for v in overrides.values() for o in v} - used
    gaps += [(ds, s, f'override for {m} matches no samplesheet row') for ds, s, m in sorted(stale)]
    rows.sort(key=lambda r: (r['workflowDataRsrcName'], r['workflowSample'], r['metaDataRsrcName']))
    return rows, gaps


def main():
    if len(sys.argv) != 4:
        sys.exit(__doc__.strip())
    rows, gaps = build(sys.argv[1], sys.argv[2])
    with open(sys.argv[3], 'w', newline='') as f:
        w = csv.DictWriter(f, COLUMNS, delimiter='\t', lineterminator='\n')
        w.writeheader()
        w.writerows(rows)
    for g in gaps:
        print('GAP\t' + '\t'.join(g), file=sys.stderr)
    counts = collections.Counter(r['matchType'] for r in rows)
    print(f'{len(rows)} rows: ' + ', '.join(f'{k} {v}' for k, v in sorted(counts.items())) +
          f'; {len(gaps)} gaps', file=sys.stderr)
    sys.exit(1 if gaps else 0)


if __name__ == '__main__':
    main()

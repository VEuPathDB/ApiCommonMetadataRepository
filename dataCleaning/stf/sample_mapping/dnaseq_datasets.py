#!/usr/bin/env python3
"""List every dnaseqExperiment dataset defined in a VEuPathDatasets checkout,
with the dataset presenter that covers it.

usage: dnaseq_datasets.py <VEuPathDatasets dir>   (TSV on stdout)
"""
import glob, os, re, sys
import xml.etree.ElementTree as ET

COLUMNS = ['datasetName', 'projectName', 'organismAbbrev', 'name', 'version', 'samplesheet',
           'datasetPresenter']
SAMPLESHEET = '{projectName}/{organismAbbrev}/SNP/{name}/{version}/final/samplesheet.csv'
PRESENTER_TAG = re.compile(r'<datasetPresenter\b[^>]*>', re.S)


def resolve(value, constants, path):
    def sub(m):
        if m.group(1) not in constants:
            sys.exit(f'{path}: undefined constant $${m.group(1)}$$')
        return constants[m.group(1)]
    return re.sub(r'\$\$(\w+)\$\$', sub, value)


def like(pattern):
    """A SQL LIKE pattern as a regex: % is any run, _ any one character."""
    return re.compile(''.join('.*' if c == '%' else '.' if c == '_' else re.escape(c) for c in pattern))


def presenters(veupath_datasets_dir):
    """Presenter name -> compiled datasetNamePattern, or None for an exact-name presenter.

    Read with a regex over the opening tags: not every presenter file parses as strict XML.
    """
    found = {}
    for path in sorted(glob.glob(os.path.join(veupath_datasets_dir, 'Model', 'lib', 'xml',
                                              'datasetPresenters', '*.xml'))):
        for tag in PRESENTER_TAG.findall(open(path, encoding='utf-8').read()):
            name = re.search(r'\bname="([^"]*)"', tag).group(1)
            pattern = re.search(r'\bdatasetNamePattern="([^"]*)"', tag)
            found[name] = like(pattern.group(1)) if pattern else None
    return found


def presenter_for(dataset_name, presenters):
    """The presenter whose page this dataset belongs to: its own, else the one pattern matching it."""
    if dataset_name in presenters:
        return dataset_name
    hits = [p for p, pattern in presenters.items() if pattern and pattern.fullmatch(dataset_name)]
    if len(hits) != 1:
        sys.exit(f'{dataset_name}: {len(hits)} dataset presenters match ({", ".join(hits)})')
    return hits[0]


def dnaseq_datasets(veupath_datasets_dir):
    root = os.path.join(veupath_datasets_dir, 'Datasets', 'lib', 'xml', 'datasets')
    by_presenter = presenters(veupath_datasets_dir)
    found = {}
    for path in sorted(glob.glob(f'{root}/**/*.xml', recursive=True)):
        tree = ET.parse(path).getroot()
        constants = {c.get('name'): c.get('value') for c in tree.iter('constant')}
        for ds in tree.iter('dataset'):
            if ds.get('class') != 'dnaseqExperiment':
                continue
            props = {p.get('name'): resolve(p.text or '', constants, path) for p in ds.iter('prop')}
            d = {k: props[k] for k in ('projectName', 'organismAbbrev', 'name', 'version')}
            d['datasetName'] = f"{d['organismAbbrev']}_{d['name']}_dnaseqExperiment_RSRC"
            d['samplesheet'] = SAMPLESHEET.format(**d)
            d['datasetPresenter'] = presenter_for(d['datasetName'], by_presenter)
            if d['datasetName'] in found:
                sys.exit(f"{path}: {d['datasetName']} defined twice")
            found[d['datasetName']] = d
    return [found[k] for k in sorted(found)]


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__.strip())
    print('\t'.join(COLUMNS))
    for d in dnaseq_datasets(sys.argv[1]):
        print('\t'.join(d[c] for c in COLUMNS))

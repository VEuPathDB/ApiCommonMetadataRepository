#!/usr/bin/env python3
"""List every dnaseqExperiment dataset defined in a VEuPathDatasets checkout.

usage: dnaseq_datasets.py <VEuPathDatasets dir>   (TSV on stdout)
"""
import glob, os, re, sys
import xml.etree.ElementTree as ET

COLUMNS = ['datasetName', 'projectName', 'organismAbbrev', 'name', 'version', 'samplesheet']
SAMPLESHEET = '{projectName}/{organismAbbrev}/SNP/{name}/{version}/final/samplesheet.csv'


def resolve(value, constants, path):
    def sub(m):
        if m.group(1) not in constants:
            sys.exit(f'{path}: undefined constant $${m.group(1)}$$')
        return constants[m.group(1)]
    return re.sub(r'\$\$(\w+)\$\$', sub, value)


def dnaseq_datasets(veupath_datasets_dir):
    root = os.path.join(veupath_datasets_dir, 'Datasets', 'lib', 'xml', 'datasets')
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

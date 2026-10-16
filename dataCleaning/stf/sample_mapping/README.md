# dnaseq sample mapping

Builds `dataCleaning/dnaseq_sample_metadata_mapping.txt`, which `stf/harmonize.py`
reads. Each row links one workflow sample to its metadata source. The output is
derived, so it isn't committed.

Pipeline outputs (`stf/studies/`, `stf/studies_harmonized/`, `stf/reports/` and
the `stf/*.tsv` reports `harmonize.py` writes) aren't committed either. Releases
live on yew under `common/dnasSeq/sampleDetailsForEda/<date>/`, each with a
`SOURCE` file naming the commit. To review a change, diff a rebuild against the
last release:

```bash
python3 stf/compare_to_production.py <release>/final stf/studies_harmonized diffs.tsv
```

It reports samples and columns removed or added, and changed cells, per organism.

```bash
python3 stf/sample_mapping/dnaseq_datasets.py $VEUPATH_DATASETS > /tmp/datasets.tsv
bash stf/sample_mapping/fetch_samplesheets.bash /tmp/datasets.tsv manualDelivery
python3 stf/sample_mapping/build_sample_mapping.py $VEUPATH_DATASETS manualDelivery dnaseq_sample_metadata_mapping.txt
python3 stf/sample_mapping/fetch_ncbi_biosamples.py dnaseq_sample_metadata_mapping.txt ncbi
```

`fetch_ncbi_biosamples.py` caches run → BioSample and the BioSample attributes
under `ncbi/`, which `harmonize.py` reads. The cache is committed so a rerun
uses the same NCBI records; only accessions missing from it are fetched.
Delete it to refresh, and review the diff. Attributes go through the same `SS`
header rules the provider spreadsheets used. If differently named samples share
one BioSample, that BioSample was registered per project, so none of them get
its attributes.

`$VEUPATH_DATASETS` is a VEuPathDatasets checkout on the build's branch. Datasets
are its `dnaseqExperiment` elements. Workflow samples are the `sample` column of
each dataset's `final/samplesheet.csv` on yew.

## Matching

A dataset's ISA studies are the ones that list `<org>_HTS_SNP_<name>_RSRC` or
`<org>_copyNumberVariations_<name>_RSRC`. If none do, the trailing `_N` chunk
number is dropped from `<name>` and the lookup is retried. Each workflow sample
must match exactly one study, trying these in order:

| matchType | rule |
| --- | --- |
| exact | same name |
| normalized | same after capital I → l and lowercasing |
| suffix-stripped | workflow name minus `_resequenceN` or `_reference_reads` |

A dataset with no ISA study uses its provider's spreadsheet when there is one:
`provider_metadata/<org>_<name>.tsv`, `<name>` with or without the `_N` chunk
number, first column the workflow sample. Its rows are `provider` matches, and a
sample missing from it is a gap. Add one when the provider sent columns NCBI
lacks. Otherwise the dataset gets `sra-run` rows whose `joinKey` is the sample's
SRA runs, for NCBI BioSample lookup.

Judgment calls are committed by hand:

- `isa_dataset_links.tsv` adds an ISA dataset to a workflow dataset.
- `sample_overrides.tsv` fixes a single sample's match.

Anything unresolved is printed as `GAP` and the exit status is 1. An override
that no longer matches a samplesheet row counts as a gap too.

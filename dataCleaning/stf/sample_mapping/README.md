# dnaseq sample mapping

Builds `dataCleaning/dnaseq_sample_metadata_mapping.txt`, which `stf/harmonize.py`
reads. Each row links one workflow sample to its metadata source. The output is
derived, so it isn't committed.

```bash
python3 stf/sample_mapping/dnaseq_datasets.py $VEUPATH_DATASETS > /tmp/datasets.tsv
bash stf/sample_mapping/fetch_samplesheets.bash /tmp/datasets.tsv manualDelivery
python3 stf/sample_mapping/build_sample_mapping.py $VEUPATH_DATASETS manualDelivery dnaseq_sample_metadata_mapping.txt
```

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

A dataset with no ISA study gets `sra-run` rows whose `joinKey` is the sample's SRA
runs. These are for NCBI BioSample lookup.

Judgment calls are committed by hand:

- `isa_dataset_links.tsv` adds an ISA dataset to a workflow dataset.
- `sample_overrides.tsv` fixes a single sample's match.

Anything unresolved is printed as `GAP` and the exit status is 1. An override
that no longer matches a samplesheet row counts as a gap too.

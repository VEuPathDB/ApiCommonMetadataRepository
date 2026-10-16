# Provider metadata

Per-sample metadata from providers or publications, for datasets no ISA study
covers. First column is the workflow sample name; other headers go through the
`SS` rules in `stf/harmonize.py`. A row with only the sample name means the
source has nothing for that sample.

| File | Source | Notes |
| --- | --- | --- |
| `pfal3D7_Neafsey_Thai-Myanmar_DNASeq.tsv` | Provider spreadsheet `combined-metadata.xlsx` Sheet1, from the retired dataCleaning repo | `Sample ID` moved to the first column; otherwise verbatim |
| `plinCAN1_Lmaculans_Chen_2021.tsv` | Chen et al. 2021, J Genet Genomics 48:994–1006, doi:10.1016/j.jgg.2021.06.019. Isolate list from the preprint's Additional Table 2, Research Square rs-24766/v2 (CC BY 4.0) | 76 of 141 samples match an isolate name. The 65 `AFCBL##` samples aren't in the table and have no values. `collection site` is verbatim; `Collection Location` is `Canada: <province>, <site>` from it; `time interval` is the year range as given |

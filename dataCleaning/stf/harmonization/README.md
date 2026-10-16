# STF harmonization (pass one)

Collapses inconsistent categorical *values* and fixes variable *naming* across the STF
studies in `stf/studies/`, driven entirely by declarative TSV rulebooks. **Inputs are
never modified**: harmonized studies are written to `stf/studies_harmonized/` and reports
to `stf/reports/`.

## Usage

```bash
export STUDY_WRANGLER_DIR=/path/to/study-wrangler                # any checkout
Rscript stf/harmonization/harmonize_stf.R                    # all studies
Rscript stf/harmonization/harmonize_stf.R PlasmoDB/pfal3D7   # selected studies
Rscript stf/harmonization/tests/run_tests.R                  # test suite
```

Per-study reports land in `stf/reports/$project/$organism/`: `changes.tsv` (every
substitution, with the layer that fired), `variable_actions.tsv`, `unresolved_values.tsv`,
`structural_findings.md`, `inspect_before_*.txt` / `inspect_after_*.txt`. Cross-study
rollups: `stf/reports/harmonization_summary.tsv` and `stf/reports/unresolved_values.tsv`
(the curation work queue). A failing study never aborts the run; it is reported instead.

## The three rule layers

| Layer | Path | Hand-edited? |
|---|---|---|
| 0 | `rules/generated/vocabulary_from_corpus.tsv` | **No — generated.** |
| 1 | `rules/global/{vocabulary,variables,non_vocabulary,approved_drops}.tsv` | Yes |
| 2 | `rules/studies/$project/$organism/<same filenames>` | Yes |

Precedence for vocabulary rules: **higher layer wins**; within a layer a
variable-specific rule beats a `*` wildcard. Values match on a normalized key (lowercased,
non-alphanumerics stripped), so `Blood` and `blood` are one key — two rows sharing a key
inside one layer is a hard error, not a coin flip. A layer-2 rule shadowing a layer-1 rule
is counted in `harmonization_summary.tsv` and warned about; promote it to global unless the
override is genuinely local.

`variables.tsv` actions (`rename`/`reparent`/`drop`) override per variable, layer 2 over 1. `non_vocabulary.tsv` (variables excluded from value harmonization: IDs,
dates, measurements) and `approved_drops.tsv` (populated variables the data owner has
authorised dropping) **union** instead of overriding: a study may add, never revoke.

## Regenerating layer 0

Layer 0 is derived from `stf/ontology_corpus.json`. Never hand-edit it; regenerate with
`write_corpus_vocabulary('stf/ontology_corpus.json', '<layer-0 path>')` and commit the
result. `test-corpus_rules.R` fails if the committed file has drifted from the corpus.

## Adding a rule

1. Find the value in `stf/reports/unresolved_values.tsv` (sorted by rows affected).
2. Add a row to layer 1, or to layer 2 if the fix is only correct for one study.
3. **`canonical_value` must be the ontology term's label, not the most frequent spelling
   in the data.** Check layer 0 first: if it already canonicalizes the term, copy that
   exact string (the USA canonical is `USA`, GAZ_00002459 — *not* "United States of
   America", which layer 0 maps *to* `USA`). Two spellings of one concept surviving is
   exactly the failure this rule prevents.
4. Fill `ontology_term_id` when known, and `note` always — an unreviewable rule is a
   liability. Blank `canonical_value` or `raw_value` is rejected: to remove a variable use
   a `drop` action, never an empty canonical.
5. Re-run the study and read its `changes.tsv` before committing.

## What this engine deliberately does NOT do

Pass one changes values and variable names only. Structural defects are **reported, not
fixed** — see each study's `structural_findings.md`. Reported checks:

- `country_pollution` — a site conflated into `country` (`Wang Pha, Thailand`); fixable
  only by splitting the variable, or by per-study curation.
- `country_noncanonical` — a country value with no ontology match.
- `age_units` — age carried as text with embedded units (`2 years`).
- `date_original_not_a_date` — `collection_date_original` holding a year or free text.
- `uninformative_value` — placeholders such as `specimen from organism`.
- `identifier_like` — near-unique free text in a declared categorical, e.g.
  `parasite_strain` / `fungal_strain`. **Deliberately kept**, by the data owner's decision.
- `all_null` — a variable with no values at all.
- `continent_incomplete` — partially populated `continent`, now dropped globally by
  authorisation in `approved_drops.tsv` as derivable from a clean `country`.

Splitting variables, unit extraction, date parsing and entity restructuring belong to pass
two. Dropping a populated variable is refused unless `approved_drops.tsv` lists it.

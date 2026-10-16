# STF Value Harmonization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a declarative, layered harmonization engine that reads one STF study at a time, collapses inconsistent categorical values and fixes variable naming, and writes corrected STF plus audit reports to a new output tree — never modifying inputs.

**Architecture:** A thin generic R engine (~200 lines total across 5 files) driven entirely by TSV rulebooks. Three rule layers merge with study-specific winning over global winning over a corpus-generated base. Two rule kinds only: value collapse (`vocabulary.tsv`) and variable name/parent/existence (`variables.tsv`). Structural defects are detected and reported but never applied.

**Tech Stack:** R 4.3.3, local install (no Docker). `study.wrangler` loaded via `devtools::load_all()`. `tidyverse`, `yaml`, `jsonlite`, `glue`. Tests via `testthat::test_dir()` (standalone, not a package).

**Spec:** `docs/superpowers/specs/2026-08-07-stf-harmonization-design.md`

---

## Domain Primer (read this first)

You likely have no context for this codebase. Key facts:

**STF** is a file format for a "study". One study directory contains:
- `study.yaml` — study name + list of entity names
- `entity-<name>.yaml` — variable metadata (name, display_name, definition, data_type, data_shape, parent_variable, provider_label)
- `entity-<name>.tsv` — the data. **The first column header has the form `sample_id \\ Descriptors`** — that is the ID column, and it is not a variable.

**`study.wrangler`** is a local R package at `study-wrangler/`. Load it with
`devtools::load_all('study-wrangler')`, never `library()` — it is not installed.

APIs you will use (all verified working):

| Call | Purpose |
|------|---------|
| `study_from_stf(dir)` | Load a study directory → `Study` object |
| `get_entity(study, name)` / `get_entities(study)` | Get `Entity` objects |
| `get_entity_name(entity)` | The entity's name, e.g. `"sample"` |
| `get_data(entity)` | The data as a tibble. **Read-only** |
| `get_variable_metadata(entity)` | Metadata tibble, one row per variable |
| `get_entity_id_column(entity)` | Name of the ID column — never harmonize this |
| `modify_data(entity, <dplyr verb>)` | The ONLY way to change data |
| `set_variable_metadata(entity, name, key=value, ...)` | Change metadata |
| `sync_variable_metadata(entity)` | Reconcile metadata with data columns |
| `validate(quiet(entity))` | → `TRUE`/`FALSE`. `validate(verbose(entity))` prints why |
| `study_from_entities(list(e), name='x')` | Build a `Study` from entities |
| `export_to_stf(study, dir)` | Write `study.yaml` + `entity-*` **directly into** `dir` |
| `inspect(entity)` | Prints variable/value summary. Used for before/after reports |

**Three traps already discovered. Do not rediscover them:**

1. **`sync_variable_metadata()` wipes metadata for columns it considers new.** After
   `modify_data(rename(new = old))`, the metadata row for `old` is deleted and `new` gets
   *defaults* — `display_name`, `definition`, `parent_variable` all become `NA`. Capture
   metadata BEFORE renaming, re-apply AFTER. (It does NOT touch metadata of unchanged
   columns, so it is safe after a `select(-x)` drop.)
2. **`provider_label` is a list column AND it is provenance.** Assign it as
   `provider_label = as.list(c('organism'))`, never a bare string. And a rename must
   restore the *original* provider label — `sync_variable_metadata()` overwrites it with
   the new column name, which is a lie about what the data provider called it.
3. **`modify_data()` cannot see your local variables.** It is an S4 method that splices its
   `...` into a magrittr pipe (`entity@data %>% ...`, `Entity-methods.R:749`). Under
   `standardGeneric` dispatch the unevaluated verb is re-evaluated in a frame where your
   loop locals do not exist, so `modify_data(mutate(!!sym(v) := new_col))` fails with
   `object 'v' not found`. Both the name and the value must be injected before handover:
   ```r
   entity <- rlang::eval_bare(
     rlang::expr(modify_data(entity, mutate(!!rlang::sym(v) := !!new_col))))
   ```
   Any code building a column name programmatically will hit this.
4. **`fct_recode` breaks validation.** STF categorical columns load as `character` with
   `data_type: string`. `fct_recode` returns a factor, the class stops matching the
   declared type, and `validate()` returns `FALSE`. Always coerce back to the original
   column class after substitution.

---

## File Structure

All new code lives under `stf/harmonization/`. Nothing inside `study-wrangler/` is modified.

| Path | Responsibility |
|------|----------------|
| `stf/harmonization/R/normalize.R` | The single value-normalization function. Shared by every layer so keys always match. |
| `stf/harmonization/R/corpus_rules.R` | Generate layer 0 vocabulary from `stf/ontology_corpus.json`. |
| `stf/harmonization/R/rulebook.R` | Load layers 0/1/2, validate them, merge with precedence, emit shadow warnings. |
| `stf/harmonization/R/apply_rules.R` | Apply a merged rulebook to an `Entity`. Returns entity + change log. |
| `stf/harmonization/R/structural_report.R` | Detect deferred (structural) defects. Read-only, makes no changes. |
| `stf/harmonization/R/report.R` | Write all TSV/markdown outputs. |
| `stf/harmonization/harmonize_stf.R` | CLI entry point + orchestration + exit status. |
| `stf/harmonization/rules/generated/vocabulary_from_corpus.tsv` | Layer 0. Generated. Never hand-edited. |
| `stf/harmonization/rules/global/vocabulary.tsv` | Layer 1 values. Hand-authored. |
| `stf/harmonization/rules/global/variables.tsv` | Layer 1 variable actions. Hand-authored. |
| `stf/harmonization/rules/studies/PlasmoDB/pfal3D7/vocabulary.tsv` | Layer 2 example. |
| `stf/harmonization/tests/testthat/helper-setup.R` | Sources all `R/*.R` + loads study.wrangler. |
| `stf/harmonization/tests/testthat/test-*.R` | Unit + integration tests. |
| `stf/harmonization/tests/testthat/fixtures/` | Small hand-written STF studies. |
| `stf/harmonization/tests/run_tests.R` | Test runner. |

Outputs (created at runtime, git-ignored):
`stf/studies_harmonized/$project/$org/` and `stf/reports/$project/$org/`.

---

## Task 0: Initialise version control and scaffold

Spec §9 flags that `dataCleaning` is not a git repository, so the commit steps in every
later task would fail. Fix that first.

**Files:**
- Create: `.gitignore`
- Create: `stf/harmonization/tests/run_tests.R`
- Create: `stf/harmonization/tests/testthat/helper-setup.R`

- [ ] **Step 1: Initialise the repo on a feature branch**

```bash
cd /home/jbrestel/dataCleaning
git init
git checkout -b feature/stf-harmonization
```

- [ ] **Step 2: Create `.gitignore`**

Large data artifacts and generated outputs must not be committed. Write
`/home/jbrestel/dataCleaning/.gitignore`:

```gitignore
# generated harmonization outputs
stf/studies_harmonized/
stf/reports/

# large source data
*.tar.gz
.nextflow/
.nextflow.log*
work/
results/

# R
.Rhistory
.RData
.Rproj.user/
```

- [ ] **Step 3: Create the test helper**

Because this is a script collection and not an R package, `testthat` needs to be told what
to load. Write `stf/harmonization/tests/testthat/helper-setup.R`:

```r
# Loaded automatically by testthat before any test file.
suppressPackageStartupMessages({
  library(tidyverse)
  library(testthat)
})

# Repo root, derived from this file's location so tests run from any cwd.
HARMONIZATION_DIR <- normalizePath(file.path(dirname(dirname(getwd()))))
REPO_ROOT <- normalizePath(file.path(HARMONIZATION_DIR, '..', '..'))

suppressPackageStartupMessages(
  devtools::load_all(file.path(REPO_ROOT, 'study-wrangler'), quiet = TRUE)
)

# Source every engine file.
for (f in list.files(file.path(HARMONIZATION_DIR, 'R'), pattern = '\\.R$', full.names = TRUE)) {
  source(f)
}
```

- [ ] **Step 4: Create the test runner**

Write `stf/harmonization/tests/run_tests.R`:

```r
#!/usr/bin/env Rscript
# Run all harmonization tests.  Usage: Rscript stf/harmonization/tests/run_tests.R
library(testthat)
setwd(file.path(dirname(sys.frame(1)$ofile %||% '.'), 'testthat'))
res <- test_dir('.', reporter = 'summary', stop_on_failure = TRUE)
```

Replace the fragile `sys.frame` line with an explicit path — write this instead:

```r
#!/usr/bin/env Rscript
# Run all harmonization tests.  Usage: Rscript stf/harmonization/tests/run_tests.R
library(testthat)
here <- dirname(normalizePath(sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))))
setwd(file.path(here, 'testthat'))
test_dir('.', reporter = 'summary', stop_on_failure = TRUE)
```

- [ ] **Step 5: Create the R directory with a placeholder so sourcing succeeds**

```bash
mkdir -p /home/jbrestel/dataCleaning/stf/harmonization/R
mkdir -p /home/jbrestel/dataCleaning/stf/harmonization/tests/testthat/fixtures
mkdir -p /home/jbrestel/dataCleaning/stf/harmonization/rules/generated
mkdir -p /home/jbrestel/dataCleaning/stf/harmonization/rules/global
mkdir -p /home/jbrestel/dataCleaning/stf/harmonization/rules/studies/PlasmoDB/pfal3D7
```

- [ ] **Step 6: Verify the harness runs (zero tests is a pass)**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: completes without error. A "no tests found" style result is fine; an error
sourcing `helper-setup.R` is not.

- [ ] **Step 7: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add .gitignore stf/harmonization
git commit -m "chore: init repo and scaffold STF harmonization test harness"
```

---

## Task 1: Value normalization

One function, used by every layer, so a corpus key and a rulebook key for the same value
always collide. Must match `harmonize.py`'s `normh()` exactly (lowercase, strip all
non-alphanumerics) so the rulebook is portable upstream.

**Files:**
- Create: `stf/harmonization/R/normalize.R`
- Test: `stf/harmonization/tests/testthat/test-normalize.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-normalize.R`:

```r
test_that("normalize_value matches harmonize.py normh semantics", {
  expect_equal(norm_value('Homo sapiens'), 'homosapiens')
  expect_equal(norm_value('human'), 'human')
  expect_equal(norm_value('Human'), 'human')
  expect_equal(norm_value('  Blood  '), 'blood')
  expect_equal(norm_value('[Candida] auris'), 'candidaauris')
  expect_equal(norm_value('S. Africa'), 'safrica')
  expect_equal(norm_value('Wang Pha, Thailand'), 'wangphathailand')
})

test_that("norm_value is vectorised and NA-safe", {
  expect_equal(norm_value(c('Blood', 'blood', NA)), c('blood', 'blood', NA))
})

test_that("norm_value collapses the known Homo sapiens variants to one key", {
  expect_equal(length(unique(norm_value(c('human', 'Human')))), 1)
  # 'Homo sapiens' is a DIFFERENT key; the corpus is what unifies it with 'human'
  expect_false(norm_value('Homo sapiens') == norm_value('human'))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "norm_value"`

- [ ] **Step 3: Write minimal implementation**

Write `stf/harmonization/R/normalize.R`:

```r
#' Normalize a value for rule-key matching.
#'
#' Must stay byte-identical in behaviour to harmonize.py's normh():
#' lowercase, then strip every character that is not a-z or 0-9.
#' This is what lets a rulebook authored here be lifted into harmonize.py unchanged.
#'
#' @param x character vector
#' @return character vector of keys; NA in, NA out
norm_value <- function(x) {
  out <- gsub('[^a-z0-9]', '', tolower(as.character(x)))
  out[is.na(x)] <- NA_character_
  out
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS, 3 test blocks

- [ ] **Step 5: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/R/normalize.R stf/harmonization/tests/testthat/test-normalize.R
git commit -m "feat: add value normalization matching harmonize.py normh"
```

---

## Task 2: Generate layer 0 from the ontology corpus

`stf/ontology_corpus.json` has shape:

```json
{"corpus": {"<normalized_key>": {"source_id": "NCBITaxon_9606",
                                 "type": "characteristicValue",
                                 "canonical": "Homo sapiens",
                                 "spellings": ["Homo sapiens", "human"]}},
 "terms": [ ...same objects as a list... ]}
```

Only entries with `type == "characteristicValue"` are value vocabulary. The other types
(`characteristicQualifier`, `protocol`, `unit`, `materialType`) describe headers and
protocols, not cell values, and must be excluded.

**Files:**
- Create: `stf/harmonization/R/corpus_rules.R`
- Test: `stf/harmonization/tests/testthat/test-corpus_rules.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-corpus_rules.R`:

```r
test_that("corpus_to_vocabulary emits one row per spelling, wildcard-scoped", {
  corpus <- list(corpus = list(
    human = list(source_id = 'NCBITaxon_9606', type = 'characteristicValue',
                 canonical = 'Homo sapiens', spellings = list('Homo sapiens', 'human')),
    blood = list(source_id = 'OBI_0000655', type = 'characteristicValue',
                 canonical = 'blood', spellings = list('blood', 'blood specimen'))
  ))
  v <- corpus_to_vocabulary(corpus)

  expect_setequal(colnames(v),
    c('variable', 'raw_value', 'canonical_value', 'ontology_term_id', 'note'))
  # 2 terms x 2 spellings
  expect_equal(nrow(v), 4)
  # corpus rules are variable-agnostic
  expect_true(all(v$variable == '*'))
  expect_setequal(v$raw_value, c('Homo sapiens', 'human', 'blood', 'blood specimen'))
  expect_equal(v %>% filter(raw_value == 'human') %>% pull(canonical_value), 'Homo sapiens')
  expect_equal(v %>% filter(raw_value == 'human') %>% pull(ontology_term_id), 'NCBITaxon_9606')
})

test_that("corpus_to_vocabulary excludes non-value term types", {
  corpus <- list(corpus = list(
    country = list(source_id = 'ENVO_00000009', type = 'characteristicQualifier',
                   canonical = 'country', spellings = list('country')),
    blood   = list(source_id = 'OBI_0000655', type = 'characteristicValue',
                   canonical = 'blood', spellings = list('blood'))
  ))
  v <- corpus_to_vocabulary(corpus)
  expect_equal(nrow(v), 1)
  expect_equal(v$canonical_value, 'blood')
})

test_that("a term whose spelling equals its canonical still yields a row", {
  corpus <- list(corpus = list(
    female = list(source_id = 'PATO_0000383', type = 'characteristicValue',
                  canonical = 'female', spellings = list('female'))
  ))
  expect_equal(nrow(corpus_to_vocabulary(corpus)), 1)
})

test_that("the real corpus produces the known Homo sapiens collapse", {
  v <- corpus_to_vocabulary(read_ontology_corpus(
    file.path(REPO_ROOT, 'stf', 'ontology_corpus.json')))
  hs <- v %>% filter(norm_value(raw_value) %in% norm_value(c('human', 'Human', 'Homo sapiens')))
  expect_true(all(hs$canonical_value == 'Homo sapiens'))
  expect_true(all(hs$ontology_term_id == 'NCBITaxon_9606'))
  expect_gt(nrow(v), 300)   # 361 characteristicValue terms, >=1 spelling each
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "corpus_to_vocabulary"`

- [ ] **Step 3: Write minimal implementation**

Write `stf/harmonization/R/corpus_rules.R`:

```r
VOCAB_COLS <- c('variable', 'raw_value', 'canonical_value', 'ontology_term_id', 'note')

#' Read the ontology corpus JSON.
read_ontology_corpus <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Build the layer-0 vocabulary from the ontology corpus.
#'
#' Emits one rule per (spelling -> canonical) for every characteristicValue term.
#' Scoped to variable '*' because these are value vocabularies that apply wherever the
#' value appears (organism, host_common_name and lab_host share a host-species vocabulary).
corpus_to_vocabulary <- function(corpus) {
  terms <- corpus$corpus %>% keep(~ identical(.x$type, 'characteristicValue'))

  if (length(terms) == 0) {
    return(tibble(variable = character(), raw_value = character(),
                  canonical_value = character(), ontology_term_id = character(),
                  note = character()))
  }

  all_rules <- terms %>%
    imap(function(term, key) {
      spellings <- unique(c(unlist(term$spellings), term$canonical))
      tibble(
        variable         = '*',
        raw_value        = spellings,
        canonical_value  = term$canonical,
        ontology_term_id = term$source_id %||% NA_character_,
        note             = 'generated from ontology_corpus.json'
      )
    }) %>%
    bind_rows()

  # AMENDED during execution: the engine matches on the NORMALIZED key, so two spellings
  # that normalize alike are redundant and must not both survive -- Task 3's validator
  # rejects duplicate normalized keys, and the real corpus contains one such pair
  # ('Vietnam' / 'Viet Nam', both -> GAZ_00003756). Collapsing is lossless ONLY when the
  # canonicals agree; disagreement is a corpus curation error and must surface loudly.
  conflicts <- all_rules %>%
    mutate(key = norm_value(raw_value)) %>%
    group_by(variable, key) %>%
    filter(n_distinct(canonical_value) > 1) %>%
    ungroup()

  if (nrow(conflicts) > 0) {
    detail <- conflicts %>%
      group_by(key) %>%
      summarise(canonicals = paste(sort(unique(canonical_value)), collapse = ' vs '),
                spellings = paste(sort(unique(raw_value)), collapse = ', '),
                .groups = 'drop') %>%
      mutate(line = glue::glue("  key '{key}': {canonicals}  (from: {spellings})")) %>%
      pull(line)
    stop(paste0(
      'ontology_corpus.json has spellings that normalize to the same key but map to ',
      'different canonical values. Fix the corpus; this cannot be resolved here:\n',
      paste(detail, collapse = '\n')))
  }

  all_rules %>%
    mutate(key = norm_value(raw_value)) %>%
    distinct(variable, key, .keep_all = TRUE) %>%
    select(all_of(VOCAB_COLS))
}

#' Write layer 0 to disk.
write_corpus_vocabulary <- function(corpus_path, out_path) {
  v <- corpus_to_vocabulary(read_ontology_corpus(corpus_path))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(v, out_path, na = '')
  v
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS

- [ ] **Step 5: Generate the layer-0 file and eyeball it**

```bash
cd /home/jbrestel/dataCleaning
Rscript -e "
suppressPackageStartupMessages(library(tidyverse))
for (f in list.files('stf/harmonization/R', pattern='[.]R\$', full.names=TRUE)) source(f)
v <- write_corpus_vocabulary('stf/ontology_corpus.json',
                             'stf/harmonization/rules/generated/vocabulary_from_corpus.tsv')
cat('rows:', nrow(v), '\n')
print(v %>% filter(canonical_value == 'Homo sapiens'))
"
```
Expected: >300 rows; the `Homo sapiens` block lists both `Homo sapiens` and `human` with
`NCBITaxon_9606`.

- [ ] **Step 6: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/R/corpus_rules.R \
        stf/harmonization/tests/testthat/test-corpus_rules.R \
        stf/harmonization/rules/generated/vocabulary_from_corpus.tsv
git commit -m "feat: generate layer-0 vocabulary from ontology corpus"
```

---

## Task 3: Rulebook loading, validation and layer merge

Precedence, fully specified (spec §4.1): **layer first, specificity second.** Layer 2 beats
layer 1 beats layer 0. Within one layer, a variable-specific rule beats a `*` wildcard.

**Files:**
- Create: `stf/harmonization/R/rulebook.R`
- Test: `stf/harmonization/tests/testthat/test-rulebook.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-rulebook.R`:

```r
# helper: build a vocabulary tibble inline
vocab <- function(...) {
  rows <- list(...)
  bind_rows(rows) %>%
    mutate(ontology_term_id = NA_character_, note = NA_character_) %>%
    select(all_of(VOCAB_COLS))
}
vrow <- function(variable, raw_value, canonical_value) {
  tibble(variable = variable, raw_value = raw_value, canonical_value = canonical_value)
}

test_that("merge_vocabulary_layers applies layer precedence 2 > 1 > 0", {
  merged <- merge_vocabulary_layers(
    layer0 = vocab(vrow('*', 'human', 'Homo sapiens')),
    layer1 = vocab(vrow('organism', 'human', 'human being')),
    layer2 = vocab(vrow('organism', 'human', 'LOCAL')))
  hit <- merged %>% filter(variable == 'organism', key == norm_value('human'))
  expect_equal(hit$canonical_value, 'LOCAL')
  expect_equal(hit$layer, 2L)
})

test_that("within a layer, a variable-specific rule beats a wildcard", {
  merged <- merge_vocabulary_layers(
    layer0 = vocab(),
    layer1 = vocab(vrow('*', 'blood', 'WILDCARD'),
                   vrow('specimen', 'blood', 'SPECIFIC')),
    layer2 = vocab())
  expect_equal(
    resolve_value(merged, 'specimen', 'blood')$canonical_value, 'SPECIFIC')
})

test_that("a layer-2 wildcard beats a layer-1 specific rule (layer wins first)", {
  merged <- merge_vocabulary_layers(
    layer0 = vocab(),
    layer1 = vocab(vrow('specimen', 'blood', 'L1_SPECIFIC')),
    layer2 = vocab(vrow('*', 'blood', 'L2_WILDCARD')))
  expect_equal(
    resolve_value(merged, 'specimen', 'blood')$canonical_value, 'L2_WILDCARD')
})

test_that("resolve_value falls back to a wildcard when no specific rule exists", {
  merged <- merge_vocabulary_layers(
    layer0 = vocab(vrow('*', 'human', 'Homo sapiens')),
    layer1 = vocab(), layer2 = vocab())
  expect_equal(resolve_value(merged, 'lab_host', 'Human')$canonical_value, 'Homo sapiens')
})

test_that("resolve_value returns zero rows when nothing matches", {
  merged <- merge_vocabulary_layers(vocab(), vocab(), vocab())
  expect_equal(nrow(resolve_value(merged, 'specimen', 'blood')), 0)
})

test_that("a duplicate (variable, raw_value) key within one layer is a hard error", {
  expect_error(
    validate_vocabulary_layer(
      vocab(vrow('specimen', 'Blood', 'blood'),
            vrow('specimen', 'blood', 'BLOOD')),   # same normalized key
      'global'),
    'Duplicate.*specimen')
})

test_that("a study rule shadowing a global rule produces a warning", {
  expect_warning(
    merge_vocabulary_layers(
      layer0 = vocab(),
      layer1 = vocab(vrow('organism', 'human', 'Homo sapiens')),
      layer2 = vocab(vrow('organism', 'human', 'human'))),
    'shadow')
})

test_that("a study rule that does not collide with a global rule warns nothing", {
  expect_silent(
    merge_vocabulary_layers(
      layer0 = vocab(),
      layer1 = vocab(vrow('organism', 'human', 'Homo sapiens')),
      layer2 = vocab(vrow('country', 'PLU', 'Mae Ra Mat Hospital'))))
})

test_that("count_shadowed_rules reports the shadow count for the summary", {
  expect_equal(
    suppressWarnings(count_shadowed_rules(
      layer1 = vocab(vrow('organism', 'human', 'Homo sapiens')),
      layer2 = vocab(vrow('organism', 'human', 'human')))),
    1L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "merge_vocabulary_layers"`

- [ ] **Step 3: Write minimal implementation**

Write `stf/harmonization/R/rulebook.R`:

```r
VARIABLES_COLS <- c('variable', 'action', 'new_name', 'new_display_name',
                    'new_parent', 'reason')
VALID_ACTIONS  <- c('rename', 'reparent', 'drop')

empty_vocabulary <- function() {
  tibble(variable = character(), raw_value = character(),
         canonical_value = character(), ontology_term_id = character(),
         note = character())
}

empty_variable_actions <- function() {
  tibble(variable = character(), action = character(), new_name = character(),
         new_display_name = character(), new_parent = character(), reason = character())
}

#' Read a rules TSV, returning an empty typed tibble when the file is absent.
read_rules_tsv <- function(path, expected_cols, empty_fn) {
  if (!file.exists(path)) return(empty_fn())
  df <- readr::read_tsv(path, col_types = readr::cols(.default = readr::col_character()),
                        na = character())
  missing <- setdiff(expected_cols, colnames(df))
  if (length(missing) > 0) {
    stop(glue::glue("'{path}' is missing required column(s): {toString(missing)}"))
  }
  df %>%
    mutate(across(everything(), ~ na_if(trimws(.x), ''))) %>%
    select(all_of(expected_cols))
}

read_vocabulary <- function(path) read_rules_tsv(path, VOCAB_COLS, empty_vocabulary)
read_variable_actions <- function(path) {
  read_rules_tsv(path, VARIABLES_COLS, empty_variable_actions)
}

#' Reject ambiguous curation inside a single layer.
validate_vocabulary_layer <- function(v, layer_label) {
  if (nrow(v) == 0) return(invisible(v))
  dupes <- v %>%
    mutate(key = norm_value(raw_value)) %>%
    count(variable, key) %>%
    filter(n > 1)
  if (nrow(dupes) > 0) {
    stop(glue::glue(
      "Duplicate rule key(s) in the '{layer_label}' vocabulary layer: ",
      "{paste(dupes$variable, dupes$key, sep='/', collapse=', ')}. ",
      "Ambiguous curation must be resolved, not silently picked."))
  }
  invisible(v)
}

validate_variable_actions <- function(a, layer_label) {
  if (nrow(a) == 0) return(invisible(a))
  bad <- setdiff(a$action, VALID_ACTIONS)
  if (length(bad) > 0) {
    stop(glue::glue("Invalid action(s) in '{layer_label}' variables layer: {toString(bad)}. ",
                    "Valid: {toString(VALID_ACTIONS)}"))
  }
  no_reason <- a %>% filter(is.na(reason) | reason == '')
  if (nrow(no_reason) > 0) {
    stop(glue::glue("Variable action(s) without a reason in '{layer_label}': ",
                    "{toString(no_reason$variable)}. A rename or drop without a stated ",
                    "reason is unreviewable."))
  }
  dupes <- a %>% count(variable) %>% filter(n > 1)
  if (nrow(dupes) > 0) {
    stop(glue::glue("Multiple actions for variable(s) in '{layer_label}': ",
                    "{toString(dupes$variable)}"))
  }
  invisible(a)
}

#' How many layer-2 rules collide with a layer-1 rule on the same key?
count_shadowed_rules <- function(layer1, layer2) {
  if (nrow(layer1) == 0 || nrow(layer2) == 0) return(0L)
  k1 <- layer1 %>% transmute(variable, key = norm_value(raw_value))
  k2 <- layer2 %>% transmute(variable, key = norm_value(raw_value))
  nrow(dplyr::inner_join(k1, k2, by = c('variable', 'key')))
}

#' Merge the three layers into one lookup keyed (variable, key).
#'
#' Precedence: layer number descending, then specificity (non-wildcard beats '*').
merge_vocabulary_layers <- function(layer0, layer1, layer2) {
  validate_vocabulary_layer(layer0, 'generated')
  validate_vocabulary_layer(layer1, 'global')
  validate_vocabulary_layer(layer2, 'study')

  n_shadowed <- count_shadowed_rules(layer1, layer2)
  if (n_shadowed > 0) {
    shadowed <- dplyr::inner_join(
      layer1 %>% transmute(variable, key = norm_value(raw_value), global = canonical_value),
      layer2 %>% transmute(variable, key = norm_value(raw_value), study  = canonical_value),
      by = c('variable', 'key'))
    warning(glue::glue(
      "{n_shadowed} study rule(s) shadow a global rule: ",
      "{paste(shadowed$variable, shadowed$key, ' global=', shadowed$global,
               ' study=', shadowed$study, collapse='; ')}. ",
      "Promote to global, or confirm the local override is intended."),
      call. = FALSE)
  }

  bind_rows(
    layer0 %>% mutate(layer = 0L),
    layer1 %>% mutate(layer = 1L),
    layer2 %>% mutate(layer = 2L)
  ) %>%
    mutate(key = norm_value(raw_value),
           specificity = if_else(variable == '*', 0L, 1L)) %>%
    arrange(desc(layer), desc(specificity)) %>%
    distinct(variable, key, .keep_all = TRUE)
}

#' Look up the winning rule for one (variable, value). Zero rows if none.
resolve_value <- function(merged, variable_name, raw) {
  if (nrow(merged) == 0) return(merged)
  k <- norm_value(raw)
  merged %>%
    filter(key == k, variable %in% c(variable_name, '*')) %>%
    arrange(desc(layer), desc(specificity)) %>%
    head(1)
}

#' Load all three layers for one study.
load_rulebook <- function(rules_dir, project, organism) {
  study_dir <- file.path(rules_dir, 'studies', project, organism)
  layers <- list(
    vocab0 = read_vocabulary(file.path(rules_dir, 'generated',
                                       'vocabulary_from_corpus.tsv')),
    vocab1 = read_vocabulary(file.path(rules_dir, 'global', 'vocabulary.tsv')),
    vocab2 = read_vocabulary(file.path(study_dir, 'vocabulary.tsv')),
    acts1  = read_variable_actions(file.path(rules_dir, 'global', 'variables.tsv')),
    acts2  = read_variable_actions(file.path(study_dir, 'variables.tsv'))
  )
  validate_variable_actions(layers$acts1, 'global')
  validate_variable_actions(layers$acts2, 'study')

  n_shadowed <- count_shadowed_rules(layers$vocab1, layers$vocab2)
  merged_vocab <- merge_vocabulary_layers(layers$vocab0, layers$vocab1, layers$vocab2)

  # study actions override global actions for the same variable
  merged_acts <- bind_rows(layers$acts2 %>% mutate(layer = 2L),
                           layers$acts1 %>% mutate(layer = 1L)) %>%
    arrange(desc(layer)) %>%
    distinct(variable, .keep_all = TRUE)

  list(vocabulary = merged_vocab, actions = merged_acts, n_shadowed = n_shadowed)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/R/rulebook.R stf/harmonization/tests/testthat/test-rulebook.R
git commit -m "feat: layered rulebook loading with precedence and shadow warnings"
```

---

## Task 4: Build STF test fixtures

Later tasks need small studies to test against. Building them by hand keeps unit tests fast
and independent of the real 62 studies.

**Files:**
- Create: `stf/harmonization/tests/testthat/fixtures/tiny/study.yaml`
- Create: `stf/harmonization/tests/testthat/fixtures/tiny/entity-sample.yaml`
- Create: `stf/harmonization/tests/testthat/fixtures/tiny/entity-sample.tsv`
- Test: `stf/harmonization/tests/testthat/test-fixtures.R`

- [ ] **Step 1: Write `study.yaml`**

```yaml
name: tiny

entities:
  - sample
```

- [ ] **Step 2: Write `entity-sample.yaml`**

Mirrors the real pfal3D7 shape: an ID column, a `host` category, and three variables
including the `organism` defect and a `Blood`/`blood` split.

```yaml
name: sample
description: Tiny fixture study for harmonization unit tests.
display_name: Sample
display_name_plural: Samples
stable_id: sample

id_columns:
  - id_column: sample_id
    entity_name: sample
    provider_label:
      - sample_id

variables:
  - variable: organism
    provider_label:
      - organism
    display_name: organism
    definition: organism (OBI_0100026)
    data_type: string
    data_shape: categorical
    parent_variable: host
  - variable: specimen
    provider_label:
      - specimen
    display_name: specimen
    definition: specimen (OBI_0100051)
    data_type: string
    data_shape: categorical
    parent_variable: host
  - variable: country
    provider_label:
      - country
    display_name: country
    definition: country (ENVO_00000009)
    data_type: string
    data_shape: categorical
    parent_variable: host

categories:
  - category: host
    display_name: Host
    data_type: category
    has_values: no

collections: []
```

- [ ] **Step 3: Write `entity-sample.tsv`**

Tab-separated. The first header **must** be `sample_id \\ Descriptors`. Six rows exercising
the case split, the sub-national country pollution, a site code, and a null.

```
sample_id \\ Descriptors	organism	specimen	country
s1	Homo sapiens	Blood	Wang Pha, Thailand
s2	human	blood	Thailand
s3	Human	blood	PLU
s4	Homo sapiens	Blood	Senegal
s5	human	culture	UK
s6		Blood	Senegal
```

- [ ] **Step 4: Write the fixture sanity test**

Write `stf/harmonization/tests/testthat/test-fixtures.R`:

```r
test_that("the tiny fixture loads and validates as STF", {
  s <- study_from_stf(file.path(getwd(), 'fixtures', 'tiny'))
  e <- get_entity(s, 'sample')
  expect_true(validate(quiet(e)))
  expect_equal(nrow(get_data(e)), 6)
  expect_equal(get_entity_id_column(e), 'sample_id')
  expect_true(all(c('organism', 'specimen', 'country') %in% colnames(get_data(e))))
})

test_that("the tiny fixture reproduces the real defects we intend to fix", {
  e <- get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
  d <- get_data(e)
  # three spellings of one host species
  expect_setequal(na.omit(unique(d$organism)), c('Homo sapiens', 'human', 'Human'))
  # a case split in specimen
  expect_true(all(c('Blood', 'blood') %in% d$specimen))
  # STF categoricals arrive as character, not factor (trap 3)
  expect_equal(class(d$organism), 'character')
})
```

- [ ] **Step 5: Run tests**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS. If `study_from_stf` errors, the TSV header is wrong — it must be
`sample_id \\ Descriptors` with a literal double backslash, and the file must be
tab-separated (check with `cat -A`).

- [ ] **Step 6: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/tests/testthat/fixtures stf/harmonization/tests/testthat/test-fixtures.R
git commit -m "test: add tiny STF fixture reproducing target defects"
```

---

## Task 5: Apply value substitutions (with type preservation)

The core of the engine. Substitutes values, preserves the column's original class (trap 3),
skips the ID column and non-categorical variables, and logs every change.

**Files:**
- Create: `stf/harmonization/R/apply_rules.R`
- Test: `stf/harmonization/tests/testthat/test-apply_values.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-apply_values.R`:

```r
tiny_entity <- function() {
  get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
}

tiny_vocab <- function() {
  merge_vocabulary_layers(
    layer0 = tibble(variable = '*', raw_value = c('Homo sapiens', 'human'),
                    canonical_value = 'Homo sapiens',
                    ontology_term_id = 'NCBITaxon_9606', note = NA_character_),
    layer1 = tibble(variable = 'specimen', raw_value = 'Blood',
                    canonical_value = 'blood',
                    ontology_term_id = 'OBI_0000655', note = NA_character_),
    layer2 = empty_vocabulary())
}

test_that("apply_value_rules collapses all spellings to the canonical value", {
  res <- apply_value_rules(tiny_entity(), tiny_vocab())
  d <- get_data(res$entity)
  expect_equal(sum(d$organism == 'Homo sapiens', na.rm = TRUE), 5)
  expect_equal(sum(is.na(d$organism)), 1)
  expect_setequal(na.omit(unique(d$organism)), 'Homo sapiens')
})

test_that("apply_value_rules preserves column class and keeps the entity valid", {
  e <- tiny_entity()
  before <- class(get_data(e)$organism)
  res <- apply_value_rules(e, tiny_vocab())
  expect_equal(class(get_data(res$entity)$organism), before)
  expect_equal(class(get_data(res$entity)$organism), 'character')
  expect_true(validate(quiet(res$entity)))
})

test_that("apply_value_rules logs one row per (variable, raw_value) with counts and layer", {
  res <- apply_value_rules(tiny_entity(), tiny_vocab())
  log <- res$changes
  expect_setequal(colnames(log),
    c('variable', 'raw_value', 'canonical_value', 'rows_affected', 'layer',
      'ontology_term_id'))
  human_row <- log %>% filter(variable == 'organism', raw_value == 'human')
  expect_equal(human_row$rows_affected, 2L)
  expect_equal(human_row$layer, 0L)
  expect_equal(human_row$ontology_term_id, 'NCBITaxon_9606')
  # 'Homo sapiens' -> 'Homo sapiens' is a no-op and must NOT be logged
  expect_equal(nrow(log %>% filter(raw_value == 'Homo sapiens')), 0)
})

test_that("values with no rule pass through untouched", {
  res <- apply_value_rules(tiny_entity(), tiny_vocab())
  d <- get_data(res$entity)
  expect_true('culture' %in% d$specimen)
  expect_true('PLU' %in% d$country)
})

test_that("the ID column is never harmonized", {
  vocab <- merge_vocabulary_layers(
    layer0 = tibble(variable = '*', raw_value = 's1', canonical_value = 'MANGLED',
                    ontology_term_id = NA_character_, note = NA_character_),
    layer1 = empty_vocabulary(), layer2 = empty_vocabulary())
  res <- apply_value_rules(tiny_entity(), vocab)
  expect_true('s1' %in% get_data(res$entity)$sample_id)
  expect_equal(nrow(res$changes), 0)
})

test_that("an empty rulebook is an exact no-op", {
  e <- tiny_entity()
  res <- apply_value_rules(e, merge_vocabulary_layers(
    empty_vocabulary(), empty_vocabulary(), empty_vocabulary()))
  expect_equal(get_data(res$entity), get_data(e))
  expect_equal(nrow(res$changes), 0)
})

test_that("unresolved_values lists every value with no rule, with counts", {
  u <- unresolved_values(tiny_entity(), tiny_vocab())
  expect_setequal(colnames(u), c('variable', 'raw_value', 'rows_affected'))
  expect_true(nrow(u %>% filter(variable == 'country', raw_value == 'PLU')) == 1)
  # 'Blood' has a rule, so it must not appear
  expect_equal(nrow(u %>% filter(variable == 'specimen', raw_value == 'Blood')), 0)
  # ID column excluded
  expect_equal(nrow(u %>% filter(variable == 'sample_id')), 0)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "apply_value_rules"`

- [ ] **Step 3: Write minimal implementation**

Write `stf/harmonization/R/apply_rules.R`:

```r
#' Which variables are eligible for value harmonization?
#'
#' Categorical/string variables only. Never the ID column, never continuous or date
#' variables (substituting into a date column would corrupt it).
harmonizable_variables <- function(entity) {
  id_col <- get_entity_id_column(entity)
  get_variable_metadata(entity) %>%
    filter(as.character(data_shape) == 'categorical',
           as.character(data_type) %in% c('string', 'category'),
           variable != id_col) %>%
    pull(variable) %>%
    intersect(colnames(get_data(entity)))
}

#' Apply vocabulary rules to an entity.
#'
#' @return list(entity = modified Entity, changes = tibble of substitutions)
apply_value_rules <- function(entity, merged_vocab) {
  vars <- harmonizable_variables(entity)
  changes <- list()

  for (v in vars) {
    col <- get_data(entity)[[v]]
    original_class <- class(col)
    present <- unique(na.omit(as.character(col)))
    if (length(present) == 0) next

    # Build raw -> canonical for the values actually present
    mapping <- present %>%
      map(function(raw) {
        hit <- resolve_value(merged_vocab, v, raw)
        if (nrow(hit) == 0) return(NULL)
        if (identical(hit$canonical_value, raw)) return(NULL)   # no-op, do not log
        tibble(variable = v, raw_value = raw,
               canonical_value = hit$canonical_value,
               rows_affected = sum(as.character(col) == raw, na.rm = TRUE),
               layer = hit$layer,
               ontology_term_id = hit$ontology_term_id)
      }) %>%
      compact() %>%
      bind_rows()

    if (nrow(mapping) == 0) next

    lookup <- setNames(mapping$canonical_value, mapping$raw_value)
    new_col <- as.character(col)
    hit_idx <- !is.na(new_col) & new_col %in% names(lookup)
    new_col[hit_idx] <- unname(lookup[new_col[hit_idx]])

    # TRAP 3: restore the original column class, or validate() fails because the
    # column class stops matching the declared data_type.
    if (!identical(original_class, 'character')) {
      new_col <- methods::as(new_col, original_class[1])
    }

    entity <- entity %>% modify_data(mutate(!!rlang::sym(v) := new_col))
    changes[[length(changes) + 1]] <- mapping
  }

  list(
    entity  = entity,
    changes = if (length(changes) == 0) {
      tibble(variable = character(), raw_value = character(),
             canonical_value = character(), rows_affected = integer(),
             layer = integer(), ontology_term_id = character())
    } else {
      bind_rows(changes) %>% mutate(rows_affected = as.integer(rows_affected))
    }
  )
}

#' Every present value that no layer has a rule for. This is the curation work queue.
unresolved_values <- function(entity, merged_vocab) {
  vars <- harmonizable_variables(entity)
  d <- get_data(entity)

  vars %>%
    map(function(v) {
      col <- as.character(d[[v]])
      tibble(variable = v, raw_value = na.omit(col)) %>%
        count(variable, raw_value, name = 'rows_affected') %>%
        filter(map_lgl(raw_value, ~ nrow(resolve_value(merged_vocab, v, .x)) == 0))
    }) %>%
    bind_rows() %>%
    arrange(desc(rows_affected))
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/R/apply_rules.R stf/harmonization/tests/testthat/test-apply_values.R
git commit -m "feat: apply vocabulary rules with column-class preservation"
```

---

## Task 6: Apply variable actions (rename, reparent, drop)

Implements the verified three-step rename recipe (traps 1 and 2) and the pass-one `drop`
guard from spec §4.2: a populated, interpretable variable may not be dropped.

**Files:**
- Modify: `stf/harmonization/R/apply_rules.R` (append)
- Test: `stf/harmonization/tests/testthat/test-apply_variables.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-apply_variables.R`:

```r
tiny_entity2 <- function() {
  get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
}

acts <- function(...) {
  bind_rows(list(...)) %>% mutate(layer = 1L)
}
arow <- function(variable, action, new_name = NA_character_,
                 new_display_name = NA_character_, new_parent = NA_character_,
                 reason = 'test') {
  tibble(variable = variable, action = action, new_name = new_name,
         new_display_name = new_display_name, new_parent = new_parent, reason = reason)
}

test_that("rename changes the column and preserves all metadata", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_name = 'host_organism',
              new_display_name = 'Host organism',
              reason = 'parented to host but named as organism under investigation')))
  e <- res$entity
  md <- get_variable_metadata(e) %>% filter(variable == 'host_organism')

  expect_true('host_organism' %in% colnames(get_data(e)))
  expect_false('organism' %in% colnames(get_data(e)))
  expect_equal(md$display_name, 'Host organism')
  expect_equal(md$definition, 'organism (OBI_0100026)')     # survived (trap 1)
  expect_equal(md$parent_variable, 'host')
  expect_equal(as.character(md$data_type), 'string')
  expect_equal(as.character(md$data_shape), 'categorical')
  expect_true(validate(quiet(e)))
})

test_that("rename preserves the ORIGINAL provider_label as provenance", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_name = 'host_organism', reason = 'x')))
  md <- get_variable_metadata(res$entity) %>% filter(variable == 'host_organism')
  # sync_variable_metadata() would set this to 'host_organism', which is false:
  # the provider called it 'organism'. (trap 2)
  expect_equal(unlist(md$provider_label), 'organism')
})

test_that("a display-name-only action leaves the column name alone", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_display_name = 'Host organism',
              reason = 'label only')))
  e <- res$entity
  expect_true('organism' %in% colnames(get_data(e)))
  expect_equal(get_variable_metadata(e) %>% filter(variable == 'organism') %>%
                 pull(display_name), 'Host organism')
  expect_true(validate(quiet(e)))
})

test_that("reparent moves a variable to another existing category", {
  e0 <- tiny_entity2()
  res <- apply_variable_actions(e0,
    acts(arow('country', 'reparent', new_parent = 'host',
              reason = 'fixture only has one category')))
  expect_equal(get_variable_metadata(res$entity) %>% filter(variable == 'country') %>%
                 pull(parent_variable), 'host')
  expect_true(validate(quiet(res$entity)))
})

test_that("reparent to a non-existent category is a hard error", {
  expect_error(
    apply_variable_actions(tiny_entity2(),
      acts(arow('country', 'reparent', new_parent = 'nonexistent_category',
                reason = 'x'))),
    'nonexistent_category')
})

test_that("dropping a populated interpretable variable is refused (spec 4.2)", {
  expect_error(
    apply_variable_actions(tiny_entity2(),
      acts(arow('country', 'drop', reason = 'I just do not like it'))),
    'populated')
})

test_that("dropping an all-null variable is allowed", {
  e <- tiny_entity2() %>% modify_data(mutate(country = NA_character_))
  res <- apply_variable_actions(e, acts(arow('country', 'drop', reason = 'all null')))
  expect_false('country' %in% colnames(get_data(res$entity)))
  expect_equal(nrow(get_variable_metadata(res$entity) %>% filter(variable == 'country')), 0)
  expect_true(validate(quiet(res$entity)))
})

test_that("dropping a populated variable on the artifact allowlist is allowed", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('country', 'drop', reason = 'header-parsing artifact')),
    artifact_allowlist = c('country'))
  expect_false('country' %in% colnames(get_data(res$entity)))
})

test_that("an action for a variable absent from this study is a no-op, and is counted", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('fungal_strain', 'drop', reason = 'not in this study')))
  expect_equal(res$not_applicable, 1L)
  expect_equal(nrow(res$applied), 0)
  expect_true(validate(quiet(res$entity)))
})

test_that("applied actions are logged with their reason", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_name = 'host_organism',
              new_display_name = 'Host organism', reason = 'because spec')))
  expect_setequal(colnames(res$applied),
    c('variable', 'action', 'new_name', 'new_display_name', 'new_parent', 'reason', 'layer'))
  expect_equal(res$applied$reason, 'because spec')
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "apply_variable_actions"`

- [ ] **Step 3: Append the implementation to `apply_rules.R`**

Add to the end of `stf/harmonization/R/apply_rules.R`:

```r
#' Rename a variable: column + metadata, preserving provenance.
#'
#' Three steps are mandatory. modify_data(rename()) alone leaves the entity invalid
#' (metadata refers to a column that no longer exists); sync_variable_metadata() fixes
#' validity but WIPES display_name/definition/parent_variable and rewrites provider_label
#' to the new name. So: capture first, rename, sync, re-apply.
rename_variable <- function(entity, old_name, new_name, new_display_name = NA_character_) {
  old <- get_variable_metadata(entity) %>% filter(variable == old_name)
  if (nrow(old) != 1) stop(glue::glue("Expected exactly one metadata row for '{old_name}'"))

  display <- if (!is.na(new_display_name)) new_display_name else old$display_name

  entity %>%
    modify_data(rename(!!rlang::sym(new_name) := !!rlang::sym(old_name))) %>%
    sync_variable_metadata() %>%
    set_variable_metadata(new_name,
      display_name    = display,
      definition      = old$definition,
      parent_variable = old$parent_variable,
      data_type       = as.character(old$data_type),
      data_shape      = as.character(old$data_shape),
      # provider_label is PROVENANCE: keep what the provider called it, not the new name.
      provider_label  = as.list(unlist(old$provider_label)))
}

#' Apply variable rename/reparent/drop actions.
#'
#' @param artifact_allowlist variables that may be dropped despite being populated,
#'        because they are header-parsing artifacts with no interpretable meaning.
#' @return list(entity, applied = tibble of actions applied, not_applicable = integer)
apply_variable_actions <- function(entity, actions, artifact_allowlist = character()) {
  applied <- list()
  not_applicable <- 0L

  if (nrow(actions) == 0) {
    return(list(entity = entity, applied = empty_applied_actions(), not_applicable = 0L))
  }

  categories <- get_category_metadata(entity)$variable

  for (i in seq_len(nrow(actions))) {
    a <- actions[i, ]
    if (!(a$variable %in% colnames(get_data(entity)))) {
      not_applicable <- not_applicable + 1L
      next
    }

    if (a$action == 'drop') {
      col <- get_data(entity)[[a$variable]]
      n_populated <- sum(!is.na(col))
      if (n_populated > 0 && !(a$variable %in% artifact_allowlist)) {
        stop(glue::glue(
          "Refusing to drop '{a$variable}': it is populated ({n_populated} non-null ",
          "rows) and is not on the header-artifact allowlist. Dropping a populated, ",
          "interpretable variable is a structural change (spec section 4.2) and belongs ",
          "in pass two. It is reported in structural_findings.md instead."))
      }
      entity <- entity %>%
        modify_data(select(-!!rlang::sym(a$variable))) %>%
        sync_variable_metadata()

    } else if (a$action == 'reparent') {
      if (is.na(a$new_parent)) {
        stop(glue::glue("Action 'reparent' for '{a$variable}' has no new_parent"))
      }
      if (!(a$new_parent %in% categories)) {
        stop(glue::glue(
          "Cannot reparent '{a$variable}' to '{a$new_parent}': no such category in this ",
          "entity. Existing categories: {toString(categories)}"))
      }
      entity <- entity %>%
        set_variable_metadata(a$variable, parent_variable = a$new_parent)

    } else if (a$action == 'rename') {
      if (!is.na(a$new_name)) {
        entity <- rename_variable(entity, a$variable, a$new_name, a$new_display_name)
      } else if (!is.na(a$new_display_name)) {
        entity <- entity %>%
          set_variable_metadata(a$variable, display_name = a$new_display_name)
      } else {
        stop(glue::glue(
          "Action 'rename' for '{a$variable}' supplies neither new_name nor ",
          "new_display_name"))
      }
    }

    applied[[length(applied) + 1]] <- a
  }

  list(
    entity = entity,
    applied = if (length(applied) == 0) empty_applied_actions() else bind_rows(applied),
    not_applicable = not_applicable
  )
}

empty_applied_actions <- function() {
  tibble(variable = character(), action = character(), new_name = character(),
         new_display_name = character(), new_parent = character(),
         reason = character(), layer = integer())
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/R/apply_rules.R \
        stf/harmonization/tests/testthat/test-apply_variables.R
git commit -m "feat: variable rename/reparent/drop with provenance and drop guard"
```

---

## Task 7: Structural findings report (read-only)

Detects the spec §5 item 4 defects that pass one deliberately does not fix. Changes
nothing. Its output is what pass two gets designed from.

**Files:**
- Create: `stf/harmonization/R/structural_report.R`
- Test: `stf/harmonization/tests/testthat/test-structural_report.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-structural_report.R`:

```r
tiny_entity3 <- function() {
  get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
}

test_that("country pollution is flagged for comma values and short uppercase codes", {
  f <- structural_findings(tiny_entity3())
  cp <- f %>% filter(check == 'country_pollution')
  expect_true('Wang Pha, Thailand' %in% cp$value)   # comma => sub-national leak
  expect_true('PLU' %in% cp$value)                  # short all-caps => site code
  expect_false('Senegal' %in% cp$value)             # clean country
  expect_false('Thailand' %in% cp$value)
})

test_that("a clean country column produces no country findings", {
  e <- tiny_entity3() %>% modify_data(mutate(country = 'Senegal'))
  f <- structural_findings(e)
  expect_equal(nrow(f %>% filter(check == 'country_pollution')), 0)
})

test_that("identifier-like variables are flagged", {
  # 6 distinct values over 6 rows = 100% distinct
  e <- tiny_entity3() %>%
    modify_data(mutate(country = c('a', 'b', 'c', 'd', 'e', 'f')))
  f <- structural_findings(e)
  expect_true('country' %in% (f %>% filter(check == 'identifier_like') %>% pull(variable)))
})

test_that("all-null variables are flagged", {
  e <- tiny_entity3() %>% modify_data(mutate(country = NA_character_))
  f <- structural_findings(e)
  expect_true('country' %in% (f %>% filter(check == 'all_null') %>% pull(variable)))
})

test_that("age unit variants are flagged when an age variable exists", {
  f <- structural_findings_for_values(
    tibble(variable = 'age', value = c('29 years', '28 year', '4 weeks', '30')))
  au <- f %>% filter(check == 'age_units')
  expect_setequal(au$value, c('29 years', '28 year', '4 weeks', '30'))
})

test_that("date ranges and datetimes in collection_date_original are flagged", {
  f <- structural_findings_for_values(
    tibble(variable = 'collection_date_original',
           value = c('2008', '1999-2002', '2014-06-23 00:00:00')))
  dr <- f %>% filter(check == 'date_original_not_a_date')
  expect_setequal(dr$value, c('1999-2002', '2014-06-23 00:00:00'))
})

test_that("the uninformative OBI parent class value is flagged in specimen", {
  f <- structural_findings_for_values(
    tibble(variable = 'specimen', value = c('blood', 'specimen from organism')))
  expect_equal(f %>% filter(check == 'uninformative_value') %>% pull(value),
               'specimen from organism')
})

test_that("structural_findings has a stable schema", {
  f <- structural_findings(tiny_entity3())
  expect_setequal(colnames(f),
    c('check', 'variable', 'value', 'rows_affected', 'detail'))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "structural_findings"`

- [ ] **Step 3: Write minimal implementation**

Write `stf/harmonization/R/structural_report.R`:

```r
# Values that are the bare ontology parent class, carrying no information.
UNINFORMATIVE_VALUES <- c('specimen from organism', 'sample from organism',
                          'whole organism', 'organism')

empty_findings <- function() {
  tibble(check = character(), variable = character(), value = character(),
         rows_affected = integer(), detail = character())
}

#' Value-level structural checks, separated so they are testable without an Entity.
#'
#' @param vals tibble(variable, value) — one row per occurrence or per distinct value
structural_findings_for_values <- function(vals) {
  if (nrow(vals) == 0) return(empty_findings())

  counted <- vals %>% filter(!is.na(value)) %>% count(variable, value, name = 'rows_affected')

  country <- counted %>%
    filter(variable %in% c('country'),
           grepl(',', value) | (nchar(value) <= 4 & value == toupper(value) &
                                grepl('^[A-Z]+$', value))) %>%
    mutate(check = 'country_pollution',
           detail = if_else(grepl(',', value),
                            'contains a comma: sub-national place merged into country',
                            'short all-caps token: looks like a collection-site code'))

  age <- counted %>%
    filter(variable == 'age') %>%
    mutate(check = 'age_units',
           detail = case_when(
             grepl('week', value, ignore.case = TRUE) ~ 'unit: weeks',
             grepl('year', value, ignore.case = TRUE) ~ 'unit: years',
             TRUE                                     ~ 'no unit token'))

  dates <- counted %>%
    filter(variable == 'collection_date_original',
           grepl('^[0-9]{4}\\s*-\\s*[0-9]{4}$', value) | grepl('[0-9]{2}:[0-9]{2}', value)) %>%
    mutate(check = 'date_original_not_a_date',
           detail = if_else(grepl(':', value), 'datetime, not a date', 'year range'))

  uninformative <- counted %>%
    filter(value %in% UNINFORMATIVE_VALUES) %>%
    mutate(check = 'uninformative_value',
           detail = 'bare ontology parent class; carries no information')

  bind_rows(country, age, dates, uninformative) %>%
    select(all_of(c('check', 'variable', 'value', 'rows_affected', 'detail')))
}

#' All structural findings for one entity. Read-only: changes nothing.
structural_findings <- function(entity) {
  d <- get_data(entity)
  id_col <- get_entity_id_column(entity)
  vars <- setdiff(colnames(d), id_col)
  if (length(vars) == 0) return(empty_findings())

  vals <- vars %>%
    map(~ tibble(variable = .x, value = as.character(d[[.x]]))) %>%
    bind_rows()

  value_level <- structural_findings_for_values(vals)

  # variable-level checks
  var_level <- vars %>%
    map(function(v) {
      col <- d[[v]]
      n_nonnull <- sum(!is.na(col))
      n_distinct <- length(unique(na.omit(as.character(col))))
      if (n_nonnull == 0) {
        return(tibble(check = 'all_null', variable = v, value = NA_character_,
                      rows_affected = 0L,
                      detail = 'variable is 100% null; carries no information'))
      }
      if (n_distinct >= 0.95 * n_nonnull && n_nonnull > 1) {
        return(tibble(check = 'identifier_like', variable = v, value = NA_character_,
                      rows_affected = as.integer(n_nonnull),
                      detail = glue::glue(
                        '{n_distinct} distinct values over {n_nonnull} non-null rows ',
                        '(>=95%): identifier-like, useless as an EDA facet')))
      }
      NULL
    }) %>%
    compact() %>%
    bind_rows()

  # continent coverage
  continent <- if ('continent' %in% vars) {
    n_null <- sum(is.na(d$continent))
    if (n_null > 0) {
      tibble(check = 'continent_incomplete', variable = 'continent', value = NA_character_,
             rows_affected = as.integer(n_null),
             detail = glue::glue('{n_null}/{nrow(d)} rows null; continent is derivable ',
                                 'from a clean country and should be computed'))
    } else NULL
  } else NULL

  bind_rows(value_level, var_level, continent) %>%
    select(all_of(c('check', 'variable', 'value', 'rows_affected', 'detail')))
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/R/structural_report.R \
        stf/harmonization/tests/testthat/test-structural_report.R
git commit -m "feat: structural findings report for deferred defects"
```

---

## Task 8: Report writing

**Files:**
- Create: `stf/harmonization/R/report.R`
- Test: `stf/harmonization/tests/testthat/test-report.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-report.R`:

```r
test_that("write_study_reports writes every expected file", {
  out <- file.path(tempdir(), 'rep', 'PlasmoDB', 'tiny')
  changes <- tibble(variable = 'organism', raw_value = 'human',
                    canonical_value = 'Homo sapiens', rows_affected = 2L,
                    layer = 0L, ontology_term_id = 'NCBITaxon_9606')
  write_study_reports(
    out_dir = out,
    changes = changes,
    applied_actions = tibble(variable = 'organism', action = 'rename',
                             new_name = 'host_organism',
                             new_display_name = 'Host organism',
                             new_parent = NA_character_, reason = 'spec', layer = 1L),
    findings = tibble(check = 'country_pollution', variable = 'country',
                      value = 'Wang Pha, Thailand', rows_affected = 1L,
                      detail = 'contains a comma'),
    unresolved = tibble(variable = 'country', raw_value = 'PLU', rows_affected = 1L))

  expect_true(file.exists(file.path(out, 'changes.tsv')))
  expect_true(file.exists(file.path(out, 'variable_actions.tsv')))
  expect_true(file.exists(file.path(out, 'structural_findings.md')))
  expect_true(file.exists(file.path(out, 'unresolved_values.tsv')))

  back <- readr::read_tsv(file.path(out, 'changes.tsv'), show_col_types = FALSE)
  expect_equal(back$canonical_value, 'Homo sapiens')
  expect_equal(back$rows_affected, 2)

  md <- readLines(file.path(out, 'structural_findings.md'))
  expect_true(any(grepl('country_pollution', md)))
  expect_true(any(grepl('Wang Pha, Thailand', md, fixed = TRUE)))
})

test_that("write_study_reports handles empty inputs without erroring", {
  out <- file.path(tempdir(), 'rep_empty')
  write_study_reports(out, tibble(variable = character(), raw_value = character(),
                                  canonical_value = character(),
                                  rows_affected = integer(), layer = integer(),
                                  ontology_term_id = character()),
                      empty_applied_actions(), empty_findings(),
                      tibble(variable = character(), raw_value = character(),
                             rows_affected = integer()))
  expect_true(file.exists(file.path(out, 'changes.tsv')))
  md <- readLines(file.path(out, 'structural_findings.md'))
  expect_true(any(grepl('No structural findings', md)))
})

test_that("capture_inspect writes a non-empty text snapshot", {
  e <- get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
  p <- file.path(tempdir(), 'inspect_before.txt')
  capture_inspect(e, p)
  expect_true(file.exists(p))
  expect_gt(length(readLines(p)), 3)
})

test_that("write_summary aggregates one row per study", {
  p <- file.path(tempdir(), 'summary.tsv')
  write_summary(list(
    tibble(project = 'PlasmoDB', organism = 'pfal3D7', status = 'ok', rows = 537L,
           variables = 22L, substitutions = 2L, rows_substituted = 521L,
           variables_renamed = 1L, variables_dropped = 0L, unresolved = 400L,
           shadowed_rules = 0L, message = NA_character_)), p)
  back <- readr::read_tsv(p, show_col_types = FALSE)
  expect_equal(nrow(back), 1)
  expect_equal(back$organism, 'pfal3D7')
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "write_study_reports"`

- [ ] **Step 3: Write minimal implementation**

Write `stf/harmonization/R/report.R`:

```r
#' Capture inspect() output to a text file for before/after diffing.
capture_inspect <- function(entity, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- capture.output(inspect(entity))
  writeLines(lines, path)
  invisible(path)
}

#' Render structural findings as reviewable markdown.
render_findings_md <- function(findings) {
  if (nrow(findings) == 0) {
    return(c('# Structural findings', '',
             'No structural findings for this study.'))
  }
  header <- c('# Structural findings', '',
              paste0('Deferred defects (spec section 5, item 4). These are **reported, ',
                     'not fixed** — they change the entity schema and belong to pass two.'),
              '')
  body <- findings %>%
    group_split(check) %>%
    map(function(grp) {
      c(glue::glue('## {grp$check[1]}'), '',
        glue::glue('{nrow(grp)} finding(s), {sum(grp$rows_affected, na.rm = TRUE)} rows affected.'),
        '',
        '| variable | value | rows | detail |',
        '|---|---|---|---|',
        glue::glue('| {grp$variable} | {ifelse(is.na(grp$value), "—", grp$value)} | ',
                   '{grp$rows_affected} | {grp$detail} |'),
        '')
    }) %>%
    unlist()
  c(header, body)
}

#' Write all per-study reports.
write_study_reports <- function(out_dir, changes, applied_actions, findings, unresolved) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(changes,         file.path(out_dir, 'changes.tsv'), na = '')
  readr::write_tsv(applied_actions, file.path(out_dir, 'variable_actions.tsv'), na = '')
  readr::write_tsv(unresolved,      file.path(out_dir, 'unresolved_values.tsv'), na = '')
  writeLines(render_findings_md(findings), file.path(out_dir, 'structural_findings.md'))
  invisible(out_dir)
}

#' Write the repo-level summary across all studies.
write_summary <- function(rows, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(bind_rows(rows), path, na = '')
  invisible(path)
}

#' Write the aggregated cross-study curation work queue.
write_unresolved_summary <- function(unresolved_list, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  bind_rows(unresolved_list) %>%
    group_by(variable, raw_value) %>%
    summarise(rows_affected = sum(rows_affected), n_studies = n(), .groups = 'drop') %>%
    arrange(desc(rows_affected)) %>%
    readr::write_tsv(path, na = '')
  invisible(path)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/R/report.R stf/harmonization/tests/testthat/test-report.R
git commit -m "feat: per-study and cross-study report writing"
```

---

## Task 9: CLI orchestration

Wires everything together. Implements the spec §6 error table: one bad study must not abort
a 62-study run, and invalid STF is never emitted.

**Files:**
- Create: `stf/harmonization/harmonize_stf.R`
- Test: `stf/harmonization/tests/testthat/test-orchestrate.R`

- [ ] **Step 1: Write the failing test**

Write `stf/harmonization/tests/testthat/test-orchestrate.R`:

```r
test_that("harmonize_one_study produces valid STF and a change log", {
  rules <- file.path(tempdir(), 'rules_ok')
  dir.create(file.path(rules, 'global'), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(rules, 'generated'), recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(
    tibble(variable = '*', raw_value = c('human', 'Human'),
           canonical_value = 'Homo sapiens', ontology_term_id = 'NCBITaxon_9606',
           note = NA_character_),
    file.path(rules, 'generated', 'vocabulary_from_corpus.tsv'), na = '')
  readr::write_tsv(
    tibble(variable = 'organism', action = 'rename', new_name = 'host_organism',
           new_display_name = 'Host organism', new_parent = NA_character_,
           reason = 'parented to host but named as organism under investigation'),
    file.path(rules, 'global', 'variables.tsv'), na = '')

  out_stf <- file.path(tempdir(), 'out_stf')
  out_rep <- file.path(tempdir(), 'out_rep')

  res <- harmonize_one_study(
    study_dir = file.path(getwd(), 'fixtures', 'tiny'),
    project = 'TestDB', organism = 'tiny',
    rules_dir = rules, out_stf_root = out_stf, out_report_root = out_rep)

  expect_equal(res$status, 'ok')

  # output STF loads and validates
  reloaded <- study_from_stf(file.path(out_stf, 'TestDB', 'tiny'))
  e <- get_entity(reloaded, 'sample')
  expect_true(validate(quiet(e)))
  expect_true('host_organism' %in% colnames(get_data(e)))
  expect_setequal(na.omit(unique(get_data(e)$host_organism)), 'Homo sapiens')

  # reports written
  expect_true(file.exists(file.path(out_rep, 'TestDB', 'tiny', 'changes.tsv')))
  expect_true(file.exists(file.path(out_rep, 'TestDB', 'tiny', 'inspect_before.txt')))
  expect_true(file.exists(file.path(out_rep, 'TestDB', 'tiny', 'inspect_after.txt')))
})

test_that("an empty rulebook round-trips the study unchanged", {
  rules <- file.path(tempdir(), 'rules_empty')
  dir.create(rules, recursive = TRUE, showWarnings = FALSE)
  out_stf <- file.path(tempdir(), 'out_noop')

  res <- harmonize_one_study(
    study_dir = file.path(getwd(), 'fixtures', 'tiny'),
    project = 'TestDB', organism = 'tiny', rules_dir = rules,
    out_stf_root = out_stf, out_report_root = file.path(tempdir(), 'rep_noop'))

  expect_equal(res$status, 'ok')
  expect_equal(res$substitutions, 0L)
  original <- get_data(get_entity(study_from_stf(
    file.path(getwd(), 'fixtures', 'tiny')), 'sample'))
  produced <- get_data(get_entity(study_from_stf(
    file.path(out_stf, 'TestDB', 'tiny')), 'sample'))
  expect_equal(produced, original)
})

test_that("inputs are never modified", {
  src <- file.path(getwd(), 'fixtures', 'tiny')
  before <- tools::md5sum(list.files(src, full.names = TRUE))
  rules <- file.path(tempdir(), 'rules_ro'); dir.create(rules, showWarnings = FALSE)
  harmonize_one_study(src, 'TestDB', 'tiny', rules,
                      file.path(tempdir(), 'o1'), file.path(tempdir(), 'o2'))
  expect_equal(tools::md5sum(list.files(src, full.names = TRUE)), before)
})

test_that("a study that fails to load is recorded, not fatal", {
  bad <- file.path(tempdir(), 'bad_study')
  dir.create(bad, showWarnings = FALSE)
  writeLines('this is not stf', file.path(bad, 'study.yaml'))
  rules <- file.path(tempdir(), 'rules_bad'); dir.create(rules, showWarnings = FALSE)

  res <- harmonize_one_study(bad, 'TestDB', 'bad', rules,
                             file.path(tempdir(), 'o3'), file.path(tempdir(), 'o4'))
  expect_equal(res$status, 'failed')
  expect_true(!is.na(res$message))
})

test_that("discover_studies finds project/organism pairs", {
  root <- file.path(tempdir(), 'studies_root')
  dir.create(file.path(root, 'PlasmoDB', 'pfal3D7'), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, 'FungiDB', 'afumAf293'), recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(root, 'PlasmoDB', 'pfal3D7', 'study.yaml'))
  file.create(file.path(root, 'FungiDB', 'afumAf293', 'study.yaml'))

  found <- discover_studies(root, NULL)
  expect_equal(nrow(found), 2)
  expect_setequal(found$organism, c('pfal3D7', 'afumAf293'))

  one <- discover_studies(root, 'PlasmoDB/pfal3D7')
  expect_equal(nrow(one), 1)
  expect_equal(one$organism, 'pfal3D7')
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: FAIL — `could not find function "harmonize_one_study"`

- [ ] **Step 3: Write minimal implementation**

Write `stf/harmonization/harmonize_stf.R`:

```r
#!/usr/bin/env Rscript
## Harmonize STF studies: collapse inconsistent categorical values and fix variable
## naming, driven entirely by declarative TSV rulebooks.
##
## Inputs are NEVER modified. See docs/superpowers/specs/2026-08-07-stf-harmonization-design.md
##
## usage:  Rscript stf/harmonization/harmonize_stf.R [Project/organism ...]
##         (no args = all studies)

HARMONIZE_MAIN <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  only <- if (length(args) > 0) args else NULL

  studies <- discover_studies(STUDIES_ROOT, only)
  if (nrow(studies) == 0) stop('No studies found to harmonize.')

  message(glue::glue('Harmonizing {nrow(studies)} study/studies...'))

  results <- list(); unresolved_all <- list()
  for (i in seq_len(nrow(studies))) {
    s <- studies[i, ]
    message(glue::glue('[{i}/{nrow(studies)}] {s$project}/{s$organism}'))
    r <- harmonize_one_study(s$dir, s$project, s$organism, RULES_DIR,
                             OUT_STF_ROOT, OUT_REPORT_ROOT)
    results[[length(results) + 1]] <- r$summary_row
    if (!is.null(r$unresolved)) unresolved_all[[length(unresolved_all) + 1]] <- r$unresolved
  }

  write_summary(results, file.path(OUT_REPORT_ROOT, 'harmonization_summary.tsv'))
  if (length(unresolved_all) > 0) {
    write_unresolved_summary(unresolved_all,
                             file.path(OUT_REPORT_ROOT, 'unresolved_values.tsv'))
  }

  summary_tbl <- bind_rows(results)
  n_failed <- sum(summary_tbl$status == 'failed')
  message(glue::glue('\nDone. ok={sum(summary_tbl$status == "ok")} failed={n_failed}'))
  message(glue::glue('Reports: {OUT_REPORT_ROOT}'))
  if (n_failed > 0) {
    message('Failed studies:')
    summary_tbl %>% filter(status == 'failed') %>%
      pwalk(function(project, organism, message, ...)
        base::message(glue::glue('  {project}/{organism}: {message}')))
    quit(status = 1)
  }
}

#' Find project/organism study directories.
#'
#' @param only NULL for all, or a character vector of "Project/organism" selectors.
discover_studies <- function(studies_root, only = NULL) {
  yamls <- list.files(studies_root, pattern = '^study\\.yaml$',
                      recursive = TRUE, full.names = TRUE)
  if (length(yamls) == 0) {
    return(tibble(project = character(), organism = character(), dir = character()))
  }
  out <- tibble(dir = dirname(yamls)) %>%
    mutate(organism = basename(dir), project = basename(dirname(dir))) %>%
    select(project, organism, dir) %>%
    arrange(project, organism)
  if (!is.null(only)) {
    keep_keys <- only
    out <- out %>% filter(paste(project, organism, sep = '/') %in% keep_keys |
                          organism %in% keep_keys)
  }
  out
}

#' Harmonize exactly one study directory.
#'
#' Never throws for per-study problems: returns status 'failed' with a message so a
#' 62-study run continues (spec section 6).
harmonize_one_study <- function(study_dir, project, organism, rules_dir,
                                out_stf_root, out_report_root,
                                artifact_allowlist = ARTIFACT_ALLOWLIST) {
  report_dir <- file.path(out_report_root, project, organism)
  stf_dir    <- file.path(out_stf_root, project, organism)

  fail <- function(msg) {
    list(status = 'failed', substitutions = 0L, unresolved = NULL,
         summary_row = tibble(project = project, organism = organism, status = 'failed',
                              rows = NA_integer_, variables = NA_integer_,
                              substitutions = NA_integer_, rows_substituted = NA_integer_,
                              variables_renamed = NA_integer_,
                              variables_dropped = NA_integer_, unresolved = NA_integer_,
                              shadowed_rules = NA_integer_, message = msg))
  }

  study <- tryCatch(study_from_stf(study_dir), error = function(e) e)
  if (inherits(study, 'error')) return(fail(paste('load failed:', conditionMessage(study))))

  rb <- tryCatch(load_rulebook(rules_dir, project, organism), error = function(e) e)
  if (inherits(rb, 'error')) return(fail(paste('rulebook error:', conditionMessage(rb))))

  entities <- get_entities(study)
  all_changes <- list(); all_applied <- list()
  all_findings <- list(); all_unresolved <- list()
  new_entities <- list()

  for (e in entities) {
    ename <- get_entity_name(e)
    capture_inspect(e, file.path(report_dir, glue::glue('inspect_before.txt')))

    all_findings[[length(all_findings) + 1]]   <- structural_findings(e)
    all_unresolved[[length(all_unresolved) + 1]] <- unresolved_values(e, rb$vocabulary) %>%
      mutate(entity = ename)

    vres <- tryCatch(apply_value_rules(e, rb$vocabulary), error = function(x) x)
    if (inherits(vres, 'error')) {
      return(fail(paste('value rules failed on', ename, ':', conditionMessage(vres))))
    }
    ares <- tryCatch(
      apply_variable_actions(vres$entity, rb$actions, artifact_allowlist),
      error = function(x) x)
    if (inherits(ares, 'error')) {
      return(fail(paste('variable actions failed on', ename, ':', conditionMessage(ares))))
    }

    all_changes[[length(all_changes) + 1]] <- vres$changes %>% mutate(entity = ename)
    all_applied[[length(all_applied) + 1]] <- ares$applied %>% mutate(entity = ename)

    if (!validate(quiet(ares$entity))) {
      validate(verbose(ares$entity))
      return(fail(glue::glue('harmonized entity {ename} failed validation; nothing written')))
    }
    capture_inspect(ares$entity, file.path(report_dir, 'inspect_after.txt'))
    new_entities[[length(new_entities) + 1]] <- ares$entity
  }

  changes    <- bind_rows(all_changes)
  applied    <- bind_rows(all_applied)
  findings   <- bind_rows(all_findings)
  unresolved <- bind_rows(all_unresolved)

  new_study <- tryCatch(
    study_from_entities(new_entities, name = get_study_name(study) %||% organism),
    error = function(x) x)
  if (inherits(new_study, 'error')) {
    return(fail(paste('study assembly failed:', conditionMessage(new_study))))
  }

  dir.create(stf_dir, recursive = TRUE, showWarnings = FALSE)
  exported <- tryCatch({ export_to_stf(new_study, stf_dir); TRUE },
                       error = function(x) x)
  if (inherits(exported, 'error')) {
    return(fail(paste('export failed:', conditionMessage(exported))))
  }

  write_study_reports(report_dir, changes, applied, findings,
                      unresolved %>% select(variable, raw_value, rows_affected))

  root <- get_root_entity(study)
  list(
    status = 'ok',
    substitutions = nrow(changes),
    unresolved = unresolved %>% select(variable, raw_value, rows_affected),
    summary_row = tibble(
      project = project, organism = organism, status = 'ok',
      rows = as.integer(nrow(get_data(root))),
      variables = as.integer(nrow(get_variable_metadata(root))),
      substitutions = as.integer(nrow(changes)),
      rows_substituted = as.integer(sum(changes$rows_affected %||% 0L)),
      variables_renamed = as.integer(sum(applied$action == 'rename')),
      variables_dropped = as.integer(sum(applied$action == 'drop')),
      unresolved = as.integer(nrow(unresolved)),
      shadowed_rules = as.integer(rb$n_shadowed),
      message = NA_character_)
  )
}
```

- [ ] **Step 4: Create the CLI bootstrap**

`harmonize_stf.R` above defines functions but nothing loads dependencies or runs `main`
(the test helper sources `R/*.R` itself, so the bootstrap must not run on source). Prepend
this to the top of `stf/harmonization/harmonize_stf.R`, above `HARMONIZE_MAIN`:

```r
if (!exists('HARMONIZATION_BOOTSTRAPPED')) {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(glue)
  })
  .this_file <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))
  HARMONIZATION_DIR <- if (length(.this_file) > 0) {
    dirname(normalizePath(.this_file))
  } else {
    normalizePath('stf/harmonization')
  }
  REPO_ROOT <- normalizePath(file.path(HARMONIZATION_DIR, '..', '..'))
  suppressPackageStartupMessages(
    devtools::load_all(file.path(REPO_ROOT, 'study-wrangler'), quiet = TRUE))
  for (f in list.files(file.path(HARMONIZATION_DIR, 'R'), pattern = '\\.R$',
                       full.names = TRUE)) source(f)
  HARMONIZATION_BOOTSTRAPPED <- TRUE
}

STUDIES_ROOT    <- file.path(REPO_ROOT, 'stf', 'studies')
RULES_DIR       <- file.path(HARMONIZATION_DIR, 'rules')
OUT_STF_ROOT    <- file.path(REPO_ROOT, 'stf', 'studies_harmonized')
OUT_REPORT_ROOT <- file.path(REPO_ROOT, 'stf', 'reports')

# Populated, interpretable variables may NOT be dropped in pass one (spec 4.2).
# These are header-parsing artifacts with no interpretable meaning, so they may.
ARTIFACT_ALLOWLIST <- c('and', 'group')
```

Then append the runner as the **last line** of the file:

```r
if (sys.nframe() == 0 && length(grep('^--file=.*harmonize_stf[.]R$', commandArgs())) > 0) {
  HARMONIZE_MAIN()
}
```

- [ ] **Step 5: Make the test helper source the CLI file too**

The orchestration functions live in `harmonize_stf.R`, not `R/`. Modify
`stf/harmonization/tests/testthat/helper-setup.R` — replace the sourcing loop at the end
with:

```r
# Source every engine file, then the CLI (which defines the orchestration functions).
for (f in list.files(file.path(HARMONIZATION_DIR, 'R'), pattern = '\\.R$', full.names = TRUE)) {
  source(f)
}
HARMONIZATION_BOOTSTRAPPED <- TRUE   # suppress the CLI's own bootstrap block
source(file.path(HARMONIZATION_DIR, 'harmonize_stf.R'))
```

- [ ] **Step 6: Run tests**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS, all test files

- [ ] **Step 7: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/harmonize_stf.R \
        stf/harmonization/tests/testthat/helper-setup.R \
        stf/harmonization/tests/testthat/test-orchestrate.R
git commit -m "feat: CLI orchestration with per-study error isolation"
```

---

## Task 10: Author the initial rulebooks

Layer 0 is generated. This task writes the hand-authored layers 1 and 2 for the defects
already identified in the spec.

**Files:**
- Create: `stf/harmonization/rules/global/vocabulary.tsv`
- Create: `stf/harmonization/rules/global/variables.tsv`
- Create: `stf/harmonization/rules/studies/PlasmoDB/pfal3D7/vocabulary.tsv`

- [ ] **Step 1: Write `rules/global/variables.tsv`**

Tab-separated, with this exact header. Only these three actions in pass one — `drop` is
restricted to header-parsing artifacts (spec §4.2), and `and`/`group` are on
`ARTIFACT_ALLOWLIST`.

```
variable	action	new_name	new_display_name	new_parent	reason
organism	rename	host_organism	Host organism	host	Parented to host but named as the organism under investigation; renamed to say what it is
and	drop				Header-parsing artifact, not a variable
group	drop				Header-parsing artifact, uninterpretable
```

- [ ] **Step 2: Write `rules/global/vocabulary.tsv`**

Values the corpus does not cover. `ontology_term_id` is left blank where no term has been
curated yet — that blank is the pass-three work queue, not an error.

```
variable	raw_value	canonical_value	ontology_term_id	note
parasite_organism	Plasmodium falciparum		Species-only canonical form; isolate suffixes collapse to this
specimen	Cerebrospinal fluid	cerebrospinal fluid		Case normalization
specimen	CSF	cerebrospinal fluid		Abbreviation expanded to the full label
specimen	cerebrospinal fluid	cerebrospinal fluid		Canonical
specimen	mycellium	mycelium		Misspelling
specimen	mycelia	mycelium		Plural to singular
specimen	Clinical	clinical		Case normalization
specimen	Environmental	environmental		Case normalization
specimen	Veterinary	veterinary		Case normalization
health_status	cryptocococcal meningitis	Cryptococcal meningitis		Misspelling
health_status	Cryptococcal meningitis	Cryptococcal meningitis		Canonical
health_status	asymptomatic	Asymptomatic		Case normalization
health_status	Asymptomatic	Asymptomatic		Canonical
fungus_organism	[Candida] auris	Candida auris		NCBI bracket notation stripped
host_common_name	Cow	Bos taurus		Common name to binomial
host_common_name	Calf	Bos taurus		Common name to binomial
```

**Note on `parasite_organism`:** the row above only canonicalizes the exact string. The
180 isolate-suffixed values (`Plasmodium falciparum NHP1523`, …) each need their own row,
which is unreasonable to hand-write. Step 3 generates them.

- [ ] **Step 3: Generate the species-collapse rows for `parasite_organism`**

Run this to append one rule per isolate-suffixed value found across all studies:

```bash
cd /home/jbrestel/dataCleaning
Rscript -e "
suppressPackageStartupMessages(library(tidyverse))
tsvs <- list.files('stf/studies', pattern='^entity-.*[.]tsv\$', recursive=TRUE, full.names=TRUE)
vals <- tsvs %>% map(function(p) {
  d <- suppressWarnings(readr::read_tsv(p, col_types=readr::cols(.default=readr::col_character())))
  if (!'parasite_organism' %in% colnames(d)) return(NULL)
  tibble(raw_value = unique(na.omit(d[['parasite_organism']])))
}) %>% bind_rows() %>% distinct()

# canonical = first two whitespace-separated tokens (Genus species)
rules <- vals %>%
  mutate(canonical_value = word(raw_value, 1, 2)) %>%
  filter(!is.na(canonical_value), raw_value != canonical_value,
         str_count(raw_value, '\\\\S+') > 2) %>%
  transmute(variable='parasite_organism', raw_value, canonical_value,
            ontology_term_id=NA_character_,
            note='Isolate suffix stripped to Genus species')
cat('generated', nrow(rules), 'species-collapse rules\n')
readr::write_tsv(rules, 'stf/harmonization/rules/global/vocabulary.tsv',
                 append=TRUE, na='')
"
```
Expected: reports ~180 generated rules.

**Review the appended rows before continuing.** `word(raw_value, 1, 2)` is a heuristic; a
value like `Plasmodium sp. Congo` would collapse wrongly. Open the file and check.

- [ ] **Step 4: Write `rules/studies/PlasmoDB/pfal3D7/vocabulary.tsv`**

The two irreducibly local facts from spec §1 — site codes that mean nothing outside this
study. These land in `country`, which is why they are *reported* as country pollution too;
mapping them here at least makes them legible.

```
variable	raw_value	canonical_value	ontology_term_id	note
country	PLU	Thailand		Site code from collection_site leaked into country; PLU is a Thai clinic
country	MRH	Thailand		Site code: Mae Ra Mat Hospital, Thailand
```

- [ ] **Step 5: Verify the rulebooks load and validate**

```bash
cd /home/jbrestel/dataCleaning
Rscript -e "
suppressPackageStartupMessages(library(tidyverse))
HARMONIZATION_DIR <- normalizePath('stf/harmonization')
REPO_ROOT <- normalizePath('.')
for (f in list.files('stf/harmonization/R', pattern='[.]R\$', full.names=TRUE)) source(f)
rb <- load_rulebook('stf/harmonization/rules', 'PlasmoDB', 'pfal3D7')
cat('merged vocabulary rules:', nrow(rb\$vocabulary), '\n')
cat('variable actions:', nrow(rb\$actions), '\n')
cat('shadowed:', rb\$n_shadowed, '\n')
"
```
Expected: no error. A duplicate-key error means two rows share a normalized key inside one
layer — fix the rulebook, do not change the validator.

- [ ] **Step 6: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/rules
git commit -m "feat: author initial global and pfal3D7 rulebooks"
```

---

## Task 11: pfal3D7 golden integration test

**Files:**
- Test: `stf/harmonization/tests/testthat/test-integration-pfal3D7.R`

- [ ] **Step 1: Write the test**

Write `stf/harmonization/tests/testthat/test-integration-pfal3D7.R`:

```r
# Integration test against the real study. Skips if the study is absent.
pfal_dir <- function() file.path(REPO_ROOT, 'stf', 'studies', 'PlasmoDB', 'pfal3D7')

test_that("pfal3D7 harmonizes to the expected value collapses", {
  skip_if_not(dir.exists(pfal_dir()), 'pfal3D7 study not present')

  out_stf <- file.path(tempdir(), 'pfal_stf')
  out_rep <- file.path(tempdir(), 'pfal_rep')
  res <- harmonize_one_study(pfal_dir(), 'PlasmoDB', 'pfal3D7',
                             file.path(REPO_ROOT, 'stf', 'harmonization', 'rules'),
                             out_stf, out_rep)
  expect_equal(res$status, 'ok')

  e <- get_entity(study_from_stf(file.path(out_stf, 'PlasmoDB', 'pfal3D7')), 'sample')
  expect_true(validate(quiet(e)))
  d <- get_data(e)

  # organism renamed and collapsed: 327 'Homo sapiens' + 194 'human' = 521
  expect_true('host_organism' %in% colnames(d))
  expect_false('organism' %in% colnames(d))
  expect_setequal(na.omit(unique(d$host_organism)), 'Homo sapiens')
  expect_equal(sum(d$host_organism == 'Homo sapiens', na.rm = TRUE), 521)

  # display name and provenance
  md <- get_variable_metadata(e) %>% filter(variable == 'host_organism')
  expect_equal(md$display_name, 'Host organism')
  expect_equal(unlist(md$provider_label), 'organism')

  # specimen: 180 'Blood' + 116 'blood' = 296
  expect_equal(sum(d$specimen == 'blood', na.rm = TRUE), 296)
  expect_false('Blood' %in% d$specimen)

  # parasite_organism collapsed to species
  expect_setequal(na.omit(unique(d$parasite_organism)), 'Plasmodium falciparum')

  # row count unchanged: harmonization never adds or removes samples
  expect_equal(nrow(d), 537)
})

test_that("pfal3D7 structural findings include the country pollution we expect", {
  skip_if_not(dir.exists(pfal_dir()), 'pfal3D7 study not present')
  e <- get_entity(study_from_stf(pfal_dir()), 'sample')
  f <- structural_findings(e)

  cp <- f %>% filter(check == 'country_pollution')
  expect_true('Wang Pha, Thailand' %in% cp$value)
  expect_true('Maela, Thailand' %in% cp$value)
  expect_true('PLU' %in% cp$value)
  expect_false('Senegal' %in% cp$value)

  # parasite_strain flagged as identifier-like (536 distinct / 537 rows)
  expect_true('parasite_strain' %in%
                (f %>% filter(check == 'identifier_like') %>% pull(variable)))

  # continent incompleteness flagged (147/537 populated)
  expect_true('continent' %in%
                (f %>% filter(check == 'continent_incomplete') %>% pull(variable)))
})

test_that("the harmonized pfal3D7 study never modifies its input", {
  skip_if_not(dir.exists(pfal_dir()), 'pfal3D7 study not present')
  before <- tools::md5sum(list.files(pfal_dir(), full.names = TRUE))
  harmonize_one_study(pfal_dir(), 'PlasmoDB', 'pfal3D7',
                      file.path(REPO_ROOT, 'stf', 'harmonization', 'rules'),
                      file.path(tempdir(), 'pfal_ro_stf'),
                      file.path(tempdir(), 'pfal_ro_rep'))
  expect_equal(tools::md5sum(list.files(pfal_dir(), full.names = TRUE)), before)
})
```

- [ ] **Step 2: Run the test**

Run: `cd /home/jbrestel/dataCleaning && Rscript stf/harmonization/tests/run_tests.R`
Expected: PASS.

If the `specimen == 'blood'` count is not 296, the corpus `blood` rule did not fire —
check that `rules/generated/vocabulary_from_corpus.tsv` exists (Task 2 Step 5). If
`host_organism` is missing, `rules/global/variables.tsv` is not being read — check the
header spelling matches `VARIABLES_COLS` exactly.

- [ ] **Step 3: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/tests/testthat/test-integration-pfal3D7.R
git commit -m "test: pfal3D7 golden integration test"
```

---

## Task 12: Full 62-study run and review

**Files:**
- Create: `stf/harmonization/README.md`

- [ ] **Step 1: Run pfal3D7 alone through the CLI**

```bash
cd /home/jbrestel/dataCleaning
Rscript stf/harmonization/harmonize_stf.R PlasmoDB/pfal3D7
```
Expected: `ok=1 failed=0`.

- [ ] **Step 2: Review the pfal3D7 reports by hand**

```bash
cd /home/jbrestel/dataCleaning
column -t -s $'\t' stf/reports/PlasmoDB/pfal3D7/changes.tsv | head -30
cat stf/reports/PlasmoDB/pfal3D7/structural_findings.md
diff stf/reports/PlasmoDB/pfal3D7/inspect_before.txt \
     stf/reports/PlasmoDB/pfal3D7/inspect_after.txt
head -30 stf/reports/PlasmoDB/pfal3D7/unresolved_values.tsv
```

Check: every row in `changes.tsv` is a change you would defend, and the `inspect` diff
shows only intended differences.

- [ ] **Step 3: Run all 62 studies**

```bash
cd /home/jbrestel/dataCleaning
Rscript stf/harmonization/harmonize_stf.R 2>&1 | tee /tmp/harmonize_all.log
```
Expected: `ok=62 failed=0`. Any failure is reported per-study with a message; the run
continues regardless. Investigate failures from `stf/reports/harmonization_summary.tsv`.

- [ ] **Step 4: Review the cross-study summary**

```bash
cd /home/jbrestel/dataCleaning
column -t -s $'\t' stf/reports/harmonization_summary.tsv
# any study with shadowed_rules > 0 means a local rule is overriding a global one
awk -F'\t' 'NR==1 || $11 > 0' stf/reports/harmonization_summary.tsv
# the curation work queue, highest-value first
head -40 stf/reports/unresolved_values.tsv
```

- [ ] **Step 5: Write `stf/harmonization/README.md`**

```markdown
# STF Harmonization

Collapses inconsistent categorical values and fixes variable naming across the STF studies
in `stf/studies/`, driven entirely by declarative TSV rulebooks. **Inputs are never
modified** — output goes to `stf/studies_harmonized/` and `stf/reports/`.

Design: `docs/superpowers/specs/2026-08-07-stf-harmonization-design.md`

## Usage

```bash
Rscript stf/harmonization/harmonize_stf.R PlasmoDB/pfal3D7   # one study
Rscript stf/harmonization/harmonize_stf.R                    # all studies
Rscript stf/harmonization/tests/run_tests.R                  # tests
```

## Rule layers

Precedence is **layer first, specificity second**: layer 2 > layer 1 > layer 0, and within
one layer a variable-specific rule beats a `*` wildcard.

| Layer | Path | Edit by hand? |
|---|---|---|
| 0 | `rules/generated/vocabulary_from_corpus.tsv` | **No** — regenerate from `stf/ontology_corpus.json` |
| 1 | `rules/global/{vocabulary,variables}.tsv` | Yes |
| 2 | `rules/studies/$project/$organism/{vocabulary,variables}.tsv` | Yes, for local facts only |

If a study rule shadows a global rule the engine warns and counts it in
`harmonization_summary.tsv`. A rising shadow count means the vocabulary is fragmenting —
promote the rule to global instead.

## Regenerating layer 0

```bash
Rscript -e "
suppressPackageStartupMessages(library(tidyverse))
for (f in list.files('stf/harmonization/R', pattern='[.]R\$', full.names=TRUE)) source(f)
write_corpus_vocabulary('stf/ontology_corpus.json',
                        'stf/harmonization/rules/generated/vocabulary_from_corpus.tsv')"
```

## Adding a rule

1. Find the value in `stf/reports/unresolved_values.tsv` (ordered by rows affected).
2. Add a row to `rules/global/vocabulary.tsv`. Set `canonical_value` to the **ontology
   term label**, not the most frequent spelling — if `ontology_term_id` is filled in later,
   the label must already agree with it.
3. Re-run and check `changes.tsv`.

## What this does NOT do

Structural defects are detected and written to `structural_findings.md`, but never fixed:
splitting `country` into country + site, computing `continent`, retyping `age`, splitting
`specimen` into material/context/anatomy axes, parsing date ranges, and rebuilding the
antifungal MIC panel. Those are pass two, and should be designed from the reported counts.

The real end state is lifting the stabilized rulebook **upstream into `harmonize.py`**, so
this stops being a downstream patch. The TSV format exists to make that a data move rather
than a rewrite.
```

- [ ] **Step 6: Commit**

```bash
cd /home/jbrestel/dataCleaning
git add stf/harmonization/README.md
git commit -m "docs: add harmonization README"
```

- [ ] **Step 7: Report back before any pass-two work**

Summarize for John: total substitutions, rows affected, studies with shadow warnings, the
top 20 unresolved values, and the structural-findings counts — specifically **how many
studies have polluted `country` values**, since spec §8 says that number drives the pass-two
design and it is currently unknown.

---

## Amendments Made During Execution

Recorded so this plan does not drift from the committed code. The code is authoritative;
the design spec has been updated to match.

**Task 1 (normalization)** — Added tests pinning three known quirks of the shared
algorithm as *deliberate*: non-ASCII is stripped rather than folded (`León` → `len`),
punctuation-only differences collide (`1-2` ≡ `12`), and all-punctuation collapses to `''`.
Not fixed, because `norm_value()` must stay byte-identical to `harmonize.py`'s `normh()`.
Verified empirically that exactly one non-ASCII value exists across all 62 studies, in a
variable pass one does not harmonize. See spec §3.3.

**Task 2 (layer-0 generation)** — Two real defects in the plan as written:
1. Deduped on the *verbatim* `raw_value` while Task 3's validator rejects duplicate
   *normalized* keys, so the generator produced a file that would fail our own validation.
   The real corpus contains one such pair (`Vietnam` / `Viet Nam`, both `GAZ_00003756`).
   Now dedupes on the normalized key, and **hard-errors when colliding spellings claim
   different canonicals** — that is a corpus curation error and must not be collapsed.
2. A malformed corpus (missing top-level `corpus` key) silently produced an empty
   vocabulary, which `write_corpus_vocabulary()` would have written over the committed
   361-row artifact. Now guarded.
   Also added tests pinning byte-reproducibility of the committed artifact and the
   type-exclusion filter against the *real* corpus.
   Incidental finding: `ontology_corpus.json` itself contains 105 groups of literal
   duplicate term objects (480 spelling rows → 362 before normalization).

**Task 3 (rulebook merge)** — One significant design bug in the plan:
shadow detection joined on exact `variable` equality, which missed the
layer-2-wildcard-over-layer-1-specific case entirely. Since a layer-2 wildcard *outranks* a
layer-1 specific rule, that is the broadest possible local override silently escaping the
guard built to prevent vocabulary fragmentation — and it would never surface by running the
engine, because the output looks clean. `count_shadowed_rules` is now a wrapper around
`shadowed_rules()`, which joins on `key` alone with a scope-overlap predicate and classifies
each hit `exact` / `broadens` / `narrows`. See spec §4.1.
Also: `load_rulebook()` returns the shadowed *rows* rather than only a count (the reporting
task needs to name what was overridden); vocabulary layers are validated at the top of
`load_rulebook` so the shadow count no longer depends on call ordering; `resolve_value()`
errors on vector input instead of silently using the first element; `read_rules_tsv()` passes
`quote = ''` so TSV parsing is explicit rather than inheriting readr's CSV quote sniffing;
and companion-field presence (`rename` needs `new_name` or `new_display_name`, `reparent`
needs `new_parent`) is validated at load time so a curator's typo names the offending row.
The plan's `vocab()` test helper could not build a zero-row layer — `bind_rows(list())`
returns a column-less tibble — and now early-returns `empty_vocabulary()`.

**Known, accepted, not fixed:** `validate_variable_actions()` tests companion fields with
`is.na()`, so a hand-built tibble containing a whitespace-only `new_name` passes. Unreachable
via `load_rulebook()`, which always routes through `read_rules_tsv()` and blanks
whitespace-only cells to `NA`. Robustness nit on a semi-public function, not a live defect.

**Deliberately deferred:** vectorizing `resolve_value()` (scalar-only by design; ~9k values
against ~400 rules is trivial, and obvious correctness was preferred); a shared R/Python
golden-fixture harness for normalization drift (pass-three work, once the rulebook is
actually lifted into `harmonize.py`).

## Self-Review

**Spec coverage:**

| Spec section | Task |
|---|---|
| §1 defect classes 1, 2, 5 (fix) | 5, 6, 10 |
| §1 defect classes 3, 4, 6 (report) | 7 |
| §2 D1 declarative rules | 3, 10 |
| §2 D2 layering, D3 shadow warning | 3 |
| §2 D4 mechanical/curatorial only | 6 (drop guard), 7 |
| §2 D5 ontology canonical labels | 2, 10 step 2 |
| §2 D6 generated layer 0 below global | 2, 3 |
| §2 D7 never modify inputs | 9, 11 (md5sum tests) |
| §3.1 per-study CLI | 9 |
| §3.2 six components | 1–9 |
| §4.1 vocabulary.tsv + wildcard precedence | 3 |
| §4.2 variables.tsv + drop restriction | 6 |
| §4.3 rename recipe + provenance | 6 |
| §4.4 type preservation | 5 |
| §5.1–5.4 per-study outputs | 8 |
| §5.5–5.6 repo-level outputs | 8, 9 |
| §5.1 inspect before/after | 8, 9 |
| §6 error handling table | 3 (dupes, actions), 6 (drop, reparent), 9 (load/export/validate) |
| §7 all listed tests | 1–9, 11 |
| §8 out of scope | 7 (reported), 12 step 5 (documented) |
| §9 version control | 0 |

No gaps.

**Placeholder scan:** No TBD/TODO. Every code step contains complete runnable code. Task 10
step 3 is a generator with a stated review requirement rather than 180 hand-written rows —
the command is complete and its heuristic risk is called out.

**Type consistency:** `VOCAB_COLS` (Task 2) is used by `read_vocabulary` and
`empty_vocabulary` (Task 3) and asserted in Task 2's test. `VARIABLES_COLS` /
`VALID_ACTIONS` (Task 3) match the TSV headers in Task 10. `merge_vocabulary_layers` /
`resolve_value` / `load_rulebook` (Task 3) are consumed with matching signatures in Tasks 5,
6, 9. `empty_applied_actions` is defined in Task 6 and used in Tasks 6, 8, 9.
`empty_findings` is defined in Task 7 and used in Tasks 7, 8. The summary-row columns
written in Task 9 match the columns asserted in Task 8's `write_summary` test. The change-log
columns in Task 5 (`variable, raw_value, canonical_value, rows_affected, layer,
ontology_term_id`) match Task 8's test and spec §5.2.

One deliberate inconsistency, flagged rather than fixed: Task 9 writes
`inspect_before.txt` once per entity into a single per-study path, so a multi-entity study
would overwrite it. Every study in `stf/studies/` currently has exactly one entity
(`sample`), so this is correct today and will need an entity-scoped filename when the assay
entity arrives from Nextflow. Noted here so the next engineer sees it rather than
discovering it.

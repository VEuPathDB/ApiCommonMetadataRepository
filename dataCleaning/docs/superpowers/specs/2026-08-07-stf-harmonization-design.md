# STF Value Harmonization — Design

**Date:** 2026-08-07
**Status:** Approved (pending spec review)
**Scope:** Post-export harmonization of variable names and categorical values across the 62 STF studies in `stf/studies/$project/$orgAbbrev/`.

---

## 1. Problem

The STF studies produced by `stf/harmonize.py` → `stf/export_stf.R` contain systematic
variable-naming and value-vocabulary defects that make categorical variables unusable as
EDA filter facets.

Surveyed evidence (all 62 studies, 84 distinct variables, 43,423 non-empty
non-identifier cells):

| # | Defect class | Evidence |
|---|---|---|
| 1 | Host/parasite conflation | `organism` is parented to `host` but named as the organism under investigation. 43 studies, 103 distinct values. `host_common_name` (12 studies) duplicates the same axis. |
| 2 | Case / spelling / inflection variants | `Homo sapiens` / `human` / `Human` (3,286 rows). `Blood` / `blood` (650). `Female` / `female` (140). `Cryptococcal meningitis` / `cryptocococcal meningitis` (typo, 56). `mycelia` / `mycellium`. `28 year` / `28 years`. |
| 3 | `country` polluted with sub-national places | pfal3D7: Thailand appears as 5 distinct "countries" — `Wang Pha, Thailand` (103), `Maela, Thailand` (55), `Mawker Tai, Thailand` (16), `Mae Khon Kaen, Thailand` (1), `Thailand` (1). Plus unresolved site codes `PLU` (3) and `MRH` (2). `collection_site` already holds the correct site values, with different spellings (`Mawker Thai` vs `Mawker Tai`, `Mae Kon Ken` vs `Mae Khon Kaen`). |
| 4 | `continent` incomplete and wrong | pfal3D7: 147/537 populated, all `South America`, while countries include Senegal, Gambia, Mali, Uganda, Cambodia. Derivable from a clean `country`; should be computed, not carried. |
| 5 | Near-duplicate identifier variables | pfal3D7 `parasite_strain`: 536 distinct values / 537 rows — it is `"Plasmodium falciparum " + sample_id`. `parasite_organism`: 181 distinct, 357 clean species + 180 species-plus-isolate. Useless as facets. |
| 6 | Type and structure errors | `age` typed `string`/`categorical` holding `"29 years"`, `"4 weeks"` — mixed units. `collection_date_original` holds ranges (`1999-2002`) and datetimes. `specimen` (266 values) mixes material (`blood`, `soil`, `leaf`), clinical context (`Clinical`, `Screening`, `Veterinary`) and host anatomy (`Vagina`, `lung`); `specimen from organism` (301 rows) is the bare OBI parent class, carrying no information. Junk variables from header-parsing failures: `and` (10 values), `group`, and an antifungal MIC panel split into abbreviation-named columns `icz`, `fcz`, `pz`, `vcz`, `amb`, `mf`, `csf`, `h2o2`, `triazole`. |

### 1.1 Root cause

`stf/ontology_corpus.json` already contains 361 `characteristicValue` terms, each with a
canonical label and an ontology source ID:

```json
"human":       {"source_id": "NCBITaxon_9606", "canonical": "Homo sapiens",
                "spellings": ["Homo sapiens", "human"]}
"homosapiens": {"source_id": "NCBITaxon_9606", "canonical": "Homo sapiens",
                "spellings": ["Homo sapiens", "human"]}
```

`harmonize.py` loads this corpus but applies it **only to column headers** — `normh()` is
called on header strings and never on cell values. The value-level curation in that file is
never consulted. Defect class 2 is therefore an *unapplied* rulebook, not a missing one.

Measured corpus coverage against current STF values:

- **38% of non-identifier rows** (16,317 / 43,423) resolve to a `characteristicValue` term.
- **3% of distinct (variable, value) pairs** (288 / 8,924) — the long tail is strain and
  isolate free-text, which correctly has no ontology term.
- **22 collapses** fire from existing curation alone, covering ~5,300 rows, at zero
  curation cost.

---

## 2. Decisions

| ID | Decision | Rationale |
|----|----------|-----------|
| D1 | Rules are **declarative data files** + a thin generic R engine. Not inline `case_when` in R. | Curators can edit and diff rules without touching code. The rulebook is portable verbatim into `harmonize.py` when the fix is eventually lifted upstream, and gains an `ontology_term_id` column to become an ontology-backed vocabulary service later. |
| D2 | Rules are **layered**, with study-specific rules overriding global. | Most collisions are global facts (`human`/`Human` spans 43 studies). Some are irreducibly local (`PLU` means a hospital only in pfal3D7). Both must be expressible. |
| D3 | The engine **warns when a study rule shadows a global rule.** Shadowing means same normalized key **and overlapping variable scope** — including the wildcard cases (see below). | Without this, D2 decays into 62 divergent rulebooks, re-fragmenting the vocabulary. The warning is the signal that a local rule should be promoted, or that someone is fragmenting. |
| D4 | First pass fixes **mechanical + curatorial** defects (classes 1, 2, 5). **Structural** defects (3, 4, 6) are *reported with evidence only*. | Structural fixes change the entity schema and cannot be expressed as value-rewrite rules; adding them now means a special case per defect, collapsing D1. Pass two is designed from the reported counts rather than from guesses. |
| D5 | Canonical labels come from **the ontology corpus**, never from value frequency or personal preference. | If `vocabulary.tsv` later carries `ontology_term_id`, its labels must already agree with the term labels or the work is redone. Concretely: pfal3D7 `organism` becomes **`Homo sapiens`** (NCBITaxon_9606), *not* `human`. Explicitly supersedes the initial `"human"` preference. |
| D6 | The corpus-derived vocabulary layer is **generated, not hand-edited**, and sits *below* hand-authored global rules. | Regenerable when the corpus changes. Sitting below global rules means a hand-authored rule can override wrong curation without editing a generated file. |
| D7 | Inputs are **never modified in place.** | Output goes to a new tree; `stf/studies/` stays byte-identical and re-runnable. |

---

## 3. Architecture

```
stf/studies/$project/$org/                      inputs, read-only
        │
        │   study_from_stf(dir)                 study.wrangler
        ▼
┌──────────────────────────────────────────────────────────┐
│  rules engine — thin, generic, ~200 lines, never changes │
│                                                          │
│  layer 0  rules/generated/vocabulary_from_corpus.tsv     │  generated — do not edit
│  layer 1  rules/global/{vocabulary,variables}.tsv        │  hand-authored
│  layer 2  rules/studies/$project/$org/{...}.tsv          │  hand-authored, local facts
└──────────────────────────────────────────────────────────┘
        │
        │   export_to_stf(study, dir)      writes study.yaml + entity-* INTO dir
        ├──► stf/studies_harmonized/$project/$org/          new STF
        └──► stf/reports/$project/$org/                     audit + structural findings
```

Layer precedence: **2 > 1 > 0**, matched on `(variable, normalized_raw_value)`. Higher
layers override lower. Every substitution records which layer fired.

### 3.1 Entry point

```
Rscript harmonize_stf.R PlasmoDB/pfal3D7      # one study
Rscript harmonize_stf.R                        # all 62
```

One study directory at a time, loaded via `study_from_stf()`, as required. Run with a
local R install and `devtools::load_all()` against `study-wrangler/` — no Docker.

### 3.2 Components

Each unit has one purpose and is independently testable.

| Unit | Responsibility | Depends on |
|------|----------------|------------|
| `R/corpus_rules.R` | Generate layer 0 from `ontology_corpus.json`: for each `characteristicValue` term, emit one row per spelling → canonical. | `ontology_corpus.json` |
| `R/rulebook.R` | Load + merge layers 0/1/2 into one lookup keyed `(variable, normalized_value)`. Emit shadow warnings (D3). Validate: no duplicate keys within a layer, no rule targeting an unknown `action`. | layer files |
| `R/apply_rules.R` | Given an `Entity` and a merged rulebook, apply value substitutions and variable actions. Returns modified entity + a tibble of every change. | `rulebook.R`, study.wrangler |
| `R/structural_report.R` | Detect and count the D4-deferred defects. Emits markdown + a machine-readable tibble. Makes no changes. | study.wrangler |
| `R/report.R` | Render `changes.tsv`, `structural_findings.md`, and the repo-level summary. | outputs of the above |
| `harmonize_stf.R` | CLI: resolve study dirs, orchestrate, write outputs, set exit status. | all of the above |

`normalized_value` uses the same normalization as `harmonize.py`'s `normh()` —
lowercase, strip all non-alphanumerics — so layer 0 keys match the corpus keys exactly and
the rulebook stays portable upstream.

### 3.3 Known limitations of the normalization key

Properties of the shared algorithm, accepted deliberately because R and Python must stay
byte-identical or the rulebook stops being liftable into `harmonize.py`. All three are
pinned by tests so they cannot be "fixed" on one side only.

| Quirk | Example | Current impact |
|---|---|---|
| Non-ASCII stripped, not folded | `León` → `len`; `Leon` → `leon` (different keys) | Nil. Exactly **one** non-ASCII value exists across all 62 studies — `León` in `FungiDB/alenIFM54703`'s `geographic_location`, a variable pass one does not harmonize. Revisit if accented country names ever appear. |
| Punctuation-only differences collide | `1-2` and `12` share a key | Nil for categorical vocabularies. Would matter if numeric-like values were ever rule-matched, which they are not — substitution is restricted to `categorical`/`string` variables (§4.4). |
| All-punctuation collapses to the empty key | `-`, `--`, `???`, `''` → `''` | Low. A rule keyed on any punctuation-only sentinel would match all of them. No such rule exists; if one is ever authored, the duplicate-key check in §6 catches the second one. |

Changing any of these requires changing `harmonize.py` in the same commit.

---

## 4. Rule kinds

Exactly two, per D4. Adding a third is the signal that a structural change is being
smuggled into pass one.

### 4.1 `vocabulary.tsv` — value collapse

| Column | Required | Notes |
|--------|----------|-------|
| `variable` | yes | STF variable name. `*` matches any variable (for cross-variable vocabularies such as host species). |
| `raw_value` | yes | Verbatim source value; matched after normalization. |
| `canonical_value` | yes | Replacement. Must equal the ontology term label where a term exists (D5). |
| `ontology_term_id` | no | Populated automatically in layer 0. Blank in hand-authored rules until curated. The on-ramp to an ontology-backed vocabulary. |
| `note` | no | Free text; why this rule exists. |

The `*` wildcard exists because `organism`, `host_common_name`, and `lab_host` share a
host-species vocabulary.

**Resolution order, fully specified:** layer precedence is evaluated first, specificity
second. A layer-2 wildcard rule therefore beats a layer-1 variable-specific rule. Within a
single layer, a variable-specific rule beats a wildcard. This ordering is chosen because it
keeps D2's promise unconditional — a study-specific rulebook always wins for that study,
with no exceptions a curator has to remember.

**Shadow detection must account for wildcards.** Because a layer-2 wildcard outranks a
layer-1 variable-specific rule, detecting shadowing by exact variable equality misses the
single most dangerous case: a study `*` rule is the broadest possible local override, and it
would silently win while reporting zero shadowed rules. Shadowing is therefore defined as
*same normalized key with overlapping variable scope*, where scopes overlap if the variables
are equal or either is `*`. Three cases, all reported, each labelled in the output:

| Case | Meaning |
|---|---|
| `exact` | Study rule and global rule name the same variable. |
| `study wildcard broadens over global specific` | Layer-2 `*` overrides a layer-1 named variable. The dangerous case. |
| `study specific narrows a global wildcard` | Layer-2 named variable carves an exception out of a layer-1 `*`. Lower risk, still reported. |

`load_rulebook()` returns the shadowed rows themselves, not merely a count, so
`harmonization_summary.tsv` can name what was overridden rather than just how often.

### 4.2 `variables.tsv` — name, parent, and existence

| Column | Required | Notes |
|--------|----------|-------|
| `variable` | yes | Current STF variable name. |
| `action` | yes | One of `rename`, `reparent`, `drop`. |
| `new_name` | for `rename` | New variable (column) name. |
| `new_display_name` | no | New `display_name`. |
| `new_parent` | for `reparent` | New `parent_variable` category. |
| `reason` | yes | Mandatory. A dropped or renamed variable without a stated reason is unreviewable. |

**`drop` is restricted in pass one** to variables that are 100% null or are header-parsing
artifacts with no interpretable meaning. Dropping a *populated, interpretable* variable is a
schema change and falls under D4's structural class — it is reported, not applied. This
restriction is enforced by the engine: a `drop` rule targeting a variable with non-null
values that is not on an explicit `artifact` allowlist is a hard error.

Worked examples for pass one:

```tsv
variable            action    new_name        new_display_name   new_parent  reason
organism            rename    host_organism   Host organism      host        Parented to host but named as organism under investigation
and                 drop                                                     Header-parsing artifact, not a variable
group               drop                                                     Header-parsing artifact, uninterpretable
```

Two variables are deliberately **not** acted on in pass one, despite being obvious targets:

- **`parasite_strain`** (536 distinct / 537 rows; equals `"Plasmodium falciparum " + sample_id`)
  is populated and interpretable. Dropping it is structural. Reported in
  `structural_findings.md` under the identifier-like-variable check.
- **`host_common_name`** duplicates the `host_organism` axis, but merging two variables is
  structural. Its *values* are still harmonized in pass one — `Human` / `human` /
  `Homo sapiens` all collapse to `Homo sapiens` — so after pass one the two variables agree
  on vocabulary, which is what makes the pass-two merge safe and mechanical.

`parasite_organism` **is** acted on: it is collapsed to species by `vocabulary.tsv` rules
(`Plasmodium falciparum NHP1523` → `Plasmodium falciparum`). That is a value rewrite, not a
schema change, and therefore in scope.

### 4.3 Variable rename mechanism — resolved by spike

Verified against pfal3D7 with a local `devtools::load_all()`. No longer an open question.

`display_name` is a first-class variable metadata field
(`Entity-metadata-defaults.R:10`), independent of `variable` (the data column name). Two
mechanisms exist and they do different jobs:

**Display-name-only change** — one call, metadata intact, validates:

```r
e %>% set_variable_metadata('organism', display_name = 'Host organism')
```

**Full rename (column + metadata)** — three steps, because
`sync_variable_metadata()` validates the entity but **wipes** `display_name`,
`definition`, and `parent_variable`, and rewrites `provider_label` to the *new* name.
Metadata must be captured before the rename and re-applied after:

```r
old <- get_variable_metadata(e) %>% filter(variable == 'organism')   # capture FIRST

e %>%
  modify_data(rename(host_organism = organism)) %>%
  sync_variable_metadata() %>%                       # else: "metadata is missing for
                                                     # data columns: host_organism"
  set_variable_metadata('host_organism',
    display_name    = 'Host organism',
    definition      = old$definition,
    parent_variable = old$parent_variable,
    data_type       = as.character(old$data_type),
    data_shape      = as.character(old$data_shape),
    provider_label  = as.list(unlist(old$provider_label)))   # list column; must be wrapped
```

Result: `validate()` returns `TRUE`, all metadata preserved.

**`provider_label` is provenance and must NOT be rewritten by a rename.** It records what
the data provider called the column. `sync_variable_metadata()` sets it to
`host_organism`, which is false — the provider said `organism`. The recipe above restores
the original value deliberately. Any rename that lets `provider_label` follow the new name
destroys the trace back to the source data, so the engine restores it explicitly and a test
asserts it.

Both mechanisms are used: `rename` performs the column change, and `new_display_name`
sets the label. A `variables.tsv` row may specify either or both.

### 4.4 Value substitution must preserve column type

Also established by spike, and a live trap. The wrangler's documented idiom for value
recoding is `modify_data(mutate(x = fct_recode(...)))`, but STF categorical columns load as
**`character`** with `data_type: string`. `fct_recode` requires a factor and leaves a factor
behind, so the column's class no longer matches its declared `data_type` and
**`validate()` fails**:

```r
# source: class(organism) == "character", data_type == "string"

modify_data(mutate(organism = fct_recode(as.factor(organism), `Homo sapiens` = 'human')))
#   -> class "factor",    validates: FALSE

modify_data(mutate(organism = as.character(
              fct_recode(as.factor(organism), `Homo sapiens` = 'human'))))
#   -> class "character", validates: TRUE   (521 rows Homo sapiens)
```

The engine therefore round-trips substitutions back to the source column class rather than
trusting the recode helper. This is the failure mode most likely to be reintroduced by a
later well-meaning refactor toward the "idiomatic" call, so §7 carries an explicit
regression test.

---

## 5. Outputs

Per study, in `stf/studies_harmonized/$project/$org/`:

1. **Harmonized STF** — `study.yaml`, `entity-*.yaml`, `entity-*.tsv`. Re-validated through
   `study.wrangler` before writing, so invalid STF is never emitted.

Per study, in `stf/reports/$project/$org/`:

2. **`changes.tsv`** — one row per substitution: `variable, raw_value, canonical_value,
   rows_affected, layer, ontology_term_id`. The human review surface.
3. **`variable_actions.tsv`** — one row per rename/reparent/drop applied, with `reason`.
4. **`structural_findings.md`** — the D4-deferred defects with the counts needed to design
   pass two:
   - `country` values containing a comma, or matching a known `collection_site` value, or
     ≤4 characters and all-uppercase (site-code candidates) — per study and in total.
   - `continent` null rate, and rows where `continent` disagrees with the country's true
     continent.
   - `age` values by unit token (`year`/`years`/`week`/`weeks`/bare number).
   - `collection_date_original` values that are ranges or datetimes.
   - `specimen` values classified as material vs clinical-context vs host-anatomy vs
     uninformative-parent-class, listed for curator triage.
   - Variables that are 100% null, or whose distinct-value count is ≥95% of their non-null
     row count (identifier-like, useless as facets).

Repo-level, in `stf/reports/`:

5. **`harmonization_summary.tsv`** — one row per study: rows, variables, substitutions
   applied, variables renamed/dropped, unresolved-value count, shadow-warning count.
6. **`unresolved_values.tsv`** — every (variable, value) with no rule in any layer, with
   row counts, ordered by frequency. **This is the work queue for curation.** Highest-value
   entries today: `fungus_organism` (40 species values, 1,110 rows for
   *Aspergillus fumigatus* alone, plus `[Candida] auris` vs `Candida auris`),
   `specimen`'s non-material values, `health_status` (86 values), `lineage`.

### 5.1 Visualization

`inspect()` on the loaded `Study` / `Entity` (study.wrangler's variable-and-value
visualization) is captured before and after harmonization into
`stf/reports/$project/$org/inspect_before.txt` and `inspect_after.txt`. The diff of those
two files is the fastest human check that a run did what was intended.

---

## 6. Error handling

| Condition | Behavior |
|-----------|----------|
| Input study fails `study_from_stf()` validation | Skip study, record in summary with the validation message, continue to next, non-zero exit at end. One bad study must not abort a 62-study run. |
| Rule references a variable absent from the study | Not an error — global rules legitimately name variables most studies lack. Counted and reported as `rules_not_applicable`. |
| Duplicate `(variable, raw_value)` key **within one layer** | Hard error. Ambiguous curation must be fixed, not silently resolved. |
| Study rule shadows a global rule | Warning + counted in the summary (D3). Not an error. |
| `canonical_value` disagrees with the corpus label for a supplied `ontology_term_id` | Warning naming both labels. Catches D5 violations at authoring time. |
| Harmonized study fails validation on export | Hard error for that study; write nothing for it. Never emit unloadable STF. |
| `drop` action with empty `reason` | Hard error. |

---

## 7. Testing

`testthat`, matching `study-wrangler/tests/testthat/`.

**Unit:**
- `corpus_rules.R`: the known `human` / `Homo sapiens` / `Human` → NCBITaxon_9606 collapse
  is generated; a term with one spelling yields one row; non-`characteristicValue` terms
  are excluded.
- `rulebook.R`: layer precedence 2 > 1 > 0; wildcard loses to variable-specific within a
  layer; duplicate key within a layer errors; shadowing warns.
- `apply_rules.R`: substitution counts are exact; a rule for an absent variable is a no-op;
  `drop` removes the column *and* its metadata; values with no rule pass through untouched.
- `apply_rules.R` **type-preservation regression** (§4.4): after substitution, the column's
  class equals its class before substitution, and `validate()` returns `TRUE`. Asserts on
  the column class directly, not just on `validate()`, so the test names the actual defect
  when it fires.
- `apply_rules.R` **rename provenance** (§4.3): after renaming `organism` →
  `host_organism`, `provider_label` still reads `organism`, and `definition`,
  `parent_variable`, `data_type`, `data_shape` all survive.
- `structural_report.R`: on a fixture with `Wang Pha, Thailand` and `PLU`, both are flagged;
  a clean country column produces no findings.

**Integration:**
- Round-trip: a study with an empty rulebook exports STF that is byte-identical to its
  input, modulo key ordering. Proves the engine is a no-op absent rules.
- pfal3D7 golden test: `organism` collapses 327 + 194 → 521 rows of `Homo sapiens`;
  `specimen` collapses 180 + 116 → 296 rows of `blood`; output loads via
  `study_from_stf()` and validates.

**Fixtures:** small hand-written STF directories under `tests/testthat/fixtures/`. Not the
real 62 studies — those are the integration smoke target, run separately.

---

## 8. Out of scope (pass two candidates)

Reported by this pass, fixed by a later one:

- Splitting `country` into `country` + `collection_site`; requires a gazetteer.
- Computing `continent` from a clean `country`.
- Retyping `age` to numeric + unit; requires unit normalization across years/weeks.
- Splitting `specimen` into material / clinical-context / host-anatomy axes; requires a
  266-value three-way classification.
- Parsing `collection_date_original` ranges into start/end.
- Reconstructing the antifungal MIC panel (`icz`, `fcz`, `pz`, `vcz`, `amb`, …) into
  properly named drug-MIC variables.
- **Lifting the stabilized rulebook upstream into `harmonize.py`**, so the fix stops being
  a downstream patch. This is the real end state; D1's file format exists to make it a
  data move rather than a rewrite.

---

## 9. Note on version control

`/home/jbrestel/dataCleaning` is not a git repository, so this document cannot be
committed. If the rulebooks are going to be curated by more than one person — and D1/D2
assume they will be — the rules directory needs version control before curation starts.
A diffable rulebook that nobody can diff is just a slower `case_when`.

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
  write_study_reports(out, empty_changes(), empty_applied_actions(), empty_findings(),
                      empty_unresolved())
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

test_that("write_unresolved_summary aggregates across studies", {
  p <- file.path(tempdir(), 'unres_summary.tsv')
  write_unresolved_summary(list(
    tibble(variable = 'country', raw_value = 'PLU', rows_affected = 3L),
    tibble(variable = 'country', raw_value = 'PLU', rows_affected = 2L),
    tibble(variable = 'specimen', raw_value = 'culture', rows_affected = 9L)), p)
  back <- readr::read_tsv(p, show_col_types = FALSE)
  plu <- back %>% filter(raw_value == 'PLU')
  expect_equal(plu$rows_affected, 5)
  expect_equal(plu$n_studies, 2)
  # ordered by rows_affected descending
  expect_equal(back$rows_affected, sort(back$rows_affected, decreasing = TRUE))
})

test_that("write_unresolved_summary handles empty input", {
  p <- file.path(tempdir(), 'unres_empty.tsv')
  write_unresolved_summary(list(empty_unresolved()), p)
  expect_true(file.exists(p))
})

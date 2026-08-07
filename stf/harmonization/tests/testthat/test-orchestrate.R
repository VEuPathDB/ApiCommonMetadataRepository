test_that("harmonize_one_study produces valid STF and a change log", {
  rules <- file.path(tempdir(), 'rules_ok')
  dir.create(file.path(rules, 'global'), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(rules, 'generated'), recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(
    # One rule, not one per casing: rule keys are normalized, so 'human' and 'Human'
    # collide and load_rulebook() rightly rejects the pair as ambiguous curation.
    # The single rule still rewrites both rows of the fixture.
    tibble(variable = '*', raw_value = 'human',
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

  reloaded <- study_from_stf(file.path(out_stf, 'TestDB', 'tiny'))
  e <- get_entity(reloaded, 'sample')
  expect_true(validate(quiet(e)))
  expect_true('host_organism' %in% colnames(get_data(e)))
  expect_setequal(na.omit(unique(get_data(e)$host_organism)), 'Homo sapiens')

  expect_true(file.exists(file.path(out_rep, 'TestDB', 'tiny', 'changes.tsv')))
  expect_true(file.exists(file.path(out_rep, 'TestDB', 'tiny', 'inspect_before_sample.txt')))
  expect_true(file.exists(file.path(out_rep, 'TestDB', 'tiny', 'inspect_after_sample.txt')))
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

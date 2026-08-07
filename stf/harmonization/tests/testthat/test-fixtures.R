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

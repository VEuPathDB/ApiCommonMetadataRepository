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

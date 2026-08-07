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

# These are KNOWN, DELIBERATE properties of the shared normalization algorithm.
# They are pinned here so nobody "fixes" them in R alone: norm_value() must stay
# byte-identical to normh() in stf/harmonize.py, or the TSV rulebooks stop being
# liftable into that script. If any of these ever need to change, change BOTH files
# together and update this test in the same commit.
test_that("known quirk: non-ASCII characters are stripped, not folded", {
  # 'Leon' and 'Leon' with an accent produce DIFFERENT keys. Currently harmless:
  # exactly one non-ASCII value exists across all 62 studies (Leon, in
  # FungiDB/alenIFM54703 geographic_location), and that variable is not harmonized
  # in pass one. Revisit if accented country names ever appear.
  expect_equal(norm_value('León'), 'len')
  expect_equal(norm_value('Leon'), 'leon')
  expect_false(norm_value('León') == norm_value('Leon'))
})

test_that("known quirk: values differing only in punctuation collide", {
  expect_equal(norm_value('1-2'), norm_value('12'))
  expect_equal(norm_value('S. Africa'), norm_value('S Africa'))
})

test_that("known quirk: all-punctuation values collapse to the empty key", {
  expect_equal(norm_value('-'), '')
  expect_equal(norm_value('--'), '')
  expect_equal(norm_value('???'), '')
  expect_equal(norm_value(''), '')
})

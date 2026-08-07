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

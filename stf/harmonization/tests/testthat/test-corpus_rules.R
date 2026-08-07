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

test_that("spellings normalizing to the same key collapse when the canonical agrees", {
  corpus <- list(corpus = list(
    vietnam = list(source_id = 'GAZ_00003756', type = 'characteristicValue',
                   canonical = 'Vietnam', spellings = list('Vietnam', 'Viet Nam'))
  ))
  v <- corpus_to_vocabulary(corpus)
  # 'Vietnam' and 'Viet Nam' share a normalized key; only one row may survive, or the
  # duplicate-key validator in the rulebook layer rejects our own generated file.
  expect_equal(nrow(v), 1)
  expect_equal(v$canonical_value, 'Vietnam')
  expect_equal(length(unique(norm_value(v$raw_value))), nrow(v))
})

test_that("spellings normalizing to the same key with DIFFERENT canonicals hard-error", {
  corpus <- list(corpus = list(
    a = list(source_id = 'X_1', type = 'characteristicValue',
             canonical = 'Thing One', spellings = list('thing-one')),
    b = list(source_id = 'X_2', type = 'characteristicValue',
             canonical = 'Thing Two', spellings = list('Thing One'))
  ))
  # both normalize to 'thingone' but claim different canonicals: a corpus curation error
  expect_error(corpus_to_vocabulary(corpus), 'different canonical')
})

test_that("the real corpus generates no duplicate normalized keys", {
  v <- corpus_to_vocabulary(read_ontology_corpus(
    file.path(REPO_ROOT, 'stf', 'ontology_corpus.json')))
  expect_equal(length(unique(norm_value(v$raw_value))), nrow(v))
})

test_that("the committed layer-0 file is byte-identical to a fresh generation", {
  # Guards against the generator and its checked-in output silently desyncing.
  committed <- file.path(HARMONIZATION_DIR, 'rules', 'generated',
                         'vocabulary_from_corpus.tsv')
  skip_if_not(file.exists(committed), 'generated layer-0 file not present')
  fresh <- tempfile(fileext = '.tsv')
  write_corpus_vocabulary(file.path(REPO_ROOT, 'stf', 'ontology_corpus.json'), fresh)
  expect_equal(unname(tools::md5sum(fresh)), unname(tools::md5sum(committed)),
               info = paste('Regenerate with write_corpus_vocabulary() and commit the',
                            'result, or the engine runs on a stale rulebook.'))
})

test_that("non-value term types are excluded from the real corpus output", {
  v <- corpus_to_vocabulary(read_ontology_corpus(
    file.path(REPO_ROOT, 'stf', 'ontology_corpus.json')))
  # 'country' (ENVO_00000009) and 'organism' (OBI_0100026) are characteristicQualifier
  # terms describing column HEADERS, not cell values. If either appears here, the filter
  # has regressed and we would rewrite data values using header vocabulary.
  expect_false('ENVO_00000009' %in% v$ontology_term_id)
  expect_false('OBI_0100026' %in% v$ontology_term_id)
})

test_that("a corpus lacking the top-level 'corpus' key errors instead of emitting empty", {
  expect_error(corpus_to_vocabulary(list(notcorpus = list())), 'Malformed ontology corpus')
})

# helper: build a vocabulary tibble inline
vocab <- function(...) {
  rows <- list(...)
  # zero rows: bind_rows(list()) has no columns at all, so hand back the canonical
  # empty-vocabulary shape instead.
  if (length(rows) == 0) return(empty_vocabulary())
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

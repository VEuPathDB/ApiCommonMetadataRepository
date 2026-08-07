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

test_that("a layer-2 wildcard shadowing a layer-1 specific rule IS detected", {
  # Regression: joining on exact variable equality missed this entirely, letting the
  # BROADEST possible local override slip past the warning that exists to catch it.
  l1 <- vocab(vrow('specimen', 'blood', 'L1_SPECIFIC'))
  l2 <- vocab(vrow('*', 'blood', 'L2_WILDCARD'))
  s <- shadowed_rules(l1, l2)
  expect_equal(nrow(s), 1)
  expect_match(s$scope, 'broadens')
  expect_warning(merge_vocabulary_layers(vocab(), l1, l2), 'shadow')
  # and the wildcard really does win, which is why it must be reported
  merged <- suppressWarnings(merge_vocabulary_layers(vocab(), l1, l2))
  expect_equal(resolve_value(merged, 'specimen', 'blood')$canonical_value, 'L2_WILDCARD')
})

test_that("a layer-2 specific rule narrowing a layer-1 wildcard IS detected", {
  s <- shadowed_rules(vocab(vrow('*', 'blood', 'L1_WILDCARD')),
                      vocab(vrow('specimen', 'blood', 'L2_SPECIFIC')))
  expect_equal(nrow(s), 1)
  expect_match(s$scope, 'narrows')
})

test_that("shadow detection is not inflated by duplicate keys within a layer", {
  # 'Blood' and 'blood' share a normalized key. Even unvalidated, the count must be 1.
  l1 <- vocab(vrow('specimen', 'Blood', 'blood'), vrow('specimen', 'blood', 'blood'))
  l2 <- vocab(vrow('specimen', 'blood', 'LOCAL'))
  expect_equal(count_shadowed_rules(l1, l2), 1L)
})

test_that("absent rulebook files load as empty layers, not errors", {
  rb <- load_rulebook(file.path(HARMONIZATION_DIR, 'rules'), 'PlasmoDB', 'pfal3D7')
  expect_equal(nrow(rb$vocabulary), 361)   # real layer 0, layers 1 and 2 absent
  expect_equal(nrow(rb$actions), 0)
  expect_equal(rb$n_shadowed, 0)
  expect_equal(nrow(rb$shadowed), 0)
  expect_equal(resolve_value(rb$vocabulary, 'organism', 'Human')$canonical_value,
               'Homo sapiens')
})

test_that("blank cells become NA but the literal string 'NA' survives", {
  # 'NA' is a plausible country abbreviation, so it must not be read as missing.
  p <- tempfile(fileext = '.tsv')
  readr::write_tsv(tibble(
    variable = c('country', 'country'), raw_value = c('NA', 'X'),
    canonical_value = c('Namibia', '   '), ontology_term_id = c(NA, NA),
    note = c('', NA)), p, na = '')
  v <- read_vocabulary(p)
  expect_false(is.na(v$raw_value[1]))
  expect_equal(v$raw_value[1], 'NA')
  expect_true(is.na(v$canonical_value[2]))   # spaces-only -> NA
  expect_true(is.na(v$note[1]))              # empty -> NA
})

test_that("resolve_value rejects a vector rather than silently using the first element", {
  merged <- merge_vocabulary_layers(vocab(vrow('*', 'human', 'Homo sapiens')),
                                   vocab(), vocab())
  expect_error(resolve_value(merged, 'organism', c('human', 'blood')), 'single value')
})

test_that("a rename action with no new_name and no new_display_name is rejected", {
  a <- tibble(variable = 'organism', action = 'rename', new_name = NA_character_,
              new_display_name = NA_character_, new_parent = NA_character_,
              reason = 'stated')
  expect_error(validate_variable_actions(a, 'global'), 'neither new_name')
})

test_that("a reparent action with no new_parent is rejected", {
  a <- tibble(variable = 'country', action = 'reparent', new_name = NA_character_,
              new_display_name = NA_character_, new_parent = NA_character_,
              reason = 'stated')
  expect_error(validate_variable_actions(a, 'global'), 'no new_parent')
})

test_that("a rename with only new_display_name is valid (display-name-only change)", {
  a <- tibble(variable = 'organism', action = 'rename', new_name = NA_character_,
              new_display_name = 'Host organism', new_parent = NA_character_,
              reason = 'label only')
  expect_silent(validate_variable_actions(a, 'global'))
})

tiny_entity <- function() {
  get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
}

tiny_vocab <- function() {
  merge_vocabulary_layers(
    layer0 = tibble(variable = '*', raw_value = c('Homo sapiens', 'human'),
                    canonical_value = 'Homo sapiens',
                    ontology_term_id = 'NCBITaxon_9606', note = NA_character_),
    layer1 = tibble(variable = 'specimen', raw_value = 'Blood',
                    canonical_value = 'blood',
                    ontology_term_id = 'OBI_0000655', note = NA_character_),
    layer2 = empty_vocabulary())
}

test_that("apply_value_rules collapses all spellings to the canonical value", {
  res <- apply_value_rules(tiny_entity(), tiny_vocab())
  d <- get_data(res$entity)
  expect_equal(sum(d$organism == 'Homo sapiens', na.rm = TRUE), 5)
  expect_equal(sum(is.na(d$organism)), 1)
  expect_setequal(na.omit(unique(d$organism)), 'Homo sapiens')
})

test_that("apply_value_rules preserves column class and keeps the entity valid", {
  e <- tiny_entity()
  before <- class(get_data(e)$organism)
  res <- apply_value_rules(e, tiny_vocab())
  expect_equal(class(get_data(res$entity)$organism), before)
  expect_equal(class(get_data(res$entity)$organism), 'character')
  expect_true(validate(quiet(res$entity)))
})

test_that("apply_value_rules logs one row per (variable, raw_value) with counts and layer", {
  res <- apply_value_rules(tiny_entity(), tiny_vocab())
  log <- res$changes
  expect_setequal(colnames(log),
    c('variable', 'raw_value', 'canonical_value', 'rows_affected', 'layer',
      'ontology_term_id'))
  human_row <- log %>% filter(variable == 'organism', raw_value == 'human')
  expect_equal(human_row$rows_affected, 2L)
  expect_equal(human_row$layer, 0L)
  expect_equal(human_row$ontology_term_id, 'NCBITaxon_9606')
  # 'Homo sapiens' -> 'Homo sapiens' is a no-op and must NOT be logged
  expect_equal(nrow(log %>% filter(raw_value == 'Homo sapiens')), 0)
})

test_that("values with no rule pass through untouched", {
  res <- apply_value_rules(tiny_entity(), tiny_vocab())
  d <- get_data(res$entity)
  expect_true('culture' %in% d$specimen)
  expect_true('PLU' %in% d$country)
})

test_that("the ID column is never harmonized", {
  vocab <- merge_vocabulary_layers(
    layer0 = tibble(variable = '*', raw_value = 's1', canonical_value = 'MANGLED',
                    ontology_term_id = NA_character_, note = NA_character_),
    layer1 = empty_vocabulary(), layer2 = empty_vocabulary())
  res <- apply_value_rules(tiny_entity(), vocab)
  expect_true('s1' %in% get_data(res$entity)$sample_id)
  expect_equal(nrow(res$changes), 0)
})

test_that("an empty rulebook is an exact no-op", {
  e <- tiny_entity()
  res <- apply_value_rules(e, merge_vocabulary_layers(
    empty_vocabulary(), empty_vocabulary(), empty_vocabulary()))
  expect_equal(get_data(res$entity), get_data(e))
  expect_equal(nrow(res$changes), 0)
})

test_that("unresolved_values lists every value with no rule, with counts", {
  u <- unresolved_values(tiny_entity(), tiny_vocab())
  expect_setequal(colnames(u), c('variable', 'raw_value', 'rows_affected'))
  expect_true(nrow(u %>% filter(variable == 'country', raw_value == 'PLU')) == 1)
  # 'Blood' has a rule, so it must not appear
  expect_equal(nrow(u %>% filter(variable == 'specimen', raw_value == 'Blood')), 0)
  # ID column excluded
  expect_equal(nrow(u %>% filter(variable == 'sample_id')), 0)
})

test_that("a blank canonical_value is rejected rather than erasing data", {
  # Regression: a blank cell in a hand-authored TSV used to fire as a rule and NULL the
  # cell. Data deletion must never be an accident of an empty spreadsheet cell.
  expect_error(
    validate_vocabulary_layer(
      tibble(variable = 'specimen', raw_value = 'culture',
             canonical_value = NA_character_, ontology_term_id = NA_character_,
             note = NA_character_), 'global'),
    'Blank canonical_value')
})

test_that("identifier and date-like variables are excluded from harmonization", {
  expect_setequal(harmonizable_variables(tiny_entity()),
                  c('organism', 'specimen', 'country'))
})

test_that("an all-NA column is handled without error", {
  e <- tiny_entity() %>% modify_data(mutate(country = NA_character_))
  res <- apply_value_rules(e, tiny_vocab())
  expect_true(validate(quiet(res$entity)))
  expect_equal(nrow(res$changes %>% filter(variable == 'country')), 0)
  expect_equal(nrow(unresolved_values(e, tiny_vocab()) %>%
                      filter(variable == 'country')), 0)
})

test_that("rows_affected is NA-safe: a column with NAs still counts correctly", {
  # organism has one NA; without na.rm the sum would be NA for every affected value.
  res <- apply_value_rules(tiny_entity(), tiny_vocab())
  expect_false(any(is.na(res$changes$rows_affected)))
  expect_equal(res$changes %>% filter(raw_value == 'Human') %>% pull(rows_affected), 1L)
})

test_that("non-categorical columns are never touched", {
  # The whole rationale for harmonizable_variables() is that substituting into a date or
  # numeric column would corrupt it. The fixture has no such column, so build one here.
  e <- tiny_entity() %>%
    modify_data(mutate(body_temperature = c(37.1, 38.0, 37.1, 39.2, 37.1, 38.0))) %>%
    sync_variable_metadata()
  v <- merge_vocabulary_layers(
    layer0 = tibble(variable = '*', raw_value = '37.1', canonical_value = 'MANGLED',
                    ontology_term_id = NA_character_, note = NA_character_),
    layer1 = empty_vocabulary(), layer2 = empty_vocabulary())
  res <- apply_value_rules(e, v)
  expect_equal(class(get_data(res$entity)$body_temperature), 'numeric')
  expect_equal(get_data(res$entity)$body_temperature, get_data(e)$body_temperature)
  expect_equal(nrow(res$changes %>% filter(variable == 'body_temperature')), 0)
})

test_that("an entity with zero harmonizable variables returns empty results, not an error", {
  # Regression: TriTrypDB/cfasCfCl has only dataset_id and sample_display_name, both
  # excluded, so vars is character(0) and arrange() died on a 0x0 tibble.
  e <- tiny_entity() %>% modify_data(select(sample_id)) %>% sync_variable_metadata()
  expect_equal(harmonizable_variables(e), character(0))
  u <- unresolved_values(e, tiny_vocab())
  expect_equal(nrow(u), 0)
  expect_setequal(colnames(u), c('variable', 'raw_value', 'rows_affected'))
  res <- apply_value_rules(e, tiny_vocab())
  expect_equal(nrow(res$changes), 0)
  expect_true(validate(quiet(res$entity)))
})

test_that("an excluded variable is absent from both substitution and the queue", {
  # The previous exclusion test was a tautology: it asserted a literal was in the literal
  # it was copied from. Assert the FILTER, on an entity that actually has the variable.
  e <- tiny_entity() %>%
    modify_data(mutate(dataset_id = 'Blood')) %>%   # value that WOULD match a rule
    sync_variable_metadata()
  expect_false('dataset_id' %in% harmonizable_variables(e, NON_VOCABULARY_VARIABLES))
  res <- apply_value_rules(e, tiny_vocab(), NON_VOCABULARY_VARIABLES)
  expect_equal(get_data(res$entity)$dataset_id, rep('Blood', 6))   # untouched
  expect_equal(nrow(unresolved_values(e, tiny_vocab(), NON_VOCABULARY_VARIABLES) %>%
                      filter(variable == 'dataset_id')), 0)
})

test_that("unresolved_values is ordered by rows_affected descending", {
  u <- unresolved_values(tiny_entity(), tiny_vocab())
  expect_equal(u$rows_affected, sort(u$rows_affected, decreasing = TRUE))
})

test_that("a blank raw_value is rejected", {
  expect_error(
    validate_vocabulary_layer(
      tibble(variable = 'specimen', raw_value = NA_character_,
             canonical_value = 'blood', ontology_term_id = NA_character_,
             note = NA_character_), 'global'),
    'Blank raw_value')
})

test_that("a study-layer substitution is logged with layer 2", {
  v <- merge_vocabulary_layers(
    empty_vocabulary(), empty_vocabulary(),
    tibble(variable = 'country', raw_value = 'PLU', canonical_value = 'Thailand',
           ontology_term_id = NA_character_, note = NA_character_))
  res <- apply_value_rules(tiny_entity(), v)
  expect_equal(res$changes %>% filter(raw_value == 'PLU') %>% pull(layer), 2L)
})

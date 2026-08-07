tiny_entity2 <- function() {
  get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
}

acts <- function(...) {
  bind_rows(list(...)) %>% mutate(layer = 1L)
}
arow <- function(variable, action, new_name = NA_character_,
                 new_display_name = NA_character_, new_parent = NA_character_,
                 reason = 'test') {
  tibble(variable = variable, action = action, new_name = new_name,
         new_display_name = new_display_name, new_parent = new_parent, reason = reason)
}

test_that("rename changes the column and preserves all metadata", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_name = 'host_organism',
              new_display_name = 'Host organism',
              reason = 'parented to host but named as organism under investigation')))
  e <- res$entity
  md <- get_variable_metadata(e) %>% filter(variable == 'host_organism')

  expect_true('host_organism' %in% colnames(get_data(e)))
  expect_false('organism' %in% colnames(get_data(e)))
  expect_equal(md$display_name, 'Host organism')
  expect_equal(md$definition, 'organism (OBI_0100026)')     # survived (trap 1)
  expect_equal(md$parent_variable, 'host')
  expect_equal(as.character(md$data_type), 'string')
  expect_equal(as.character(md$data_shape), 'categorical')
  expect_true(validate(quiet(e)))
})

test_that("rename preserves the ORIGINAL provider_label as provenance", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_name = 'host_organism', reason = 'x')))
  md <- get_variable_metadata(res$entity) %>% filter(variable == 'host_organism')
  # sync_variable_metadata() would set this to 'host_organism', which is false:
  # the provider called it 'organism'. (trap 2)
  expect_equal(unlist(md$provider_label), 'organism')
})

test_that("a display-name-only action leaves the column name alone", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_display_name = 'Host organism',
              reason = 'label only')))
  e <- res$entity
  expect_true('organism' %in% colnames(get_data(e)))
  expect_equal(get_variable_metadata(e) %>% filter(variable == 'organism') %>%
                 pull(display_name), 'Host organism')
  expect_true(validate(quiet(e)))
})

test_that("reparent moves a variable to another existing category", {
  e0 <- tiny_entity2()
  res <- apply_variable_actions(e0,
    acts(arow('country', 'reparent', new_parent = 'host',
              reason = 'fixture only has one category')))
  expect_equal(get_variable_metadata(res$entity) %>% filter(variable == 'country') %>%
                 pull(parent_variable), 'host')
  expect_true(validate(quiet(res$entity)))
})

test_that("reparent to a non-existent category is a hard error", {
  expect_error(
    apply_variable_actions(tiny_entity2(),
      acts(arow('country', 'reparent', new_parent = 'nonexistent_category',
                reason = 'x'))),
    'nonexistent_category')
})

test_that("dropping a populated interpretable variable is refused (spec 4.2)", {
  expect_error(
    apply_variable_actions(tiny_entity2(),
      acts(arow('country', 'drop', reason = 'I just do not like it'))),
    'populated')
})

test_that("dropping an all-null variable is allowed", {
  e <- tiny_entity2() %>% modify_data(mutate(country = NA_character_))
  res <- apply_variable_actions(e, acts(arow('country', 'drop', reason = 'all null')))
  expect_false('country' %in% colnames(get_data(res$entity)))
  expect_equal(nrow(get_variable_metadata(res$entity) %>% filter(variable == 'country')), 0)
  expect_true(validate(quiet(res$entity)))
})

test_that("dropping a populated variable on the artifact allowlist is allowed", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('country', 'drop', reason = 'header-parsing artifact')),
    artifact_allowlist = c('country'))
  expect_false('country' %in% colnames(get_data(res$entity)))
})

test_that("an action for a variable absent from this study is a no-op, and is counted", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('fungal_strain', 'drop', reason = 'not in this study')))
  expect_equal(res$not_applicable, 1L)
  expect_equal(nrow(res$applied), 0)
  expect_true(validate(quiet(res$entity)))
})

test_that("applied actions are logged with their reason", {
  res <- apply_variable_actions(tiny_entity2(),
    acts(arow('organism', 'rename', new_name = 'host_organism',
              new_display_name = 'Host organism', reason = 'because spec')))
  expect_setequal(colnames(res$applied),
    c('variable', 'action', 'new_name', 'new_display_name', 'new_parent', 'reason', 'layer'))
  expect_equal(res$applied$reason, 'because spec')
})

tiny_entity3 <- function() {
  get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
}

test_that("country pollution is flagged for comma values and short uppercase codes", {
  f <- structural_findings(tiny_entity3())
  cp <- f %>% filter(check == 'country_pollution')
  expect_true('Wang Pha, Thailand' %in% cp$value)   # comma => sub-national leak
  expect_true('PLU' %in% cp$value)                  # short all-caps => site code
  expect_false('Senegal' %in% cp$value)             # clean country
  expect_false('Thailand' %in% cp$value)
})

test_that("a clean country column produces no country findings", {
  e <- tiny_entity3() %>% modify_data(mutate(country = 'Senegal'))
  f <- structural_findings(e)
  expect_equal(nrow(f %>% filter(check == 'country_pollution')), 0)
})

test_that("identifier-like variables are flagged", {
  # 6 distinct values over 6 rows = 100% distinct
  e <- tiny_entity3() %>%
    modify_data(mutate(country = c('a', 'b', 'c', 'd', 'e', 'f')))
  f <- structural_findings(e)
  expect_true('country' %in% (f %>% filter(check == 'identifier_like') %>% pull(variable)))
})

test_that("all-null variables are flagged", {
  e <- tiny_entity3() %>% modify_data(mutate(country = NA_character_))
  f <- structural_findings(e)
  expect_true('country' %in% (f %>% filter(check == 'all_null') %>% pull(variable)))
})

test_that("age unit variants are flagged when an age variable exists", {
  f <- structural_findings_for_values(
    tibble(variable = 'age', value = c('29 years', '28 year', '4 weeks', '30')))
  au <- f %>% filter(check == 'age_units')
  expect_setequal(au$value, c('29 years', '28 year', '4 weeks', '30'))
})

test_that("date ranges and datetimes in collection_date_original are flagged", {
  f <- structural_findings_for_values(
    tibble(variable = 'collection_date_original',
           value = c('2008', '1999-2002', '2014-06-23 00:00:00')))
  dr <- f %>% filter(check == 'date_original_not_a_date')
  expect_setequal(dr$value, c('1999-2002', '2014-06-23 00:00:00'))
})

test_that("the uninformative OBI parent class value is flagged in specimen", {
  f <- structural_findings_for_values(
    tibble(variable = 'specimen', value = c('blood', 'specimen from organism')))
  expect_equal(f %>% filter(check == 'uninformative_value') %>% pull(value),
               'specimen from organism')
})

test_that("structural_findings has a stable schema", {
  f <- structural_findings(tiny_entity3())
  expect_setequal(colnames(f),
    c('check', 'variable', 'value', 'rows_affected', 'detail'))
})

test_that("an entity with only an ID column produces no findings and does not error", {
  e <- tiny_entity3() %>% modify_data(select(sample_id)) %>% sync_variable_metadata()
  f <- structural_findings(e)
  expect_equal(nrow(f), 0)
  expect_setequal(colnames(f), c('check', 'variable', 'value', 'rows_affected', 'detail'))
})

tiny_entity3 <- function() {
  get_entity(study_from_stf(file.path(getwd(), 'fixtures', 'tiny')), 'sample')
}

test_that("country pollution is flagged for comma values only", {
  f <- structural_findings(tiny_entity3())
  cp <- f %>% filter(check == 'country_pollution')
  expect_true('Wang Pha, Thailand' %in% cp$value)   # comma => sub-national leak
  expect_false('PLU' %in% cp$value)                 # a name defect, not structural
  expect_false('Senegal' %in% cp$value)             # clean country
  expect_false('Thailand' %in% cp$value)
})

test_that("short all-caps country tokens are a pass-one name defect, not pollution", {
  e <- tiny_entity3() %>%
    modify_data(mutate(country = c('USA', 'USA', 'PLU', 'Senegal', 'Senegal',
                                   'Wang Pha, Thailand')))
  f <- structural_findings(e)
  nc <- f %>% filter(check == 'country_noncanonical')
  expect_setequal(nc$value, c('USA', 'PLU'))
  expect_true(all(grepl('PASS ONE', nc$detail)))
  # ...and they must NOT also appear under the structural check
  cp <- f %>% filter(check == 'country_pollution')
  expect_equal(cp$value, 'Wang Pha, Thailand')
})

test_that("country_pollution reports the collection_site cross-check either way", {
  no_site <- structural_findings_for_values(
    tibble(variable = 'country', value = 'Wang Pha, Thailand'))
  expect_match(no_site$detail, 'no collection_site column', fixed = TRUE)

  corroborated <- structural_findings_for_values(
    tibble(variable = 'country', value = 'Wang Pha, Thailand'),
    collection_sites = c('Wang Pha', 'Maela'))
  expect_match(corroborated$detail, 'corroborated', fixed = TRUE)

  missing <- structural_findings_for_values(
    tibble(variable = 'country', value = 'Wang Pha, Thailand'),
    collection_sites = c('Maela'))
  expect_match(missing$detail, 'not found in collection_site', fixed = TRUE)
})

test_that("a clean country column produces no country findings", {
  e <- tiny_entity3() %>% modify_data(mutate(country = 'Senegal'))
  f <- structural_findings(e)
  expect_equal(nrow(f %>% filter(check == 'country_pollution')), 0)
  expect_equal(nrow(f %>% filter(check == 'country_noncanonical')), 0)
})

test_that("identifier-like variables are flagged", {
  # 12 distinct values over 12 rows = 100% distinct, clearing the n>=10 floor
  e <- tiny_entity3() %>%
    modify_data(slice(rep(1:6, 2))) %>%
    modify_data(mutate(sample_id = as.character(1:12),
                       country = letters[1:12])) %>%
    sync_variable_metadata()
  f <- structural_findings(e)
  expect_true('country' %in% (f %>% filter(check == 'identifier_like') %>% pull(variable)))
})

test_that("a fully distinct but tiny column is arithmetic, not an identifier", {
  # 3 distinct over 3 non-null rows: 100% distinct, but below the n>=10 floor
  e <- tiny_entity3() %>%
    modify_data(mutate(country = c('a', 'b', 'c', NA, NA, NA)))
  f <- structural_findings(e)
  expect_false('country' %in% (f %>% filter(check == 'identifier_like') %>% pull(variable)))
})

test_that("non_vocabulary variables are exempt from identifier_like but not from other checks", {
  e <- tiny_entity3() %>%
    modify_data(slice(rep(1:6, 2))) %>%
    modify_data(mutate(sample_id = as.character(1:12),
                       country = c(letters[1:11], 'USA'))) %>%
    sync_variable_metadata()
  f <- structural_findings(e, non_vocabulary = 'country')
  expect_false('country' %in% (f %>% filter(check == 'identifier_like') %>% pull(variable)))
  # the value-level country check still sees it
  expect_true('USA' %in% (f %>% filter(check == 'country_noncanonical') %>% pull(value)))
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
  # the check is a census by design, and says so
  expect_true(all(grepl('census row', au$detail)))
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

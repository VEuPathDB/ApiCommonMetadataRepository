# Values that are the bare ontology parent class, carrying no information.
UNINFORMATIVE_VALUES <- c('specimen from organism', 'sample from organism',
                          'whole organism', 'organism')

empty_findings <- function() {
  tibble(check = character(), variable = character(), value = character(),
         rows_affected = integer(), detail = character())
}

#' Value-level structural checks, separated so they are testable without an Entity.
#'
#' @param vals tibble(variable, value) - one row per occurrence
structural_findings_for_values <- function(vals) {
  if (nrow(vals) == 0) return(empty_findings())

  counted <- vals %>% filter(!is.na(value)) %>% count(variable, value, name = 'rows_affected')
  if (nrow(counted) == 0) return(empty_findings())

  country <- counted %>%
    filter(variable %in% c('country'),
           grepl(',', value) | (nchar(value) <= 4 & value == toupper(value) &
                                grepl('^[A-Z]+$', value))) %>%
    mutate(check = 'country_pollution',
           detail = if_else(grepl(',', value),
                            'contains a comma: sub-national place merged into country',
                            'short all-caps token: looks like a collection-site code'))

  age <- counted %>%
    filter(variable == 'age') %>%
    mutate(check = 'age_units',
           detail = case_when(
             grepl('week', value, ignore.case = TRUE) ~ 'unit: weeks',
             grepl('year', value, ignore.case = TRUE) ~ 'unit: years',
             TRUE                                     ~ 'no unit token'))

  dates <- counted %>%
    filter(variable == 'collection_date_original',
           grepl('^[0-9]{4}\\s*-\\s*[0-9]{4}$', value) | grepl('[0-9]{2}:[0-9]{2}', value)) %>%
    mutate(check = 'date_original_not_a_date',
           detail = if_else(grepl(':', value), 'datetime, not a date', 'year range'))

  uninformative <- counted %>%
    filter(value %in% UNINFORMATIVE_VALUES) %>%
    mutate(check = 'uninformative_value',
           detail = 'bare ontology parent class; carries no information')

  bind_rows(empty_findings(), country, age, dates, uninformative) %>%
    select(all_of(c('check', 'variable', 'value', 'rows_affected', 'detail')))
}

#' All structural findings for one entity. Read-only: changes nothing.
structural_findings <- function(entity) {
  d <- get_data(entity)
  id_col <- get_entity_id_column(entity)
  vars <- setdiff(colnames(d), id_col)
  if (length(vars) == 0) return(empty_findings())

  vals <- vars %>%
    map(~ tibble(variable = .x, value = as.character(d[[.x]]))) %>%
    bind_rows()

  value_level <- structural_findings_for_values(vals)

  var_level <- vars %>%
    map(function(v) {
      col <- d[[v]]
      n_nonnull <- sum(!is.na(col))
      n_distinct <- length(unique(na.omit(as.character(col))))
      if (n_nonnull == 0) {
        return(tibble(check = 'all_null', variable = v, value = NA_character_,
                      rows_affected = 0L,
                      detail = 'variable is 100% null; carries no information'))
      }
      if (n_distinct >= 0.95 * n_nonnull && n_nonnull > 1) {
        return(tibble(check = 'identifier_like', variable = v, value = NA_character_,
                      rows_affected = as.integer(n_nonnull),
                      detail = glue::glue(
                        '{n_distinct} distinct values over {n_nonnull} non-null rows ',
                        '(>=95%): identifier-like, useless as an EDA facet')))
      }
      NULL
    }) %>%
    compact() %>%
    bind_rows()

  continent <- if ('continent' %in% vars) {
    n_null <- sum(is.na(d$continent))
    if (n_null > 0) {
      tibble(check = 'continent_incomplete', variable = 'continent', value = NA_character_,
             rows_affected = as.integer(n_null),
             detail = glue::glue('{n_null}/{nrow(d)} rows null; continent is derivable ',
                                 'from a clean country and should be computed'))
    } else NULL
  } else NULL

  bind_rows(empty_findings(), value_level, var_level, continent) %>%
    select(all_of(c('check', 'variable', 'value', 'rows_affected', 'detail')))
}

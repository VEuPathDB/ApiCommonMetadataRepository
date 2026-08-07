# Values that are the bare ontology parent class, carrying no information.
UNINFORMATIVE_VALUES <- c('specimen from organism', 'sample from organism',
                          'whole organism', 'organism')

empty_findings <- function() {
  tibble(check = character(), variable = character(), value = character(),
         rows_affected = integer(), detail = character())
}

#' Value-level structural checks, separated so they are testable without an Entity.
#'
#' The two country checks are deliberately separate because they need different fixes in
#' different passes. A short all-caps token is a NAME defect: one vocabulary rule maps it
#' to the canonical country and pass one is done. A comma value is a SCHEMA defect: the
#' sub-national place has to be lifted into its own column, which is pass two. Reporting
#' both under one heading would send 25 studies of one-line vocabulary work into pass two.
#'
#' @param vals tibble(variable, value) - one row per occurrence
#' @param collection_sites character vector of the study's distinct collection_site values,
#'   used only to evidence the country_pollution check. Empty means the study has no
#'   collection_site column, which is reported rather than silently omitted.
structural_findings_for_values <- function(vals, collection_sites = character()) {
  if (nrow(vals) == 0) return(empty_findings())

  counted <- vals %>% filter(!is.na(value)) %>% count(variable, value, name = 'rows_affected')
  if (nrow(counted) == 0) return(empty_findings())

  # Pass-one fix: a non-canonical country NAME, not a structural defect. A short all-caps
  # token is ambiguous by nature -- 'USA' is a country abbreviation, 'MRH' is a site code --
  # and nothing in the value distinguishes them, so both land here for a human to triage.
  country_noncanonical <- counted %>%
    filter(variable == 'country',
           !grepl(',', value),
           nchar(value) <= 4, value == toupper(value), grepl('^[A-Z]+$', value)) %>%
    mutate(check = 'country_noncanonical',
           detail = paste('short all-caps token: either a country abbreviation or a',
                          'collection-site code -- needs a human look. PASS ONE fix: add',
                          'a vocabulary rule mapping it to the canonical country name.',
                          'Not structural.'))

  # Pass-two fix: two facts merged into one column. The cross-check against the study's own
  # collection_site column is the evidence that splitting is the right fix, so it is
  # reported inline rather than left for a reader to reconstruct.
  site_keys <- norm_value(collection_sites)
  site_keys <- site_keys[!is.na(site_keys) & site_keys != '']
  country_pollution <- counted %>%
    filter(variable == 'country', grepl(',', value)) %>%
    mutate(pre_comma = trimws(sub(',.*$', '', value)),
           site_evidence = if (length(site_keys) == 0) {
             'no collection_site column in this study: cannot corroborate'
           } else {
             if_else(norm_value(pre_comma) %in% site_keys,
                     paste0("pre-comma token '", pre_comma,
                            "' also appears in collection_site: split is corroborated"),
                     paste0("pre-comma token '", pre_comma,
                            "' not found in collection_site: near-miss or unlisted site"))
           },
           check = 'country_pollution',
           detail = paste0('contains a comma: sub-national place merged into country; ',
                           site_evidence, '. PASS TWO fix: split into country + site.')) %>%
    select(-pre_comma, -site_evidence)

  # Deliberately a census, not a defect list: the supplied contract flags every age value,
  # unit-bearing or not, so the report doubles as design input for a pass-two age parser.
  age <- counted %>%
    filter(variable == 'age') %>%
    mutate(check = 'age_units',
           detail = case_when(
             grepl('week', value, ignore.case = TRUE) ~
               'unit-bearing (weeks); census row, not a defect on its own',
             grepl('year', value, ignore.case = TRUE) ~
               'unit-bearing (years); census row, not a defect on its own',
             TRUE ~
               'no unit token; census row, not a defect on its own'))

  dates <- counted %>%
    filter(variable == 'collection_date_original',
           grepl('^[0-9]{4}\\s*-\\s*[0-9]{4}$', value) | grepl('[0-9]{2}:[0-9]{2}', value)) %>%
    mutate(check = 'date_original_not_a_date',
           detail = if_else(grepl(':', value), 'datetime, not a date', 'year range'))

  uninformative <- counted %>%
    filter(value %in% UNINFORMATIVE_VALUES) %>%
    mutate(check = 'uninformative_value',
           detail = 'bare ontology parent class; carries no information')

  bind_rows(empty_findings(), country_noncanonical, country_pollution, age, dates,
            uninformative) %>%
    select(all_of(c('check', 'variable', 'value', 'rows_affected', 'detail')))
}

# An identifier-like column needs enough rows for high distinctness to mean anything. At
# n=2 a fully distinct column is arithmetic, not a defect; 10 is the smallest floor that
# clears the tiny studies without hiding real identifiers.
IDENTIFIER_LIKE_MIN_ROWS <- 10L
IDENTIFIER_LIKE_DISTINCT_FRACTION <- 0.95

#' All structural findings for one entity. Read-only: changes nothing.
#'
#' @param non_vocabulary character vector of known identifier/date/measurement variables,
#'   normally the merged list from load_rulebook(). Excluded from the identifier_like check
#'   ONLY -- flagging them there restates the input. Every other check still sees them.
structural_findings <- function(entity, non_vocabulary = character()) {
  d <- get_data(entity)
  id_col <- get_entity_id_column(entity)
  vars <- setdiff(colnames(d), id_col)
  if (length(vars) == 0) return(empty_findings())

  vals <- vars %>%
    map(~ tibble(variable = .x, value = as.character(d[[.x]]))) %>%
    bind_rows()

  collection_sites <- if ('collection_site' %in% vars) {
    unique(na.omit(as.character(d$collection_site)))
  } else {
    character()
  }

  value_level <- structural_findings_for_values(vals, collection_sites = collection_sites)

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
      # Declared identifiers, dates and measurements are expected to be near-unique;
      # reporting them would restate non_vocabulary.tsv back to the reader.
      if (v %in% non_vocabulary) return(NULL)
      if (n_distinct >= IDENTIFIER_LIKE_DISTINCT_FRACTION * n_nonnull &&
          n_nonnull >= IDENTIFIER_LIKE_MIN_ROWS) {
        return(tibble(check = 'identifier_like', variable = v, value = NA_character_,
                      rows_affected = as.integer(n_nonnull),
                      detail = glue::glue(
                        '{n_distinct} distinct values over {n_nonnull} non-null rows ',
                        '(>={pct}%): identifier-like, useless as an EDA facet. ',
                        'Thresholds: >={pct}% distinct and >={min_n} non-null rows; ',
                        'variables listed in non_vocabulary.tsv are not considered.',
                        pct = IDENTIFIER_LIKE_DISTINCT_FRACTION * 100,
                        min_n = IDENTIFIER_LIKE_MIN_ROWS)))
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

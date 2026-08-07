VARIABLES_COLS <- c('variable', 'action', 'new_name', 'new_display_name',
                    'new_parent', 'reason')
VALID_ACTIONS  <- c('rename', 'reparent', 'drop')

empty_vocabulary <- function() {
  tibble(variable = character(), raw_value = character(),
         canonical_value = character(), ontology_term_id = character(),
         note = character())
}

empty_variable_actions <- function() {
  tibble(variable = character(), action = character(), new_name = character(),
         new_display_name = character(), new_parent = character(), reason = character())
}

#' Read a rules TSV, returning an empty typed tibble when the file is absent.
read_rules_tsv <- function(path, expected_cols, empty_fn) {
  if (!file.exists(path)) return(empty_fn())
  df <- readr::read_tsv(path, col_types = readr::cols(.default = readr::col_character()),
                        na = character())
  missing <- setdiff(expected_cols, colnames(df))
  if (length(missing) > 0) {
    stop(glue::glue("'{path}' is missing required column(s): {toString(missing)}"))
  }
  df %>%
    mutate(across(everything(), ~ na_if(trimws(.x), ''))) %>%
    select(all_of(expected_cols))
}

read_vocabulary <- function(path) read_rules_tsv(path, VOCAB_COLS, empty_vocabulary)
read_variable_actions <- function(path) {
  read_rules_tsv(path, VARIABLES_COLS, empty_variable_actions)
}

#' Reject ambiguous curation inside a single layer.
validate_vocabulary_layer <- function(v, layer_label) {
  if (nrow(v) == 0) return(invisible(v))
  dupes <- v %>%
    mutate(key = norm_value(raw_value)) %>%
    count(variable, key) %>%
    filter(n > 1)
  if (nrow(dupes) > 0) {
    stop(glue::glue(
      "Duplicate rule key(s) in the '{layer_label}' vocabulary layer: ",
      "{paste(dupes$variable, dupes$key, sep='/', collapse=', ')}. ",
      "Ambiguous curation must be resolved, not silently picked."))
  }
  invisible(v)
}

validate_variable_actions <- function(a, layer_label) {
  if (nrow(a) == 0) return(invisible(a))
  bad <- setdiff(a$action, VALID_ACTIONS)
  if (length(bad) > 0) {
    stop(glue::glue("Invalid action(s) in '{layer_label}' variables layer: {toString(bad)}. ",
                    "Valid: {toString(VALID_ACTIONS)}"))
  }
  no_reason <- a %>% filter(is.na(reason) | reason == '')
  if (nrow(no_reason) > 0) {
    stop(glue::glue("Variable action(s) without a reason in '{layer_label}': ",
                    "{toString(no_reason$variable)}. A rename or drop without a stated ",
                    "reason is unreviewable."))
  }
  dupes <- a %>% count(variable) %>% filter(n > 1)
  if (nrow(dupes) > 0) {
    stop(glue::glue("Multiple actions for variable(s) in '{layer_label}': ",
                    "{toString(dupes$variable)}"))
  }
  invisible(a)
}

#' Which layer-2 rules shadow a layer-1 rule?
#'
#' Shadowing means: same normalized key AND overlapping variable scope. Scope overlap
#' includes the wildcard cases -- a layer-2 '*' rule overrides a layer-1 variable-specific
#' rule for that variable, and is the BROADEST possible local override, so it is exactly
#' what this warning must catch. Joining on exact variable equality misses it entirely.
#'
#' @return tibble of shadowed rule pairs, zero rows if none
shadowed_rules <- function(layer1, layer2) {
  empty <- tibble(variable_global = character(), variable_study = character(),
                  key = character(), global = character(), study = character(),
                  scope = character())
  if (nrow(layer1) == 0 || nrow(layer2) == 0) return(empty)

  k1 <- layer1 %>%
    transmute(variable_global = variable, key = norm_value(raw_value),
              global = canonical_value) %>%
    distinct(variable_global, key, .keep_all = TRUE)
  k2 <- layer2 %>%
    transmute(variable_study = variable, key = norm_value(raw_value),
              study = canonical_value) %>%
    distinct(variable_study, key, .keep_all = TRUE)

  dplyr::inner_join(k1, k2, by = 'key', relationship = 'many-to-many') %>%
    filter(variable_global == variable_study |
           variable_global == '*' | variable_study == '*') %>%
    mutate(scope = case_when(
      variable_global == variable_study ~ 'exact',
      variable_study == '*'             ~ 'study wildcard broadens over global specific',
      TRUE                              ~ 'study specific narrows a global wildcard')) %>%
    select(all_of(names(empty)))
}

#' How many layer-2 rules shadow a layer-1 rule? Kept for the summary report.
count_shadowed_rules <- function(layer1, layer2) {
  nrow(shadowed_rules(layer1, layer2))
}

#' Merge the three layers into one lookup keyed (variable, key).
#'
#' Precedence: layer number descending, then specificity (non-wildcard beats '*').
merge_vocabulary_layers <- function(layer0, layer1, layer2) {
  validate_vocabulary_layer(layer0, 'generated')
  validate_vocabulary_layer(layer1, 'global')
  validate_vocabulary_layer(layer2, 'study')

  shadowed <- shadowed_rules(layer1, layer2)
  if (nrow(shadowed) > 0) {
    detail <- shadowed %>%
      mutate(line = glue::glue(
        "  {variable_study}/{key}: global='{global}' study='{study}' [{scope}]")) %>%
      pull(line)
    warning(paste0(
      nrow(shadowed), ' study rule(s) shadow a global rule. Promote to global, or ',
      "confirm the local override is intended:\n", paste(detail, collapse = '\n')),
      call. = FALSE)
  }

  bind_rows(
    layer0 %>% mutate(layer = 0L),
    layer1 %>% mutate(layer = 1L),
    layer2 %>% mutate(layer = 2L)
  ) %>%
    mutate(key = norm_value(raw_value),
           specificity = if_else(variable == '*', 0L, 1L)) %>%
    arrange(desc(layer), desc(specificity)) %>%
    distinct(variable, key, .keep_all = TRUE)
}

#' Look up the winning rule for one (variable, value). Zero rows if none.
resolve_value <- function(merged, variable_name, raw) {
  if (length(raw) != 1) {
    stop(glue::glue('resolve_value() takes a single value, got {length(raw)}. ',
                    'Callers should iterate or use a join.'))
  }
  if (nrow(merged) == 0) return(merged)
  k <- norm_value(raw)
  merged %>%
    filter(key == k, variable %in% c(variable_name, '*')) %>%
    arrange(desc(layer), desc(specificity)) %>%
    head(1)
}

#' Load all three layers for one study.
load_rulebook <- function(rules_dir, project, organism) {
  study_dir <- file.path(rules_dir, 'studies', project, organism)
  layers <- list(
    vocab0 = read_vocabulary(file.path(rules_dir, 'generated',
                                       'vocabulary_from_corpus.tsv')),
    vocab1 = read_vocabulary(file.path(rules_dir, 'global', 'vocabulary.tsv')),
    vocab2 = read_vocabulary(file.path(study_dir, 'vocabulary.tsv')),
    acts1  = read_variable_actions(file.path(rules_dir, 'global', 'variables.tsv')),
    acts2  = read_variable_actions(file.path(study_dir, 'variables.tsv'))
  )
  # Validate up front: shadow accounting below must not depend on merge_vocabulary_layers
  # happening to validate later. Double validation on a 361-row table is free.
  validate_vocabulary_layer(layers$vocab0, 'generated')
  validate_vocabulary_layer(layers$vocab1, 'global')
  validate_vocabulary_layer(layers$vocab2, 'study')
  validate_variable_actions(layers$acts1, 'global')
  validate_variable_actions(layers$acts2, 'study')

  shadowed <- shadowed_rules(layers$vocab1, layers$vocab2)
  merged_vocab <- merge_vocabulary_layers(layers$vocab0, layers$vocab1, layers$vocab2)

  # study actions override global actions for the same variable
  merged_acts <- bind_rows(layers$acts2 %>% mutate(layer = 2L),
                           layers$acts1 %>% mutate(layer = 1L)) %>%
    arrange(desc(layer)) %>%
    distinct(variable, .keep_all = TRUE)

  list(vocabulary = merged_vocab, actions = merged_acts,
       n_shadowed = nrow(shadowed), shadowed = shadowed)
}

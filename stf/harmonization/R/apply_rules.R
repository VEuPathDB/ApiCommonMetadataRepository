#' Which variables are eligible for value harmonization?
#'
#' Categorical/string variables only. Never the ID column, never continuous or date
#' variables (substituting into a date column would corrupt it).
harmonizable_variables <- function(entity) {
  id_col <- get_entity_id_column(entity)
  get_variable_metadata(entity) %>%
    filter(as.character(data_shape) == 'categorical',
           as.character(data_type) %in% c('string', 'category'),
           variable != id_col) %>%
    pull(variable) %>%
    intersect(colnames(get_data(entity)))
}

#' Apply vocabulary rules to an entity.
#'
#' @return list(entity = modified Entity, changes = tibble of substitutions)
apply_value_rules <- function(entity, merged_vocab) {
  vars <- harmonizable_variables(entity)
  changes <- list()

  for (v in vars) {
    col <- get_data(entity)[[v]]
    original_class <- class(col)
    present <- unique(na.omit(as.character(col)))
    if (length(present) == 0) next

    # Build raw -> canonical for the values actually present
    mapping <- present %>%
      map(function(raw) {
        hit <- resolve_value(merged_vocab, v, raw)
        if (nrow(hit) == 0) return(NULL)
        if (identical(hit$canonical_value, raw)) return(NULL)   # no-op, do not log
        tibble(variable = v, raw_value = raw,
               canonical_value = hit$canonical_value,
               rows_affected = sum(as.character(col) == raw, na.rm = TRUE),
               layer = hit$layer,
               ontology_term_id = hit$ontology_term_id)
      }) %>%
      compact() %>%
      bind_rows()

    if (nrow(mapping) == 0) next

    lookup <- setNames(mapping$canonical_value, mapping$raw_value)
    new_col <- as.character(col)
    hit_idx <- !is.na(new_col) & new_col %in% names(lookup)
    new_col[hit_idx] <- unname(lookup[new_col[hit_idx]])

    # TRAP: restore the original column class, or validate() fails because the
    # column class stops matching the declared data_type.
    if (!identical(original_class, 'character')) {
      new_col <- methods::as(new_col, original_class[1])
    }

    # modify_data() splices its `...` into a magrittr pipe inside an S4 method, so the
    # unevaluated verb is re-evaluated in a frame where local names ('v', 'new_col') do
    # not exist. Both must therefore be injected as VALUES -- a symbol and a literal
    # vector -- before the call is handed over, leaving the expression free of any
    # reference to this function's locals.
    entity <- rlang::eval_bare(
      rlang::expr(modify_data(entity, mutate(!!rlang::sym(v) := !!new_col))))
    changes[[length(changes) + 1]] <- mapping
  }

  list(
    entity  = entity,
    changes = if (length(changes) == 0) {
      tibble(variable = character(), raw_value = character(),
             canonical_value = character(), rows_affected = integer(),
             layer = integer(), ontology_term_id = character())
    } else {
      bind_rows(changes) %>% mutate(rows_affected = as.integer(rows_affected))
    }
  )
}

#' Every present value that no layer has a rule for. This is the curation work queue.
unresolved_values <- function(entity, merged_vocab) {
  vars <- harmonizable_variables(entity)
  d <- get_data(entity)

  vars %>%
    map(function(v) {
      col <- as.character(d[[v]])
      tibble(variable = v, raw_value = na.omit(col)) %>%
        count(variable, raw_value, name = 'rows_affected') %>%
        filter(map_lgl(raw_value, ~ nrow(resolve_value(merged_vocab, v, .x)) == 0))
    }) %>%
    bind_rows() %>%
    arrange(desc(rows_affected))
}

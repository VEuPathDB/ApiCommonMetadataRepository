# Default non-vocabulary exclusions.
#
# SOURCE OF TRUTH IS 'rules/global/non_vocabulary.tsv', read by load_rulebook() and passed
# in by callers. This constant is only the fallback for direct callers that supply no list,
# and it is deliberately the narrow original set -- identifiers, free-text provenance, and
# dates/numbers carried as strings. Do not grow it: add rows to the TSV instead, where a
# curator can read the reason and revise it without touching the engine.
#
# Why exclude at all: these are declared string/categorical but are not controlled
# vocabularies. Substituting into an identifier would corrupt the join back to source data,
# and listing them as "unresolved" buries the real curation queue in thousands of unique IDs.
NON_VOCABULARY_VARIABLES <- c(
  'dataset_id', 'sample_display_name', 'alternative_sample_id', 'organism_id',
  'repository_id',
  'collection_date', 'collection_date_original', 'collection_year', 'collection_month',
  'latitude_coordinate_measurement_datum', 'longitude_coordinate_measurement_datum'
)

#' Which variables are eligible for value harmonization?
#'
#' Categorical/string variables only. Never the ID column, never continuous or date
#' variables (substituting into a date column would corrupt it), and never a variable
#' named in the non-vocabulary exclusion list.
#'
#' @param non_vocabulary character vector of variable names to exclude
harmonizable_variables <- function(entity, non_vocabulary = NON_VOCABULARY_VARIABLES) {
  id_col <- get_entity_id_column(entity)
  get_variable_metadata(entity) %>%
    filter(as.character(data_shape) == 'categorical',
           as.character(data_type) %in% c('string', 'category'),
           variable != id_col,
           !variable %in% non_vocabulary) %>%
    pull(variable) %>%
    intersect(colnames(get_data(entity)))
}

#' Apply vocabulary rules to an entity.
#'
#' @param non_vocabulary character vector of variable names to exclude
#' @return list(entity = modified Entity, changes = tibble of substitutions)
apply_value_rules <- function(entity, merged_vocab,
                              non_vocabulary = NON_VOCABULARY_VARIABLES) {
  vars <- harmonizable_variables(entity, non_vocabulary)
  changes <- list()

  # Read the data ONCE. Reading inside the loop would see the progressively-modified
  # entity, which is harmless only while every iteration writes a different column --
  # an invariant nothing enforces and that adding or dropping columns would break.
  d <- get_data(entity)

  for (v in vars) {
    col <- d[[v]]
    original_class <- class(col)
    chr <- as.character(col)
    present <- unique(na.omit(chr))
    if (length(present) == 0) next

    # One pass for all counts, rather than re-scanning the column per distinct value.
    # Single-bracket indexing: counts[['']] raises 'subscript out of bounds', and an
    # empty-string value must not crash the engine.
    counts <- table(chr)

    # Build raw -> canonical for the values actually present
    mapping <- present %>%
      map(function(raw) {
        hit <- resolve_value(merged_vocab, v, raw)
        if (nrow(hit) == 0) return(NULL)
        if (identical(hit$canonical_value, raw)) return(NULL)   # no-op, do not log
        tibble(variable = v, raw_value = raw,
               canonical_value = hit$canonical_value,
               rows_affected = as.integer(unname(counts[raw])),
               layer = hit$layer,
               ontology_term_id = hit$ontology_term_id)
      }) %>%
      compact() %>%
      bind_rows()

    if (nrow(mapping) == 0) next

    lookup <- setNames(mapping$canonical_value, mapping$raw_value)
    new_col <- chr
    hit_idx <- !is.na(new_col) & new_col %in% names(lookup)
    new_col[hit_idx] <- unname(lookup[new_col[hit_idx]])

    # STF categorical/string columns are always character -- validate() enforces it.
    # If that ever stops holding, fail loudly rather than coercing: every coercion
    # available here either errors or silently NAs the data.
    if (!identical(original_class, 'character')) {
      stop(glue::glue("Variable '{v}' is declared categorical/string but its column is ",
                      "'{original_class[1]}'. Harmonizing it would change its type; ",
                      'fix the entity or add the variable to the non-vocabulary list.'))
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

  # Seeding with the shared empty schema makes the zero-substitution case fall out of the
  # same expression as every other case: no parallel schema to drift, and column order and
  # types are fixed by empty_changes() alone.
  list(entity = entity,
       changes = bind_rows(empty_changes(), !!!changes) %>%
         mutate(rows_affected = as.integer(rows_affected)))
}

#' Every present value that no layer has a rule for. This is the curation work queue.
#'
#' @param non_vocabulary character vector of variable names to exclude
unresolved_values <- function(entity, merged_vocab,
                              non_vocabulary = NON_VOCABULARY_VARIABLES) {
  vars <- harmonizable_variables(entity, non_vocabulary)
  d <- get_data(entity)

  per_var <- vars %>%
    map(function(v) {
      col <- as.character(d[[v]])
      tibble(variable = v, raw_value = na.omit(col)) %>%
        count(variable, raw_value, name = 'rows_affected') %>%
        filter(map_lgl(raw_value, ~ nrow(resolve_value(merged_vocab, v, .x)) == 0))
    })

  # Seed the bind: an entity whose every variable is excluded yields zero tibbles, and
  # bind_rows(list()) returns a 0x0 tibble whose missing 'rows_affected' column made the
  # arrange() below throw. Real studies hit this (TriTrypDB/cfasCfCl).
  bind_rows(empty_unresolved(), !!!per_var) %>%
    arrange(desc(rows_affected))
}

#' Rename a variable: column + metadata, preserving provenance.
#'
#' Three steps are mandatory. modify_data(rename()) alone leaves the entity invalid
#' (metadata refers to a column that no longer exists); sync_variable_metadata() fixes
#' validity but WIPES display_name/definition/parent_variable and rewrites provider_label
#' to the new name. So: capture first, rename, sync, re-apply.
rename_variable <- function(entity, old_name, new_name, new_display_name = NA_character_) {
  old <- get_variable_metadata(entity) %>% filter(variable == old_name)
  if (nrow(old) != 1) stop(glue::glue("Expected exactly one metadata row for '{old_name}'"))

  display <- if (!is.na(new_display_name)) new_display_name else old$display_name

  entity <- rlang::eval_bare(rlang::expr(
    modify_data(entity, rename(!!rlang::sym(new_name) := !!rlang::sym(old_name)))))

  entity %>%
    sync_variable_metadata() %>%
    set_variable_metadata(new_name,
      display_name    = display,
      definition      = old$definition,
      parent_variable = old$parent_variable,
      data_type       = as.character(old$data_type),
      data_shape      = as.character(old$data_shape),
      # provider_label is PROVENANCE: keep what the provider called it, not the new name.
      provider_label  = as.list(unlist(old$provider_label)))
}

#' Apply variable rename/reparent/drop actions.
#'
#' @param artifact_allowlist variables that may be dropped despite being populated,
#'        because they are header-parsing artifacts with no interpretable meaning.
#' @return list(entity, applied = tibble of actions applied, not_applicable = integer)
apply_variable_actions <- function(entity, actions, artifact_allowlist = character()) {
  applied <- list()
  not_applicable <- 0L

  if (nrow(actions) == 0) {
    return(list(entity = entity, applied = empty_applied_actions(), not_applicable = 0L))
  }

  categories <- get_category_metadata(entity)$variable

  for (i in seq_len(nrow(actions))) {
    a <- actions[i, ]
    if (!(a$variable %in% colnames(get_data(entity)))) {
      not_applicable <- not_applicable + 1L
      next
    }

    if (a$action == 'drop') {
      col <- get_data(entity)[[a$variable]]
      n_populated <- sum(!is.na(col))
      if (n_populated > 0 && !(a$variable %in% artifact_allowlist)) {
        stop(glue::glue(
          "Refusing to drop '{a$variable}': it is populated ({n_populated} non-null ",
          "rows) and is not on the header-artifact allowlist. Dropping a populated, ",
          "interpretable variable is a structural change (spec section 4.2) and belongs ",
          "in pass two. It is reported in structural_findings.md instead."))
      }
      nm <- a$variable
      entity <- rlang::eval_bare(rlang::expr(
        modify_data(entity, select(-!!rlang::sym(nm))))) %>%
        sync_variable_metadata()

    } else if (a$action == 'reparent') {
      if (is.na(a$new_parent)) {
        stop(glue::glue("Action 'reparent' for '{a$variable}' has no new_parent"))
      }
      if (!(a$new_parent %in% categories)) {
        stop(glue::glue(
          "Cannot reparent '{a$variable}' to '{a$new_parent}': no such category in this ",
          "entity. Existing categories: {toString(categories)}"))
      }
      entity <- entity %>%
        set_variable_metadata(a$variable, parent_variable = a$new_parent)

    } else if (a$action == 'rename') {
      if (!is.na(a$new_name)) {
        entity <- rename_variable(entity, a$variable, a$new_name, a$new_display_name)
      } else if (!is.na(a$new_display_name)) {
        entity <- entity %>%
          set_variable_metadata(a$variable, display_name = a$new_display_name)
      } else {
        stop(glue::glue(
          "Action 'rename' for '{a$variable}' supplies neither new_name nor ",
          "new_display_name"))
      }
    }

    applied[[length(applied) + 1]] <- a
  }

  list(
    entity = entity,
    applied = bind_rows(empty_applied_actions(), !!!applied),
    not_applicable = not_applicable
  )
}

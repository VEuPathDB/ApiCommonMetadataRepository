VOCAB_COLS <- c('variable', 'raw_value', 'canonical_value', 'ontology_term_id', 'note')

#' Read the ontology corpus JSON.
read_ontology_corpus <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Build the layer-0 vocabulary from the ontology corpus.
#'
#' Emits one rule per (spelling -> canonical) for every characteristicValue term.
#' Scoped to variable '*' because these are value vocabularies that apply wherever the
#' value appears (organism, host_common_name and lab_host share a host-species vocabulary).
corpus_to_vocabulary <- function(corpus) {
  terms <- corpus$corpus %>% keep(~ identical(.x$type, 'characteristicValue'))

  if (length(terms) == 0) {
    return(tibble(variable = character(), raw_value = character(),
                  canonical_value = character(), ontology_term_id = character(),
                  note = character()))
  }

  all_rules <- terms %>%
    imap(function(term, key) {
      spellings <- unique(c(unlist(term$spellings), term$canonical))
      tibble(
        variable         = '*',
        raw_value        = spellings,
        canonical_value  = term$canonical,
        ontology_term_id = term$source_id %||% NA_character_,
        note             = 'generated from ontology_corpus.json'
      )
    }) %>%
    bind_rows()

  # The engine matches on the NORMALIZED key, so two spellings that normalize alike are
  # redundant and must not both survive -- a later validator rejects duplicate normalized
  # keys. Collapsing them is lossless ONLY when they agree on the canonical value.
  # Disagreement is a curation error in the corpus and must surface, not be papered over.
  conflicts <- all_rules %>%
    mutate(key = norm_value(raw_value)) %>%
    group_by(variable, key) %>%
    filter(n_distinct(canonical_value) > 1) %>%
    ungroup()

  if (nrow(conflicts) > 0) {
    detail <- conflicts %>%
      group_by(key) %>%
      summarise(canonicals = paste(sort(unique(canonical_value)), collapse = ' vs '),
                spellings = paste(sort(unique(raw_value)), collapse = ', '),
                .groups = 'drop') %>%
      mutate(line = glue::glue("  key '{key}': {canonicals}  (from: {spellings})")) %>%
      pull(line)
    stop(paste0(
      'ontology_corpus.json has spellings that normalize to the same key but map to ',
      'different canonical values. Fix the corpus; this cannot be resolved here:\n',
      paste(detail, collapse = '\n')))
  }

  all_rules %>%
    mutate(key = norm_value(raw_value)) %>%
    distinct(variable, key, .keep_all = TRUE) %>%
    select(all_of(VOCAB_COLS))
}

#' Write layer 0 to disk.
write_corpus_vocabulary <- function(corpus_path, out_path) {
  v <- corpus_to_vocabulary(read_ontology_corpus(corpus_path))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(v, out_path, na = '')
  v
}

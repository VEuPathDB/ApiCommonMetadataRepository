#!/usr/bin/env Rscript
## Harmonize STF studies: collapse inconsistent categorical values and fix variable
## naming, driven entirely by declarative TSV rulebooks.
##
## Inputs are NEVER modified.
## See docs/superpowers/specs/2026-08-07-stf-harmonization-design.md
##
## usage:  Rscript stf/harmonization/harmonize_stf.R [Project/organism ...]
##         (no args = all studies)

if (!exists('HARMONIZATION_BOOTSTRAPPED')) {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(glue)
  })
  .this_file <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))
  HARMONIZATION_DIR <- if (length(.this_file) > 0) {
    dirname(normalizePath(.this_file))
  } else {
    normalizePath('stf/harmonization')
  }
  REPO_ROOT <- normalizePath(file.path(HARMONIZATION_DIR, '..', '..'))
  STUDY_WRANGLER_DIR <- Sys.getenv('STUDY_WRANGLER_DIR')
  if (!nzchar(STUDY_WRANGLER_DIR)) stop('Set STUDY_WRANGLER_DIR to a study-wrangler checkout.')
  suppressPackageStartupMessages(
    devtools::load_all(STUDY_WRANGLER_DIR, quiet = TRUE))
  for (f in list.files(file.path(HARMONIZATION_DIR, 'R'), pattern = '\\.R$',
                       full.names = TRUE)) source(f)
  HARMONIZATION_BOOTSTRAPPED <- TRUE
}

STUDIES_ROOT    <- file.path(REPO_ROOT, 'stf', 'studies')
RULES_DIR       <- file.path(HARMONIZATION_DIR, 'rules')
OUT_STF_ROOT    <- file.path(REPO_ROOT, 'stf', 'studies_harmonized')
OUT_REPORT_ROOT <- file.path(REPO_ROOT, 'stf', 'reports')

# Populated, interpretable variables may NOT be dropped in pass one (spec 4.2). The
# exceptions -- header-parsing artifacts, plus anything the data owner has explicitly
# authorised -- live in rules/global/approved_drops.tsv with a stated reason, so the
# authorisation is reviewable data rather than a constant in this file.

#' Find project/organism study directories.
#' @param only NULL for all, or a character vector of "Project/organism" selectors.
discover_studies <- function(studies_root, only = NULL) {
  yamls <- list.files(studies_root, pattern = '^study\\.yaml$',
                      recursive = TRUE, full.names = TRUE)
  if (length(yamls) == 0) {
    return(tibble(project = character(), organism = character(), dir = character()))
  }
  out <- tibble(dir = dirname(yamls)) %>%
    mutate(organism = basename(dir), project = basename(dirname(dir))) %>%
    select(project, organism, dir) %>%
    arrange(project, organism)
  if (!is.null(only)) {
    out <- out %>% filter(paste(project, organism, sep = '/') %in% only |
                          organism %in% only)
  }
  out
}

#' Harmonize exactly one study directory.
#'
#' Never throws for per-study problems: returns status 'failed' with a message so a
#' 62-study run continues.
harmonize_one_study <- function(study_dir, project, organism, rules_dir,
                                out_stf_root, out_report_root) {
  report_dir <- file.path(out_report_root, project, organism)
  stf_dir    <- file.path(out_stf_root, project, organism)

  # 'message' is carried BOTH at the top level and in summary_row: the caller reads
  # summary_row for the TSV, but a direct caller (and the tests) needs the reason without
  # having to reach into a tibble.
  fail <- function(msg) {
    msg <- as.character(msg)
    list(status = 'failed', substitutions = 0L, unresolved = NULL, message = msg,
         summary_row = tibble(project = project, organism = organism, status = 'failed',
                              rows = NA_integer_, variables = NA_integer_,
                              substitutions = NA_integer_, rows_substituted = NA_integer_,
                              variables_renamed = NA_integer_,
                              variables_dropped = NA_integer_, unresolved = NA_integer_,
                              shadowed_rules = NA_integer_, message = msg))
  }

  study <- tryCatch(study_from_stf(study_dir), error = function(e) e)
  if (inherits(study, 'error')) return(fail(paste('load failed:', conditionMessage(study))))

  rb <- tryCatch(load_rulebook(rules_dir, project, organism), error = function(e) e)
  if (inherits(rb, 'error')) return(fail(paste('rulebook error:', conditionMessage(rb))))

  non_vocab <- rb$non_vocabulary$variable
  entities <- get_entities(study)
  all_changes <- list(); all_applied <- list()
  all_findings <- list(); all_unresolved <- list()
  new_entities <- list()

  for (e in entities) {
    ename <- get_entity_name(e)
    # Entity-scoped filenames: every study currently has exactly one entity, but a
    # single per-study path would silently overwrite when the assay entity arrives.
    capture_inspect(e, file.path(report_dir, glue('inspect_before_{ename}.txt')))

    all_findings[[length(all_findings) + 1]] <- structural_findings(e, non_vocab)
    all_unresolved[[length(all_unresolved) + 1]] <-
      unresolved_values(e, rb$vocabulary, non_vocab) %>% mutate(entity = ename)

    vres <- tryCatch(apply_value_rules(e, rb$vocabulary, non_vocab),
                     error = function(x) x)
    if (inherits(vres, 'error')) {
      return(fail(paste('value rules failed on', ename, ':', conditionMessage(vres))))
    }
    ares <- tryCatch(
      apply_variable_actions(vres$entity, rb$actions, rb$approved_drops$variable),
      error = function(x) x)
    if (inherits(ares, 'error')) {
      return(fail(paste('variable actions failed on', ename, ':', conditionMessage(ares))))
    }

    all_changes[[length(all_changes) + 1]] <- vres$changes %>% mutate(entity = ename)
    all_applied[[length(all_applied) + 1]] <- ares$applied %>% mutate(entity = ename)

    if (!validate(quiet(ares$entity))) {
      validate(verbose(ares$entity))
      return(fail(glue('harmonized entity {ename} failed validation; nothing written')))
    }
    capture_inspect(ares$entity, file.path(report_dir, glue('inspect_after_{ename}.txt')))
    new_entities[[length(new_entities) + 1]] <- ares$entity
  }

  changes    <- bind_rows(all_changes)
  applied    <- bind_rows(all_applied)
  findings   <- bind_rows(all_findings)
  unresolved <- bind_rows(all_unresolved)

  new_study <- tryCatch(
    study_from_entities(new_entities, name = get_study_name(study) %||% organism),
    error = function(x) x)
  if (inherits(new_study, 'error')) {
    return(fail(paste('study assembly failed:', conditionMessage(new_study))))
  }

  dir.create(stf_dir, recursive = TRUE, showWarnings = FALSE)
  exported <- tryCatch({ export_to_stf(new_study, stf_dir); TRUE }, error = function(x) x)
  if (inherits(exported, 'error')) {
    return(fail(paste('export failed:', conditionMessage(exported))))
  }

  write_study_reports(report_dir, changes, applied, findings,
                      unresolved %>% select(variable, raw_value, rows_affected))

  root <- get_root_entity(study)
  list(
    status = 'ok',
    substitutions = nrow(changes),
    unresolved = unresolved %>% select(variable, raw_value, rows_affected),
    message = NA_character_,
    summary_row = tibble(
      project = project, organism = organism, status = 'ok',
      rows = as.integer(nrow(get_data(root))),
      variables = as.integer(nrow(get_variable_metadata(root))),
      substitutions = as.integer(nrow(changes)),
      rows_substituted = as.integer(sum(changes$rows_affected, na.rm = TRUE)),
      variables_renamed = as.integer(sum(applied$action == 'rename', na.rm = TRUE)),
      variables_dropped = as.integer(sum(applied$action == 'drop', na.rm = TRUE)),
      unresolved = as.integer(nrow(unresolved)),
      shadowed_rules = as.integer(rb$n_shadowed),
      message = NA_character_)
  )
}

HARMONIZE_MAIN <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  only <- if (length(args) > 0) args else NULL

  studies <- discover_studies(STUDIES_ROOT, only)
  if (nrow(studies) == 0) stop('No studies found to harmonize.')

  message(glue('Harmonizing {nrow(studies)} study/studies...'))

  results <- list(); unresolved_all <- list()
  for (i in seq_len(nrow(studies))) {
    s <- studies[i, ]
    message(glue('[{i}/{nrow(studies)}] {s$project}/{s$organism}'))
    r <- harmonize_one_study(s$dir, s$project, s$organism, RULES_DIR,
                             OUT_STF_ROOT, OUT_REPORT_ROOT)
    results[[length(results) + 1]] <- r$summary_row
    if (!is.null(r$unresolved)) unresolved_all[[length(unresolved_all) + 1]] <- r$unresolved
  }

  write_summary(results, file.path(OUT_REPORT_ROOT, 'harmonization_summary.tsv'))
  if (length(unresolved_all) > 0) {
    write_unresolved_summary(unresolved_all,
                             file.path(OUT_REPORT_ROOT, 'unresolved_values.tsv'))
  }

  summary_tbl <- bind_rows(results)
  n_failed <- sum(summary_tbl$status == 'failed')
  message(glue('\nDone. ok={sum(summary_tbl$status == "ok")} failed={n_failed}'))
  message(glue('Reports: {OUT_REPORT_ROOT}'))
  if (n_failed > 0) {
    message('Failed studies:')
    summary_tbl %>% filter(status == 'failed') %>%
      pwalk(function(project, organism, message, ...)
        base::message(glue('  {project}/{organism}: {message}')))
    quit(status = 1)
  }
  invisible(summary_tbl)
}

if (sys.nframe() == 0 && length(grep('^--file=.*harmonize_stf[.]R$', commandArgs())) > 0) {
  HARMONIZE_MAIN()
}

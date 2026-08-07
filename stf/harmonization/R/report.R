#' Capture inspect() output to a text file for before/after diffing.
capture_inspect <- function(entity, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(capture.output(inspect(entity)), path)
  invisible(path)
}

#' Render structural findings as reviewable markdown.
render_findings_md <- function(findings) {
  if (nrow(findings) == 0) {
    return(c('# Structural findings', '', 'No structural findings for this study.'))
  }
  header <- c('# Structural findings', '',
              paste0('Deferred defects (spec section 5, item 4). These are **reported, ',
                     'not fixed** - they change the entity schema and belong to pass two.'),
              '')
  body <- findings %>%
    group_split(check) %>%
    map(function(grp) {
      c(glue::glue('## {grp$check[1]}'), '',
        glue::glue('{nrow(grp)} finding(s), {sum(grp$rows_affected, na.rm = TRUE)} rows affected.'),
        '',
        '| variable | value | rows | detail |',
        '|---|---|---|---|',
        glue::glue('| {grp$variable} | {ifelse(is.na(grp$value), "-", grp$value)} | ',
                   '{grp$rows_affected} | {grp$detail} |'),
        '')
    }) %>%
    unlist()
  c(header, body)
}

#' Write all per-study reports.
write_study_reports <- function(out_dir, changes, applied_actions, findings, unresolved) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(changes,         file.path(out_dir, 'changes.tsv'), na = '')
  readr::write_tsv(applied_actions, file.path(out_dir, 'variable_actions.tsv'), na = '')
  readr::write_tsv(unresolved,      file.path(out_dir, 'unresolved_values.tsv'), na = '')
  writeLines(render_findings_md(findings), file.path(out_dir, 'structural_findings.md'))
  invisible(out_dir)
}

#' Write the repo-level summary across all studies.
write_summary <- function(rows, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_tsv(bind_rows(rows), path, na = '')
  invisible(path)
}

#' Write the aggregated cross-study curation work queue.
write_unresolved_summary <- function(unresolved_list, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  bind_rows(unresolved_list) %>%
    group_by(variable, raw_value) %>%
    summarise(rows_affected = sum(rows_affected), n_studies = n(), .groups = 'drop') %>%
    arrange(desc(rows_affected)) %>%
    readr::write_tsv(path, na = '')
  invisible(path)
}

#!/usr/local/bin/Rscript
## Emit valid STF (entity-sample.tsv + entity-sample.yaml + study.yaml) for every
## organism, from the harmonized tables written by stf/harmonize.py.
##
## One STF study per REFERENCE ORGANISM. Variables are already harmonized across
## all of that organism's dnaseq experiments (union of their variables), and
## dataset_name is retained so the experiments stay separable.
##
## Nextflow will later add the assay entity built from the per-sample
## *_alignment_stats.tsv files and re-export the final per-organism STF.
##
## usage:  export_stf.R <harmonizedRoot> <outputRoot> [organism ...]

suppressPackageStartupMessages({
  library(tidyverse)
  library(study.wrangler)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: export_stf.R <harmonizedRoot> <outputRoot> [organism ...]")

harmonized_root <- args[1]
output_root <- args[2]
only <- if (length(args) > 2) args[3:length(args)] else NULL

sample_files <- list.files(harmonized_root, pattern = "^samples\\.tsv$",
                           recursive = TRUE, full.names = TRUE)

ok <- 0; failed <- character()

for (sample_file in sample_files) {
  org_dir <- dirname(sample_file)
  organism <- basename(org_dir)
  project <- basename(dirname(org_dir))
  if (!is.null(only) && !(organism %in% only)) next

  mapping_file <- file.path(org_dir, "header_ontology_mapping.txt")
  out_dir <- file.path(output_root, project, organism)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  message("\n========== ", project, " / ", organism, " ==========")

  result <- try({
    entity <- entity_from_file(file_path = sample_file)

    # An organism's table is the union of its experiments' variables, so any
    # variable none of its experiments recorded arrives all-NA. Drop those.
    entity <- entity %>%
      modify_data({
        empty_cols <- (.) %>%
          select(-sample_id) %>%
          select(where(~ all(is.na(.)))) %>%
          names()
        if (length(empty_cols) > 0) {
          message("  dropping ", length(empty_cols), " all-empty columns")
          (.) %>% select(-all_of(empty_cols))
        } else (.)
      })

    # Variables whose values happen to be unique per row (parasite_strain,
    # fungal_strain, collection_date_original, ...) get auto-detected as ID
    # columns, which makes the entity invalid: "multiple ID columns per entity
    # level". sample_id is the only identifier; force everything else to be a
    # variable before promoting it.
    others <- setdiff(names(entity@data), "sample_id")
    if (length(others) > 0) entity <- entity %>% redetect_columns_as_variables(others)

    entity <- entity %>%
      set_entity_metadata(
        name = "sample",
        display_name = "Sample",
        display_name_plural = "Samples",
        description = paste0("Sequenced isolates aligned to the ", organism,
                             " reference genome, across all dnaseq experiments."),
        stable_id = "sample"
      )

    # display names + definitions, including the terms minted by harmonize.py
    mappings <- read_tsv(mapping_file, col_types = cols(.default = "c"))
    present <- (entity %>% get_variable_metadata())$variable
    for (i in seq_len(nrow(mappings))) {
      v <- mappings$headerName[i]
      if (!(v %in% present)) next
      if (!is.na(mappings$DisplayName[i]) && mappings$DisplayName[i] != "")
        entity <- entity %>% set_variable_metadata(v, display_name = mappings$DisplayName[i])
      if (!is.na(mappings$Definition[i]) && mappings$Definition[i] != "")
        entity <- entity %>% set_variable_metadata(v, definition = mappings$Definition[i])
    }

    entity <- entity %>%
      sync_variable_metadata() %>%
      redetect_column_as_id("sample_id") %>%
      set_variable_metadata("sample_id", entity_name = "sample")

    # Variable hierarchy. Categories come from harmonize.py so the grouping is
    # identical across organisms; only the categories whose children actually
    # exist for this organism are created.
    cats <- mappings %>%
      filter(!is.na(Category), Category != "", headerName %in% present) %>%
      group_by(Category, CategoryDisplayName) %>%
      summarise(children = list(headerName), .groups = "drop")

    for (i in seq_len(nrow(cats))) {
      kids <- intersect(cats$children[[i]], (entity %>% get_variable_metadata())$variable)
      if (length(kids) == 0) next
      entity <- entity %>% create_variable_category(
        cats$Category[i],
        display_name = cats$CategoryDisplayName[i],
        children = kids
      )
    }
    message("  created ", nrow(cats), " variable categories")

    study <- study_from_entities(entities = list(entity), name = organism)

    if (!validate(study, profiles = c("baseline", "eda")))
      stop("study did not validate")

    export_to_stf(study, out_dir)
    TRUE
  }, silent = FALSE)

  if (inherits(result, "try-error")) {
    failed <- c(failed, paste0(project, "/", organism))
    message("  !! FAILED: ", conditionMessage(attr(result, "condition")))
  } else {
    ok <- ok + 1
    message("  exported STF -> ", out_dir)
  }
}

message("\n================ SUMMARY ================")
message("exported: ", ok, "   failed: ", length(failed))
if (length(failed)) {
  message("failures:\n  ", paste(failed, collapse = "\n  "))
  quit(status = 1)
}

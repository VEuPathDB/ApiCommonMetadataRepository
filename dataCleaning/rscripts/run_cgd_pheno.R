#!/usr/bin/env Rscript

library(devtools)
devtools::load_all("/home/jbrestel/dataCleaning/study-wrangler", quiet = FALSE)
library(tidyverse)

# Set working directory to the test data location so entity_from_file() finds the file
setwd("/home/jbrestel/dataCleaning/test_data/FungiDB_CGD_pheno_Phenotype_RSRC")

# Source the dataset script
datasetName <- "FungiDB_CGD_pheno_Phenotype_RSRC"
datasetScript <- "/home/jbrestel/dataCleaning/study-dealer-nextflow/lib/R/phenotype/FungiDB_CGD_pheno_Phenotype_RSRC.R"

dataset_env <- new.env()
source(datasetScript, local = dataset_env)

study <- dataset_env$wrangle() %>%
  set_study_name(datasetName)

if (!validate(study, profiles = c("baseline", "eda"))) {
  stop("Stopping....Study is not valid")
}

outputDir <- "/home/jbrestel/dataCleaning/results/FungiDB_CGD_pheno_Phenotype_RSRC"
dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
export_to_vdi(study, outputDir)

message("Done! Output in: ", outputDir)

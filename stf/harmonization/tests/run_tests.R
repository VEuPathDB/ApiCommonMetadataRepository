#!/usr/bin/env Rscript
# Run all harmonization tests.  Usage: Rscript stf/harmonization/tests/run_tests.R
library(testthat)
here <- dirname(normalizePath(sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))))
setwd(file.path(here, 'testthat'))
test_dir('.', reporter = 'summary', stop_on_failure = TRUE)

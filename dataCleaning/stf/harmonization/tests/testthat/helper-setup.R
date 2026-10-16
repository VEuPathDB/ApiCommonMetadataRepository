# Loaded automatically by testthat before any test file.
suppressPackageStartupMessages({
  library(tidyverse)
  library(testthat)
})

# Repo root, derived from this file's location so tests run from any cwd.
HARMONIZATION_DIR <- normalizePath(file.path(dirname(dirname(getwd()))))
REPO_ROOT <- normalizePath(file.path(HARMONIZATION_DIR, '..', '..'))

STUDY_WRANGLER_DIR <- Sys.getenv('STUDY_WRANGLER_DIR')
if (!nzchar(STUDY_WRANGLER_DIR)) stop('Set STUDY_WRANGLER_DIR to a study-wrangler checkout.')
suppressPackageStartupMessages(
  devtools::load_all(STUDY_WRANGLER_DIR, quiet = TRUE)
)

# Source every engine file, then the CLI (which defines the orchestration functions).
for (f in list.files(file.path(HARMONIZATION_DIR, 'R'), pattern = '\\.R$', full.names = TRUE)) {
  source(f)
}
HARMONIZATION_BOOTSTRAPPED <- TRUE   # suppress the CLI's own bootstrap block
# local = TRUE is load-bearing, not style. Default source() evaluates in globalenv, where
# neither the flag above nor HARMONIZATION_DIR/REPO_ROOT exist (testthat sources helpers
# into the test environment, not globalenv). The CLI would then re-bootstrap, resolve
# HARMONIZATION_DIR from a relative --file= belonging to run_tests.R, and fail to find
# study-wrangler. Evaluating in this environment lets the guard and the constants see it.
source(file.path(HARMONIZATION_DIR, 'harmonize_stf.R'), local = TRUE)

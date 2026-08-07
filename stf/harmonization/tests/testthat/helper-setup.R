# Loaded automatically by testthat before any test file.
suppressPackageStartupMessages({
  library(tidyverse)
  library(testthat)
})

# Repo root, derived from this file's location so tests run from any cwd.
HARMONIZATION_DIR <- normalizePath(file.path(dirname(dirname(getwd()))))
REPO_ROOT <- normalizePath(file.path(HARMONIZATION_DIR, '..', '..'))

suppressPackageStartupMessages(
  devtools::load_all(file.path(REPO_ROOT, 'study-wrangler'), quiet = TRUE)
)

# Source every engine file.
for (f in list.files(file.path(HARMONIZATION_DIR, 'R'), pattern = '\\.R$', full.names = TRUE)) {
  source(f)
}

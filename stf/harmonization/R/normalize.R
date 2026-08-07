#' Normalize a value for rule-key matching.
#'
#' Must stay byte-identical in behaviour to harmonize.py's normh():
#' lowercase, then strip every character that is not a-z or 0-9.
#' This is what lets a rulebook authored here be lifted into harmonize.py unchanged.
#'
#' Known quirks, deliberate and pinned by tests in test-normalize.R:
#'   - Non-ASCII characters are STRIPPED, not folded: 'Leon' with an accent and
#'     plain 'Leon' produce different keys.
#'   - Values differing only in punctuation collide: '1-2' and '12' share a key.
#'   - All-punctuation values collapse to the empty key ''.
#' Do not "fix" any of these in R alone — normh() in stf/harmonize.py must change
#' in the same commit, or the rulebooks stop being portable.
#'
#' @param x character vector
#' @return character vector of keys; NA in, NA out
norm_value <- function(x) {
  out <- gsub('[^a-z0-9]', '', tolower(as.character(x)))
  out[is.na(x)] <- NA_character_
  out
}

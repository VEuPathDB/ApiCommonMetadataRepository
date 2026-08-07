#' Normalize a value for rule-key matching.
#'
#' Must stay byte-identical in behaviour to harmonize.py's normh():
#' lowercase, then strip every character that is not a-z or 0-9.
#' This is what lets a rulebook authored here be lifted into harmonize.py unchanged.
#'
#' @param x character vector
#' @return character vector of keys; NA in, NA out
norm_value <- function(x) {
  out <- gsub('[^a-z0-9]', '', tolower(as.character(x)))
  out[is.na(x)] <- NA_character_
  out
}

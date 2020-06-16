##### Minimal Reproducible Analysis Example
##### Functions

#' Detect unique identifier columns
#'
#' Given a data.frame properly setup with units appended to measured variables,
#' this function will detect unique identifiers in the data.frame and return
#' a vector of 0 and 1s:
#'    - 0: column is NOT an unique identifier.
#'    - 1: column is an unique identifier.
#'
#' @param df A data.frame with with units appended to measured variables.
#' variable name and unit are separated by a dollar sign: variable$unit.
#'
#' @return An integer vector of 0 and 1 indicating if a given column
#' is a unique identifier or not.

detect_unique_ID <- function(df) {

  facts <- df[grep(".+\\$.+", names(df), invert = TRUE)]

  quants_names <- names(d)[!(names(d) %in% names(facts))]
  quants <- rep(0, length(quants_names))
  names(quants) <-  quants_names

  facts_is_ID <- purrr::map_int(facts, ~ dplyr::n_distinct(.x) == length(.x))

  int_ID <-  c(facts_is_ID, quants)
  int_ID <- int_ID[names(df)]

  return(int_ID)
}


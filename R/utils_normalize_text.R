#' Main function to retrieve utils_normalize_text
#'
#' @title Normalize text
#' @description Normalizes text by converting to lowercase, removing diacritics, and eliminating whitespace.
#' @param x Character vector.
#' @return A normalized character vector.
#' @keywords internal
#' @importFrom dplyr filter mutate select summarise bind_rows left_join
#' @importFrom stringr str_detect str_remove str_to_lower str_sub
#' @importFrom purrr map map_dfr compact
#' @importFrom magrittr %>%
#' @importFrom rlang abort
utils_normalize_text <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("\\s+", "")
}

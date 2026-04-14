utils_normalize_text <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("\\s+", "")
}
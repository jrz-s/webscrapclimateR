#' Main function to retrieve search_locations
#'
#' @title Search locations
#' @description Searches the internal location database using flexible matching for region, state, and city names.
#' @param region Character. Optional region name.
#' @param state Character. State abbreviation or full name.
#' @param city Character. City name (partial or full).
#' @return A tibble containing matching locations and associated metadata, including region, state, city name, and data source links.
#' @examples
#' search_locations(state = "se", city = "arac")
#' @importFrom dplyr filter mutate select summarise bind_rows left_join
#' @importFrom stringr str_detect str_remove str_to_lower str_sub
#' @importFrom purrr map map_dfr compact
#' @importFrom magrittr %>%
#' @export
search_locations <- function(region = NULL,
                             state = NULL,
                             city = NULL) {

  df <- linksdataset

  # -------- REGION --------
  if (!is.null(region)) {

    region_match <- match_region(region)

    if (length(region_match) == 0) {
      warning("No matching regions found")
    }

    pattern <- paste(region_match, collapse = "|")

    df <- df %>%
      dplyr::filter(
        stringr::str_detect(
          utils_normalize_text(.data$region),
          pattern
        )
      )
  }

  # -------- STATE --------
  if (!is.null(state)) {

    state_abbr <- match_state(state)

    if (length(state_abbr) == 0) {
      warning("No matching states found")
    }

    pattern <- paste(state_abbr, collapse = "|")

    df <- df %>%
      dplyr::filter(
        stringr::str_detect(.data$state, pattern)
      )
  }

  # -------- CITY --------
  if (!is.null(city)) {

    city_pattern <- utils_normalize_text(city) %>%
      paste(collapse = "|")

    df <- df %>%
      dplyr::mutate(
        city_clean = utils_normalize_text(.data$city_name)
      ) %>%
      dplyr::filter(
        stringr::str_detect(city_clean, city_pattern)
      ) %>%
      dplyr::select(-city_clean)
  }

  return(df)
}

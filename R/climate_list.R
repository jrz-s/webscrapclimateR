#' Main function to retrieve climate_list
#'
#' @title Retrieve climate data for multiple locations
#' @description Fetches climatological data for multiple locations using their associated data source links.
#' @param data A tibble containing location metadata, including a 'links' column.
#' @param ids Optional numeric vector of location identifiers used to filter the dataset.
#' @param delay Numeric. Time delay in seconds between successive web requests.
#' @return A list of tibbles, each containing monthly climatological data for a given location.
#' @examples
#' df <- search_locations(state = "se", city = "aracaju")
#' climate_list(df)
#' @keywords internal
#' @importFrom dplyr filter mutate select summarise bind_rows left_join
#' @importFrom stringr str_detect str_remove str_to_lower str_sub
#' @importFrom purrr map map_dfr compact
#' @importFrom magrittr %>%
#' @export
climate_list <- function(data, ids = NULL, delay = 1) {

  if (missing(data)) {
    data <- linksdataset
  }

  if (!is.null(ids)) {
    data <- data %>%
      dplyr::filter(id %in% ids)
  }

  links <- data$links

  results <- purrr::map(links, function(link) {
    Sys.sleep(delay)
    climate_link(link)
  })

  purrr::compact(results)
}

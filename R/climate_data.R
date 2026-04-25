#' Main function to retrieve climate data
#'
#' @title Retrieve climatological data
#' @description Retrieves monthly climatological data from Climatempo using flexible location inputs and automated web scraping.
#' @param region Character. Optional region name (e.g., "north", "northeast").
#' @param state Character. State abbreviation or full name (e.g., "se", "sergipe").
#' @param city Character. City name (partial or full; accent-insensitive).
#' @param max_cities Numeric. Maximum number of locations allowed to prevent excessive requests. Default is 5.
#' @param delay Numeric. Time delay in seconds between successive web requests. Default is 1.
#' @param add_site Logical. If TRUE, adds a site identifier column to the output.
#' @param add_avg Logical. If TRUE, computes monthly averages across locations (only when more than one location is selected).
#' @return A tibble containing monthly climatological variables, including minimum temperature (tmin), maximum temperature (tmax), and precipitation (precip).
#' @examples
#' climate_data(state = "se", city = "aracaju")
#' @importFrom dplyr filter mutate select summarise bind_rows left_join
#' @importFrom stringr str_detect str_remove str_to_lower str_sub
#' @importFrom purrr map map_dfr compact
#' @importFrom magrittr %>%
#' @export
climate_data <- function(region = NULL,
                         state = NULL,
                         city = NULL,
                         max_cities = 5,
                         delay = 1,
                         add_site = TRUE,
                         add_avg = TRUE) {
  
  tryCatch({
    
    df <- search_locations(region, state, city)
    
    if (nrow(df) == 0) {
      rlang::abort("No locations found. Please refine your search.")
    }
    
    if (nrow(df) > max_cities) {
      rlang::abort(
        paste0(
          "Too many locations selected (", nrow(df), "). ",
          "Please refine your search or increase 'max_cities'."
        )
      )
    }
    
    message("Fetching climate data...")
    
    res <- climate_list(df, delay = delay)
    
    if (length(res) == 0) {
      rlang::abort("No data could be retrieved from the source.")
    }
    
    out <- build_climate_table(
      climate_list = res,
      add_site = add_site,
      add_avg  = add_avg
    )
    
    message("Done!")
    
    return(out)
    
  }, error = function(e) {
    stop(e$message, call. = FALSE)
  })
}
climate_data <- function(region = NULL,
                             state = NULL,
                             city = NULL,
                             max_cities = 5,
                             delay = 1,
                             add_site = TRUE,
                             add_avg = TRUE) {
  
  df <- search_locations(region, state, city)
  
  if (nrow(df) == 0) {
    stop("No locations found. Please refine your search.")
  }
  
  if (nrow(df) > max_cities) {
    stop(
      paste0(
        "Too many locations selected (", nrow(df), "). ",
        "Please refine your search or increase 'max_cities'."
      )
    )
  }
  
  message("Fetching climate data...")
  
  res <- climate_list(df, delay = delay)
  
  if (length(res) == 0) {
    stop("No data could be retrieved from the source.")
  }
  
  out <- build_climate_table(
    climate_list = res,
    add_site = add_site,
    add_avg  = add_avg
  )
  
  message("Done!")
  
  return(out)
}
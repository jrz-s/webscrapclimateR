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
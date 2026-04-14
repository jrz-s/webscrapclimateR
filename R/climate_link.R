climate_link <- function(climatologia_link) {
  
  tryCatch({
    
    page <- rvest::read_html(climatologia_link)
    
    table <- page %>%
      rvest::html_elements(".txt-black + .columns") %>%
      rvest::html_table()
    
    if (length(table) == 0) {
      warning(paste("No table found for:", climatologia_link))
      return(NULL)
    }
    
    df <- table[[1]]
    
    colnames(df) <- c("month", "tmin", "tmax", "precip")
    
    df <- df %>%
      dplyr::mutate(
        tmin = as.numeric(stringr::str_remove(tmin, "[°.]")),
        tmax = as.numeric(stringr::str_remove(tmax, "[°.]")),
        precip = as.numeric(precip) / 10,
        month = 1:12
      )
    
    df$id <- stringr::str_extract(climatologia_link, "[0-9]+")
    
    return(df)
    
  }, error = function(e) {
    warning(paste("Error in:", climatologia_link))
    return(NULL)
  })
}
#' Main function to retrieve climate link
#'
#' @title Retrieve climate data from a single source
#' @description Scrapes monthly climatological data from a single Climatempo webpage.
#' @param climatologia_link Character. URL of the climatology webpage.
#' @return A tibble containing monthly values of temperature and precipitation for the specified location.
#' @keywords internal
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
        tmin = as.numeric(stringr::str_replace_all(tmin, "[^0-9.-]", "")),
        tmax = as.numeric(stringr::str_replace_all(tmax, "[^0-9.-]", "")),
        precip = as.numeric(precip),
        month = 1:12
      )

    df$id <- stringr::str_extract(climatologia_link, "[0-9]+")

    return(df)

  }, error = function(e) {
    warning(paste("Error in:", climatologia_link))
    return(NULL)
  })
}

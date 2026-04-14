#' Main function to retrieve build_climate_table
#'
#' @title Build climatological dataset
#' @description Combines and structures climatological data from multiple locations into a single tidy dataset.
#' @param climate_list A list of tibbles containing climatological data.
#' @param n_char_site Numeric. Number of characters used to construct the site identifier.
#' @param add_site Logical. If TRUE, adds a site identifier column.
#' @param add_avg Logical. If TRUE, computes monthly averages across locations.
#' @return A tibble containing structured climatological data.
#' @keywords internal
#' @importFrom dplyr filter mutate select summarise bind_rows left_join
#' @importFrom stringr str_detect str_remove str_to_lower str_sub
#' @importFrom purrr map map_dfr compact
#' @importFrom magrittr %>%
build_climate_table <- function(climate_list,
                                n_char_site = 3,
                                add_site = TRUE,
                                add_avg = TRUE) {

  if (!is.list(climate_list)) {
    stop("Input must be a list of data frames")
  }

  df <- purrr::map_dfr(climate_list, dplyr::bind_rows)

  df <- df %>%
    dplyr::mutate(id = as.integer(id)) %>%
    dplyr::left_join(
      dplyr::select(linksdataset, id, region, state, city_name),
      by = "id"
    )

  # -------- SITE --------
  if (add_site) {

    df <- df %>%
      dplyr::mutate(
        site = paste0(
          stringr::str_to_upper(stringr::str_sub(city_name, 1, n_char_site)),
          stringr::str_to_upper(state)
        )
      )
  }

  # -------- Nº DE CIDADES --------
  n_cities <- df %>%
    dplyr::distinct(city_name) %>%
    nrow()

  # -------- MÉDIA --------
  if (add_avg) {

    if (n_cities == 1) {

      message("Average not computed: only one city selected.")

    } else {

      avg_df <- df %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
          tmin = round(mean(tmin, na.rm = TRUE), 0),
          tmax = round(mean(tmax, na.rm = TRUE), 0),
          precip = round(mean(precip, na.rm = TRUE), 0),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          city_name = "average",
          state = NA,
          region = NA,
          id = NA
        )

      if (add_site) {
        avg_df <- avg_df %>% dplyr::mutate(site = "AVG")
      }

      df <- dplyr::bind_rows(df, avg_df)
    }
  }

  # -------- SELEÇÃO DINÂMICA --------

  cols <- c("region", "state", "city_name", "month", "tmin", "tmax", "precip")

  if (add_site) {
    cols <- c("region", "state", "city_name", "site", "month", "tmin", "tmax", "precip")
  }

  df <- df %>%
    dplyr::select(all_of(cols))

  # -------- ORDENAÇÃO --------

  df <- df %>%
    dplyr::mutate(
      .order = ifelse(city_name == "average", 1, 0)
    ) %>%
    dplyr::arrange(.order, city_name, month) %>%
    dplyr::select(-.order)

  return(df)
}

#' Main function to retrieve match_region
#'
#' @title Match region input
#' @description Matches user-provided input to valid Brazilian regions using flexible and approximate string matching.
#' @param region_input Character vector containing user-provided region names.
#' @return A character vector of matched region names.
#' @keywords internal
#' @importFrom dplyr filter mutate select summarise bind_rows left_join
#' @importFrom stringr str_detect str_remove str_to_lower str_sub
#' @importFrom purrr map map_dfr compact
#' @importFrom magrittr %>%
match_region <- function(region_input) {

  region_table <- tibble::tibble(
    region_full = c("north","northeast","centralwest","southeast","south"),
    region_pt   = c("norte","nordeste","centrooeste","sudeste","sul")
  )

  input <- utils_normalize_text(region_input)

  matches <- purrr::map(input, function(x) {

    # -------- 1. MATCH EXATO (EN) --------
    if (x %in% region_table$region_full) {
      return(x)
    }

    # -------- 2. MATCH EXATO (PT → EN) --------
    idx_pt <- which(region_table$region_pt == x)

    if (length(idx_pt) == 1) {
      return(region_table$region_full[idx_pt])
    }

    # -------- 3. MATCH PARCIAL --------
    idx_partial <- which(
      stringr::str_detect(region_table$region_full, x) |
        stringr::str_detect(region_table$region_pt, x)
    )

    candidates <- unique(region_table$region_full[idx_partial])

    # -------- DECISÃO --------

    if (length(candidates) == 1) {
      return(candidates)
    }

    if (length(candidates) > 1) {
      stop(
        paste0(
          "Ambiguous region input: '", x, "'.\n",
          "Possible matches: ", paste(candidates, collapse = ", "), "\n",
          "Please be more specific."
        )
      )
    }

    # -------- NÃO ENCONTRADO --------
    stop(paste0("No region found for: ", x))
  })

  unique(unlist(matches))
}

#' Main function to retrieve match_state
#'
#' @title Match state input
#' @description Matches user-provided input to valid Brazilian state abbreviations using flexible and approximate string matching.
#' @param state_input Character vector containing user-provided state names or abbreviations.
#' @return A character vector of matched state abbreviations.
#' @keywords internal
#' @importFrom dplyr filter mutate select summarise bind_rows left_join
#' @importFrom stringr str_detect str_remove str_to_lower str_sub
#' @importFrom purrr map map_dfr compact
#' @importFrom magrittr %>%
match_state <- function(state) {
  
  valid_states <- c(
    "ac","al","ap","am","ba","ce","df","es","go",
    "ma","mt","ms","mg","pa","pb","pr","pe","pi",
    "rj","rn","rs","ro","rr","sc","sp","se","to"
  )
  
  state_clean <- utils_normalize_text(state)
  
  if (!state_clean %in% valid_states) {
    rlang::abort(
      paste0(
        "No state found for: '", state, "'. ",
        "Please provide a valid Brazilian state abbreviation (e.g., 'se', 'sp')."
      )
    )
  }
  
  return(state_clean)
}
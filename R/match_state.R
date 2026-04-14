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
match_state <- function(state_input) {

  state_table <- tibble::tibble(
    state_full = c("acre","alagoas","amapa","amazonas","bahia","ceara",
                   "distritofederal","espiritosanto","goias","maranhao",
                   "matogrosso","matogrossodosul","minasgerais","para",
                   "paraiba","parana","pernambuco","piaui","riodejaneiro",
                   "riograndedonorte","riograndedosul","rondonia","roraima",
                   "santacatarina","saopaulo","sergipe","tocantins"),

    state_abbr = c("ac","al","ap","am","ba","ce","df","es","go","ma","mt","ms",
                   "mg","pa","pb","pr","pe","pi","rj","rn","rs","ro","rr","sc",
                   "sp","se","to")
  )

  input <- utils_normalize_text(state_input)

  matches <- purrr::map(input, function(x) {

    # -------- 1. MATCH SIGLA EXATA --------
    if (x %in% state_table$state_abbr) {
      return(x)
    }

    # -------- MATCH PARCIAL (QUALQUER TAMANHO) --------
    idx_partial <- which(stringr::str_detect(state_table$state_full, x))

    candidates <- state_table$state_abbr[idx_partial]

    # -------- DECISÃO --------

    if (length(candidates) == 1 && nchar(x) > 2) {
      return(candidates)
    }

    if (length(candidates) > 1) {
      stop(
        paste0(
          "Ambiguous state input: '", x, "'.\n",
          "Possible matches: ", paste(candidates, collapse = ", "), "\n",
          "Please be more specific."
        )
      )
    }

    # -------- 4. NÃO ENCONTRADO --------
    stop(paste0("No state found for: ", x))
  })

  unique(unlist(matches))
}

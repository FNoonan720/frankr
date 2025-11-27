#' @importFrom magrittr "%>%"
#' @importFrom httr "GET" "content" "add_headers"
#' @importFrom plyr "round_any"
#' @importFrom dplyr "filter" "arrange" "desc"
#' @importFrom rvest "read_html" "html_nodes" "html_table"
#' @importFrom data.table "rbindlist"
#' @importFrom stats "setNames"

#' @title preview
#'
#' @param df df
#' @param n n
#'
#' @export
preview <- function(df, n=3) {
  df_name <- sys.calls()[[length(sys.calls())-1]] %>%
    as.character %>%
    strsplit(split=' ') %>%
    .[[2]]

  View(df[1:n,],
       paste0("\n", df_name, "[1:", n, "]:"))
  View(df[(nrow(df)-(n-1)):nrow(df),],
       paste0("\n", df_name, "[", nrow(df)-(n-1), ":", nrow(df), "]:"))
}

#' @title generate_season_strings
#'
#' @param start_year start_year
#' @param end_year end_year
#'
#' @return seasons
#' @export
generate_season_strings <- function(start_year, end_year) {
  years <-
    start_year:(end_year - 1)
  sprintf(
    "%d-%02d",
    years,
   (years + 1) %% 100
  ) %>%
  return()
}

#' get_html_table_from_url
#'
#' @param url url
#' @param idx idx
#'
#' @return df
#' @export
get_html_table_from_url <- function(url, idx=1) {
  read_html(url) %>%
    html_nodes("table") %>%
    html_table(fill=T) %>%
    .[[idx]] %>%
    as.data.frame %>%
    return()
}

#' get_nba_com_table
#'
#' @param url url
#' @param params params
#' @param idx idx
#' @param ignore_ranks ignore_ranks
#'
#' @return df
#' @export
get_nba_com_table <- function(url, params, idx=1, ignore_ranks=F) {
  resultSet <- content(GET(url = url, add_headers(.headers=nba_com_headers), query = params)) %>%
    .[['resultSets']] %>%
    .[[idx]]
  df <- resultSet %>%
    .[['rowSet']] %>%
    rbindlist %>%
    as.data.frame %>%
    setNames(resultSet %>%
               .[['headers']] %>%
               unlist)
  if(ignore_ranks) {
    df <- df[, !grepl("_RANK", names(df))]
  }
  return(df)
}

#' get_pbp_stats_table
#'
#' @param url url
#' @param params params
#' @param single_row single_row
#'
#' @return df
#' @export
get_pbp_stats_table <- function(url, params, single_row=F) {
  res <- content(GET(url = url, add_headers(.headers=pbp_stats_headers), query = params))
  if(single_row) {
    res[['single_row_table_data']] %>%
      as.data.frame %>%
      return()
  } else {
    if(res[['multi_row_table_data']] %>% is.null)
    {
      data <- res[['results']]
    }
    else {
      data <- res[['multi_row_table_data']]
    }
    data %>%
      rbindlist(fill=T) %>%
      as.data.frame %>%
      return()
  }
}

#' get_formula_from_model
#'
#' @param model model
#' @param df_name df_name
#'
#' @return formula
#' @export
get_formula_from_model <- function(model, df_name) {
  formula <- model$coefficients["(Intercept)"]
  for(i in 1:length(model$coefficients[-1])) {
    formula <- paste0(formula, " + ", model$coefficients[-1][i], "*", df_name,
                      "$", names(model$coefficients[-1])[i])
  }
  return(formula)
}

#' remaining_sample_avg
#'
#' @param entire_weight entire_weight
#' @param entire_value entire_value
#' @param sample_weight sample_weight
#' @param sample_value sample_value
#'
#' @return avg
#' @export
remaining_sample_avg <- function(entire_weight, entire_value, sample_weight, sample_value) {
  return((entire_value - sample_weight / entire_weight * sample_value) /
           ( (entire_weight - sample_weight) / entire_weight) )
}

#' get_team_color
#'
#' @param team team
#' @param color color
#' @param secondary secondary
#'
#' @return color
#' @export
get_team_color <- function(team, color=df_teams$COLOR_PRIMARY[df_teams$ABV %in% team], secondary=F) {
  if(secondary) {
    return(df_teams$COLOR_SECONDARY[df_teams$ABV %in% team])
  } else {
    return(color)
  }
}

## quiets concerns of R CMD check re: the var names that appear in pipelines
utils::globalVariables(c(".", "View"))

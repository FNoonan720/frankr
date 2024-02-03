#' @importFrom magrittr "%>%"
#' @importFrom data.table "rbindlist"
#' @importFrom httr "GET" "content" "add_headers"
#' @importFrom rvest "read_html" "html_nodes" "html_table"
#' @importFrom ggplot2 "ggsave"

#' @title preview
#'
#' @param df df
#' @param n n
#'
#' @export
preview <- function(df, n) {
  View(df[1:n,])
  View(df[(nrow(df)-(n-1)):nrow(df),])
}

#' get_html_table_from_url
#'
#' @param url url
#' @param idx idx
#'
#' @return df
#' @export
get_html_table_from_url <- function(url, idx=1) {
  rvest::read_html(url) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill=T) %>%
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
  resultSet <- httr::content(
    httr::GET(url = url, httr::add_headers(.headers=nba_com_headers), query = params)) %>%
    .[['resultSets']] %>%
    .[[idx]]
  df <- resultSet %>%
    .[['rowSet']] %>%
    data.table::rbindlist() %>%
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
  content <- httr::content(
    httr::GET(url = url, httr::add_headers(.headers=pbp_stats_headers), query = params))
  if(single_row) {
    content[['single_row_table_data']] %>%
      as.data.frame %>%
      return()
  } else {
    if(content[['multi_row_table_data']] %>% is.null)
    {
      data <- content[['results']]
    }
    else {
      data <- content[['multi_row_table_data']]
    }
    data %>%
      data.table::rbindlist(fill=T) %>%
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

#' save_plot
#'
#' @param title title
#'
#' @return plot
#' @export
save_plot <- function(title) {
  return(ggplot2::ggsave(file = title, units = "in", dpi = 320, width = 10, height = 10))
}

## quiets concerns of R CMD check re: the var names that appear in pipelines
utils::globalVariables(c(".", "setNames", "View"))

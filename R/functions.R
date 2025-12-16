#' @importFrom magrittr "%>%"
#' @importFrom httr "GET" "content" "add_headers"
#' @importFrom plyr "round_any"
#' @importFrom dplyr "filter" "arrange" "desc"
#' @importFrom rvest "read_html" "html_nodes" "html_table"
#' @importFrom data.table "rbindlist"
#' @importFrom stats "setNames"


#' @title Custom ggplot2 theme
#'
#' @description Creates a custom dark-themed ggplot2 theme with Montserrat font and
#' consistent styling. This theme is optimized for presentations and reports with
#' a dark background aesthetic.
#'
#' @details This function requires the following packages to be installed:
#' \itemize{
#'   \item ggplot2
#'   \item ggtext
#'   \item showtext
#'   \item sysfonts
#' }
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Basic usage
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(title = "Car Weight vs MPG",
#'        subtitle = "Example using frankr theme")
#'
#' # Apply the custom theme
#' p + theme_frankr()
#' }
#'
#' @export
theme_frankr <- function() {
  required_packages <- c("ggplot2", "ggtext", "showtext", "sysfonts")
  for(pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = T)) {
      stop(pkg, " is required for this function. Please install it with: install.packages('", pkg, "')")
    }
  }

  sysfonts::font_add_google("Montserrat", "mont")
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto()

  ggplot2::theme(
    text = ggplot2::element_text(family = "mont"),
    plot.title = ggtext::element_markdown(
      size = 54/.pt, color = "gray88", hjust = 0.5, face = "bold"
    ),
    plot.subtitle = ggtext::element_markdown(
      size = 46/.pt, color = "gray88", hjust = 0.5
    ),
    plot.caption = ggtext::element_markdown(
      size = 40/.pt, color = "gray88", hjust = 1.025,
      margin = ggplot2::margin(t = 10)
    ),
    axis.title = ggplot2::element_text(
      size=48/.pt, color="gray88", face="bold"
    ),
    axis.title.x = ggtext::element_markdown(
      size = 48/.pt, color = "gray88",
      hjust = 0.05, vjust = -2, margin = ggplot2::margin(t = 20)
    ),
    axis.title.y = ggtext::element_markdown(
      size = 48/.pt, color = "gray88", face = "bold",
      hjust = 0.05, vjust = 2, angle = 90,
      margin = ggplot2::margin(l = 10, r = 20)
    ),
    axis.text = ggplot2::element_text(
      size = 40/.pt, color="gray77", angle=0
    ),
    plot.background = ggplot2::element_rect(
      fill = "gray22", color = "gray22"
    ),
    plot.margin = ggplot2::margin(
      0.5, 1, 0.5, 0.5, "cm"
    ),
    panel.background = ggplot2::element_rect(
      fill = "gray28"
    ),
    panel.grid.major = ggplot2::element_line(
      color = "gray44", linetype = "dashed", linewidth = 0.5
    ),
    panel.grid.minor = ggplot2::element_line(
      color = "gray44", linetype = "dotted", linewidth = 0.5
    ),
    legend.position = "none"
  )
}

#' @title Preview function
#'
#' @description Quickly view the first and last n rows of a data frame in RStudio's viewer.
#' This is useful for getting a quick glimpse of large datasets.
#'
#' @param df A data frame to preview
#' @param n Number of rows to show from beginning and end (default is 3)
#'
#' @details This function opens two views in RStudio's Viewer: one showing the first n rows
#' and another showing the last n rows of the provided data frame. The function captures
#' the variable name dynamically using system calls.
#'
#' @examples
#' # Using with mtcars dataset
#' preview(mtcars)  # Shows first 3 and last 3 rows
#'
#' # Using with 5 rows
#' # preview(mtcars, n = 5)
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

#' @title Generate Season Strings
#'
#' @description Generate NBA-style season strings in the format "YYYY-YY" for a given range of years.
#'
#' @param start_year Numeric. The starting year (e.g., 2020)
#' @param end_year Numeric. The ending year (e.g., 2024)
#'
#' @return A character vector of season strings in the format "YYYY-YY"
#'
#' @examples
#' # Generate seasons from 2020 to 2024
#' generate_season_strings(2020, 2024)
#' # Returns: "2020-21" "2021-22" "2022-23" "2023-24"
#'
#' # Generate a single season
#' generate_season_strings(2023, 2024)
#' # Returns: "2023-24"
#'
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

#' Extract HTML Table from URL
#'
#' @description Extracts a table from a webpage by parsing the HTML content.
#' This is a general-purpose function for web scraping tabular data.
#'
#' @param url Character. The URL of the webpage containing the table
#' @param idx Numeric. The index of the table to extract if multiple tables exist on the page (default is 1)
#'
#' @return A data frame containing the extracted table data
#'
#' @details This function uses rvest package functions to read HTML from the given URL,
#' find all table elements, and extract the specified table. It handles basic table formatting
#' with the fill parameter to accommodate irregular table structures.
#'
#' @section Important Legal and Ethical Notice:
#' This function is provided for educational purposes. Users are responsible for ensuring
#' their use complies with the Terms of Service of target websites, applicable laws, and ethical
#' guidelines. Please respect robots.txt files, rate limits, and website terms of service.
#' Consider using official APIs when available as they are more reliable and ethically sound.
#'
#' @examples
#' \dontrun{
#' # Example usage (not run to avoid actual web requests)
#' # Extract first table from a webpage
#' df <- get_html_table_from_url("https://example.com/data-table.html")
#'
#' # Extract the second table from a webpage
#' df <- get_html_table_from_url("https://example.com/data-table.html", idx = 2)
#' }
#'
#' @export
get_html_table_from_url <- function(url, idx=1) {
  read_html(url) %>%
    html_nodes("table") %>%
    html_table(fill=T) %>%
    .[[idx]] %>%
    as.data.frame %>%
    return()
}

#' Get NBA.com Table Data
#'
#' @description Retrieves data from NBA.com's stats API by sending a request with specified parameters.
#' This function is designed to work with the NBA.com stats endpoint and returns tabular data.
#'
#' @param url Character. The NBA.com API endpoint URL
#' @param params Named list. Query parameters for the API request
#' @param idx Numeric. Index of the result set to return (default is 1)
#' @param ignore_ranks Logical. Whether to remove columns containing "_RANK" (default is FALSE)
#'
#' @return A data frame containing the requested table data
#'
#' @details This function sends a GET request to the NBA.com API with the provided URL and parameters.
#' It expects a JSON response with a "resultSets" element, from which it extracts the specified
#' result set and converts it to a data frame.
#'
#' The function uses predefined NBA.com headers to mimic legitimate browser requests. Note that
#' while this function doesn't properly handle all API endpoints (it works for most, which is why
#' I have left it as-is), it provides a good starting point for many common NBA.com API requests.
#'
#' @section Important Legal and Ethical Notice:
#' This function is provided for educational purposes. Users are responsible for ensuring
#' their use complies with the Terms of Service of target websites, applicable laws, and ethical
#' guidelines. Please respect robots.txt files, rate limits, and website terms of service.
#' Consider using official APIs when available as they are more reliable and ethically sound.
#' The NBA has an official API that may be more appropriate for production use.
#'
#' @examples
#' \dontrun{
#' # Example usage (not run to avoid actual API calls)
#' params <- list(MeasureType = "Base", PerMode = "PerGame", Season = "2023-24")
#' df <- get_nba_com_table("https://stats.nba.com/stats/leaguedashplayerstats", params)
#' }
#'
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

#' Get PBPStats.com Table Data
#'
#' @description Retrieves data from PBPStats.com by sending a request with specified parameters.
#' This function handles both single-row and multi-row table data formats.
#'
#' @param url Character. The PBPStats.com API endpoint URL
#' @param params Named list. Query parameters for the API request
#' @param single_row Logical. Whether to return single-row table data (default is FALSE)
#'
#' @return A data frame containing the requested table data
#'
#' @details This function sends a GET request to PBPStats.com with the provided URL and parameters.
#' It handles different response formats depending on whether single-row or multi-row data is expected.
#' The function uses predefined PBPStats.com headers to mimic legitimate browser requests.
#'
#' @section Important Legal and Ethical Notice:
#' This function is provided for educational purposes. Users are responsible for ensuring
#' their use complies with the Terms of Service of target websites, applicable laws, and ethical
#' guidelines. Please respect robots.txt files, rate limits, and website terms of service.
#' Consider using official APIs when available as they are more reliable and ethically sound.
#'
#' @examples
#' \dontrun{
#' # Example usage (not run to avoid actual API calls)
#' params <- list(team_id = 1610612744, season = "2023-24")
#' df <- get_pbp_stats_table("https://www.pbpstats.com/data/team/shots", params)
#' }
#'
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

#' Extract Formula from Model
#'
#' @description Creates a readable formula string from a fitted model object.
#' This function constructs a mathematical expression from model coefficients.
#'
#' @param model A fitted model object with coefficients (e.g., from lm(), glm())
#' @param df_name Character. The name of the data frame used in the model (for variable reference)
#'
#' @return Character string representing the mathematical formula of the model
#'
#' @details This function iterates through model coefficients to build a string formula
#' representing the model. It starts with the intercept and adds each coefficient term
#' with its associated variable name.
#'
#' @examples
#' # Create a simple linear model
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # Get the formula as a string
#' formula_str <- get_formula_from_model(model, "mtcars")
#' print(formula_str)
#'
#' @export
get_formula_from_model <- function(model, df_name) {
  formula <- model$coefficients["(Intercept)"]
  for(i in 1:length(model$coefficients[-1])) {
    formula <- paste0(formula, " + ", model$coefficients[-1][i], "*", df_name,
                      "$", names(model$coefficients[-1])[i])
  }
  return(formula)
}

#' Calculate Average of Remaining Sample
#'
#' @description Computes the average value of the remaining portion after removing a weighted sample.
#' This is useful for calculating how removing certain data points affects the overall average.
#'
#' @param entire_weight Numeric. The total weight of the complete dataset
#' @param entire_value Numeric. The total value of the complete dataset
#' @param sample_weight Numeric. The weight of the sample to remove
#' @param sample_value Numeric. The total value of the sample to remove
#'
#' @return Numeric. The average value of the remaining sample after subtraction
#'
#' @details This function calculates what the average would be if you removed a specific weighted
#' sample from the total dataset. It's commonly used in statistics when analyzing the impact of
#' subsets on overall calculations.
#'
#' @examples
#' # If you have a dataset with total weight 100 and total value 500,
#' # and you want to know the average after removing a sample of weight 20 with value 120
#' remaining_avg <- remaining_sample_avg(100, 500, 20, 120)
#' print(remaining_avg)  # Should output 4.75
#'
#' @export
remaining_sample_avg <- function(entire_weight, entire_value, sample_weight, sample_value) {
  return((entire_value - sample_weight / entire_weight * sample_value) /
           ( (entire_weight - sample_weight) / entire_weight) )
}

#' Get NBA Team Color
#'
#' @description Retrieves the primary or secondary color for a given NBA team.
#'
#' @param team Character. The 3-letter team abbreviation (e.g., "GSW", "LAL")
#' @param color Character. The primary color for the team (default is automatically retrieved)
#' @param secondary Logical. Whether to return the secondary color instead of primary (default is FALSE)
#'
#' @return Character string representing the hex color code for the team color
#'
#' @details This function looks up team colors from the df_teams data frame.
#' It provides easy access to official NBA team colors for visualization purposes.
#'
#' @examples
#' # Get Golden State Warriors primary color
#' gsw_primary <- get_team_color("GSW")
#' print(gsw_primary)  # Should output "#FFC72C"
#'
#' # Get Golden State Warriors secondary color
#' gsw_secondary <- get_team_color("GSW", secondary = TRUE)
#' print(gsw_secondary)  # Should output "#1D428A"
#'
#' @export
get_team_color <- function(team, color=df_teams$COLOR_PRIMARY[df_teams$ABV %in% team], secondary=F) {
  if(secondary) {
    return(df_teams$COLOR_SECONDARY[df_teams$ABV %in% team])
  } else {
    return(color)
  }
}

## quiets concerns of R CMD check re: the var names that appear in pipelines
utils::globalVariables(c(".", "View", ".pt"))

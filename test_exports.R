# Test script for frankr package exports
# Run with: Rscript test_exports.R
# Or install first with: R CMD INSTALL . && Rscript test_exports.R

cat("Loading frankr...\n")
library(frankr)

# Track test results
passed <- 0
failed <- 0

test <- function(name, expr) {
  result <- tryCatch({
    eval(expr)
    TRUE
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    FALSE
  })

  if (result) {
    cat("✓", name, "\n")
    passed <<- passed + 1
  } else {
    cat("✗", name, "\n")
    failed <<- failed + 1
  }
}

cat("\n=== Testing Constants ===\n")

test("df_teams exists and has 30 rows", {
  stopifnot(is.data.frame(df_teams))
  stopifnot(nrow(df_teams) == 30)
})

test("df_teams has expected columns", {
  expected <- c("ID", "ABV", "CITY", "NAME", "COLOR_PRIMARY", "COLOR_SECONDARY")
  stopifnot(all(expected %in% names(df_teams)))
})

test("eastern_conf has 15 teams", {
  stopifnot(is.character(eastern_conf))
  stopifnot(length(eastern_conf) == 15)
})

test("western_conf has 15 teams", {
  stopifnot(is.character(western_conf))
  stopifnot(length(western_conf) == 15)
})

test("theme_colors is named character vector", {
  stopifnot(is.character(theme_colors))
  stopifnot(!is.null(names(theme_colors)))
})

test("common_headers is named character vector", {
  stopifnot(is.character(common_headers))
  stopifnot("User-Agent" %in% names(common_headers))
})

test("nba_com_headers includes Origin", {
  stopifnot(is.character(nba_com_headers))
  stopifnot("Origin" %in% names(nba_com_headers))
})

test("pbp_stats_headers includes Origin", {
  stopifnot(is.character(pbp_stats_headers))
  stopifnot("Origin" %in% names(pbp_stats_headers))
})

test("ctg_headers is character vector", {
  stopifnot(is.character(ctg_headers))
})

test("ctg_cookies is character vector", {
  stopifnot(is.character(ctg_cookies))
})

test("nba_com_season_types has expected values", {
  stopifnot(is.character(nba_com_season_types))
  stopifnot("Regular Season" %in% nba_com_season_types)
  stopifnot("Playoffs" %in% nba_com_season_types)
})

test("logo_base_url is a URL string", {
  stopifnot(is.character(logo_base_url))
  stopifnot(grepl("^https?://", logo_base_url))
})

test("square_logo_base_url is a URL string", {
  stopifnot(is.character(square_logo_base_url))
  stopifnot(grepl("^https?://", square_logo_base_url))
})

cat("\n=== Testing Functions ===\n")

test("generate_season_strings(2020, 2024)", {
  result <- generate_season_strings(2020, 2024)
  stopifnot(is.character(result))
  stopifnot(length(result) == 4)
  stopifnot(result[1] == "2020-21")
  stopifnot(result[4] == "2023-24")
})

test("generate_season_strings(2023, 2024) single season", {
  result <- generate_season_strings(2023, 2024)
  stopifnot(length(result) == 1)
  stopifnot(result == "2023-24")
})

test("get_team_color('GSW') returns primary color", {
  result <- get_team_color("GSW")
  stopifnot(is.character(result))
  stopifnot(grepl("^#", result))  # hex color
})

test("get_team_color('LAL', secondary=TRUE) returns secondary", {
  result <- get_team_color("LAL", secondary = TRUE)
  stopifnot(is.character(result))
  stopifnot(grepl("^#", result))
})

test("get_formula_from_model with lm()", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- get_formula_from_model(model, "mtcars")
  stopifnot(is.character(result))
  stopifnot(grepl("wt", result))
  stopifnot(grepl("hp", result))
})

test("remaining_sample_avg calculation", {
  result <- remaining_sample_avg(100, 500, 20, 120)
  stopifnot(is.numeric(result))
  stopifnot(abs(result - 595) < 0.01)
})

test("theme_frankr returns ggplot2 theme", {
  if (requireNamespace("ggplot2", quietly = TRUE) &&
      requireNamespace("ggtext", quietly = TRUE) &&
      requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)) {
    library(ggplot2)  # need to load for .pt
    result <- theme_frankr()
    stopifnot(inherits(result, "theme"))
  } else {
    cat("  (skipped - missing suggested packages)\n")
  }
})

cat("\n=== Network Tests ===\n")

# Check if we have network connectivity
has_network <- tryCatch({
  con <- url("https://www.google.com", open = "r")
  close(con)
  TRUE
}, error = function(e) FALSE)

if (!has_network) {
  cat("(skipping network tests - no connectivity)\n")
} else {
  test("get_html_table_from_url from Wikipedia", {
    # Fetch a simple table from Wikipedia
    url <- "https://en.wikipedia.org/wiki/List_of_NBA_champions"
    result <- get_html_table_from_url(url, idx = 1)
    stopifnot(is.data.frame(result))
    stopifnot(nrow(result) > 0)
  })

  test("get_nba_com_table for player stats", {
    url <- "https://stats.nba.com/stats/leaguedashplayerstats"
    params <- list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      DraftYear = "",
      GameScope = "",
      GameSegment = "",
      Height = "",
      ISTRound = "",
      LastNGames = "0",
      LeagueID = "00",
      Location = "",
      MeasureType = "Base",
      Month = "0",
      OpponentTeamID = "0",
      Outcome = "",
      PORound = "0",
      PaceAdjust = "N",
      PerMode = "PerGame",
      Period = "0",
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = "N",
      Season = "2024-25",
      SeasonSegment = "",
      SeasonType = "Regular Season",
      ShotClockRange = "",
      StarterBench = "",
      TeamID = "0",
      VsConference = "",
      VsDivision = "",
      Weight = ""
    )
    Sys.sleep(1)  # Rate limiting
    result <- get_nba_com_table(url, params)
    stopifnot(is.data.frame(result))
    stopifnot(nrow(result) > 0)
    stopifnot("PLAYER_NAME" %in% names(result))
  })

  test("get_pbp_stats_table for team totals", {
    url <- "https://api.pbpstats.com/get-totals/nba"
    params <- list(
      Season = "2024-25",
      SeasonType = "Regular Season",
      Type = "Player"
    )
    Sys.sleep(1)  # Rate limiting
    result <- get_pbp_stats_table(url, params)
    stopifnot(is.data.frame(result))
    stopifnot(nrow(result) > 0)
  })
}

cat("\n=== Skipped Tests (require RStudio) ===\n")
cat("- preview() - requires RStudio Viewer\n")

cat("\n=== Summary ===\n")
cat("Passed:", passed, "\n")
cat("Failed:", failed, "\n")

if (failed > 0) {
  quit(status = 1)
}

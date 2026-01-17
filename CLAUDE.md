# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`frankr` is a personal R package for data science workflows, focused on NBA/sports analytics and web scraping. It provides utilities for fetching data from NBA.com, PBPStats.com, and cleaningtheglass.com, plus visualization helpers.

## Development Commands

```bash
# Check package (runs R CMD check)
R CMD check .

# Build package
R CMD build .

# Install locally for testing
R CMD INSTALL .

# Generate documentation from roxygen2 comments
Rscript -e "roxygen2::roxygenise()"

# Load package in development (from R console)
devtools::load_all()
```

## Architecture

**File Structure:**
- `R/constants.R` - Exported data objects: `df_teams` (NBA team info with IDs, colors, logos), conference vectors, HTTP headers for API requests, color palettes
- `R/functions.R` - All exported functions

**Key Exports:**
- Web scraping: `get_nba_com_table()`, `get_pbp_stats_table()`, `get_html_table_from_url()` - each uses predefined headers from constants.R
- Visualization: `theme_frankr()` (dark ggplot2 theme with Montserrat font), `get_team_color()`, `theme_colors`
- Utilities: `preview()`, `generate_season_strings()`, `get_formula_from_model()`, `remaining_sample_avg()`

**Dependencies:**
- `Depends` (auto-attached): magrittr, dplyr, rvest
- `Imports`: httr, plyr, data.table
- `Suggests` (for theme_frankr): ggplot2, ggtext, showtext, sysfonts

## Code Conventions

- Uses roxygen2 for documentation (`#'` comments generate man/*.Rd files and NAMESPACE)
- Pipe-heavy style using magrittr `%>%`
- Functions use `return()` explicitly
- Web scraping functions include legal/ethical notices in documentation

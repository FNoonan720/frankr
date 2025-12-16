# frankr

`frankr` is an R package containing a collection of functions and constants that simplify common data science workflows, particularly focused on sports analytics and web scraping. The package provides utilities for NBA data collection, custom themes for visualization, and convenient helper functions.

## Installation

```r
# Install from GitHub
remotes::install_github("FNoonan720/frankr")
```

## Features

### NBA Data Utilities
- **Team Information**: Access to comprehensive NBA team data including abbreviations, names, colors, and logos
- **Web Scraping Helpers**: Functions to extract tables from websites and NBA.com's API
- **Season Generation**: Utility to generate season year strings (e.g., "2023-24")

### Visualization Tools
- **Custom Theme**: `theme_frankr()` for consistent, dark-themed plots
- **Team Colors**: Easy access to official NBA team colors

### General Purpose Functions
- **Preview Function**: Quickly view the first and last rows of a dataset
- **Formula Extraction**: Generate readable formulas from model objects
- **Average Calculations**: Helper for calculating averages of remaining samples

## Usage

### Team Data
```r
library(frankr)

# Access team information
head(df_teams)
get_team_color("GSW")  # Get primary color for Golden State Warriors
get_team_color("LAL", secondary = TRUE)  # Get secondary color for LA Lakers
```

### Web Scraping
```r
# Extract table from a URL
# Note: Always respect robots.txt and website terms of service
# get_html_table_from_url("https://example.com/table", idx = 1)
```

### Custom Themes
```r
library(ggplot2)
library(frankr)

# Apply custom theme
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_frankr()
```

### Season Strings
```r
# Generate NBA season strings
generate_season_strings(2020, 2024)
# Output: "2020-21" "2021-22" "2022-23" "2023-24"
```

## Important Notes for Data Access

For functions that interact with websites (NBA.com, PBPStats.com, cleaningtheglass.com, etc.):
- The `ctg_cookies` constant contains a session ID placeholder
- You need to replace `"YOUR_SESSION_ID"` with your actual session ID from the website
- This is required to make requests to the site
- Always comply with the website's terms of service and robots.txt
- Use rate limiting (e.g., Sys.sleep()) to avoid overloading servers
- Consider using official APIs when available as they're more reliable and ethical
- Be aware of legal and ethical implications of web scraping

## Contributing

This is a personal package that is being made public for convenience. If you have suggestions or notice issues, please feel free to reach out.

## License

MIT License - see the [LICENSE](LICENSE.md) file for details.

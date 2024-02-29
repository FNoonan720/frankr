#' common_headers
#' @export
common_headers = c(`Accept` = "*/*", `Accept-Language` = "en-US,en;q=0.9", `Connection` = "keep-alive", `Sec-Fetch-Dest` = "empty",
                    `Sec-Fetch-Mode` = "cors", `Sec-Fetch-Site` = "same-site", `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
                    `sec-ch-ua` = '"Not_A Brand";v="8", "Chromium";v="120", "Google Chrome";v="120"', `sec-ch-ua-mobile` = "?0", `sec-ch-ua-platform` = '"Windows"')

#' nba_com_headers
#' @export
nba_com_headers = c(common_headers, `Origin` = "https://www.nba.com", `Referer` = "https://www.nba.com/")

#' pbp_stats_headers
#' @export
pbp_stats_headers = c(common_headers, `Origin` = "https://www.pbpstats.com", `Referer` = "https://www.pbpstats.com/")

#' logo_base_url
#' @export
logo_base_url <- "https://loodibee.com/wp-content/uploads/"

#' df_teams
#' @export
df_teams <- data.frame(
  ID = c(1610612737, 1610612751, 1610612738, 1610612766, 1610612741, 1610612739, 1610612742, 1610612743, 1610612765, 1610612744,
         1610612745, 1610612754, 1610612746, 1610612747, 1610612763, 1610612748, 1610612749, 1610612750, 1610612740, 1610612752,
         1610612760, 1610612753, 1610612755, 1610612756, 1610612757, 1610612758, 1610612759, 1610612761, 1610612762, 1610612764),
  ABV = c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
          "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
          "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"),
  CITY = c("Atlanta",       "Brooklyn", "Boston",       "Charlotte",    "Chicago",  "Cleveland",  "Dallas",       "Denver",     "Detroit",      "Golden State",
           "Houston",       "Indiana",  "Los Angeles",  "Los Angeles",  "Memphis",  "Miami",      "Milwaukee",    "Minnesota",  "New Orleans",  "New York",
           "Oklahoma City", "Orlando",  "Philadelphia", "Phoenix",      "Portland", "Sacramento", "San Antonio",  "Toronto",    "Utah",         "Washington"),
  NAME = c("Hawks", "Nets", "Celtics", "Hornets", "Bulls", "Cavaliers", "Mavericks", "Nuggets", "Pistons", "Warriors",
           "Rockets", "Pacers", "Clippers", "Lakers", "Grizzlies", "Heat", "Bucks", "Timberwolves", "Pelicans", "Knicks",
           "Thunder", "Magic", "76ers", "Suns", "Trail Blazers", "Kings", "Spurs", "Raptors", "Jazz", "Wizards"),
  CTG_NAME = c("Atlanta",       "Brooklyn", "Boston",       "Charlotte",  "Chicago",  "Cleveland",  "Dallas",       "Denver",     "Detroit",      "Golden State",
               "Houston",       "Indiana",  "LA Clippers",  "LA Lakers",  "Memphis",  "Miami",      "Milwaukee",    "Minnesota",  "New Orleans",  "New York",
               "Oklahoma City", "Orlando",  "Philadelphia", "Phoenix",    "Portland", "Sacramento", "San Antonio",  "Toronto",    "Utah",         "Washington"),
  LOGO = c(paste0(logo_base_url,"nba-atlanta-hawks-logo.png"),              paste0(logo_base_url,"brooklyn-nets-logo-symbol.png"),
           paste0(logo_base_url,"nba-boston-celtics-logo.png"),             paste0(logo_base_url,"charlotte-hornets-logo-symbol.png"),
           paste0(logo_base_url,"nba-chicago-bulls-logo.png"),              paste0(logo_base_url,"cleveland-cavaliers-logo-symbol.png"),
           paste0(logo_base_url,"dallas-mavericks-logo-symbol.png"),        paste0(logo_base_url,"denver-nuggets-logo-symbol.png"),
           paste0(logo_base_url,"nba-detroit-pistons-logo.png"),            paste0(logo_base_url,"nba-golden-state-warriors-logo-2020.png"),
           paste0(logo_base_url,"houston-rockets-logo-symbol.png"),         paste0(logo_base_url,"indiana-pacers-logo-symbol.png"),
           paste0(logo_base_url,"los-angeles-clippers-logo-symbol.png"),    paste0(logo_base_url,"nba-los-angeles-lakers-logo.png"),
           paste0(logo_base_url,"nba-memphis-grizzlies-logo.png"),          paste0(logo_base_url,"miami-heat-logo-symbol.png"),
           paste0(logo_base_url,"nba-milwaukee-bucks-logo.png"),            paste0(logo_base_url,"minnesota-timberwolves-logo-symbol.png"),
           paste0(logo_base_url,"new-orleans-pelicans-logo-symbol.png"),    paste0(logo_base_url,"nba-new-york-knicks-logo.png"),
           paste0(logo_base_url,"oklahoma-city-thunder-logo-symbol.png"),   paste0(logo_base_url,"orlando-magic-logo-symbol.png"),
           paste0(logo_base_url,"nba-philadelphia-76ers-logo.png"),         paste0(logo_base_url,"phoenix-suns-logo-symbol.png"),
           paste0(logo_base_url,"portland-trail-blazers-logo-symbol.png"),  paste0(logo_base_url,"nba-sacramento-kings-logo.png"),
           paste0(logo_base_url,"san-antonio-spurs-logo-symbol.png"),       paste0(logo_base_url,"nba-toronto-raptors-logo-2020.png"),
           paste0(logo_base_url,"utah-jazz-logo-symbol.png"),               paste0(logo_base_url,"washington-wizards-logo-symbol.png")),
COLOR_PRIMARY =    c("#E03A3E","#000000","#007A33","#00788C","#CE1141","#860038","#002B5E","#FEC524","#1D42BA","#FFC72C",
                     "#CE1141","#FDBB30","#1D428A","#552583","#5D76A9","#9A002A","#00471B","#0C2340","#85714D","#F58426",
                     "#007AC1","#0077C0","#006BB6","#E56020","#E03A3E","#5A2D81","#C4CED4","#CE1141","#002B5C","#002B5C"),
COLOR_SECONDARY =  c("#26282A","#FFFFFF","#000000","#1D1160","#FFFFFF","#FDBB30","#00538C","#0E2240","#C8102E","#1D428A",
                     "#000000","#002D62","#C8102E","#FDB927","#12173F","#F9A01B","#EEE1C6","#236192","#C8102E","#006BB6",
                     "#EF3B24","#C4CED4","#ED174C","#1D1160","#000000","#63727A","#000000","#000000","#00471B","#E31837"))

#' theme_colors
#' @export
theme_colors <- c(`text` = "gray88", `text_axis` = "gray77", `text_red` = "#FFC9CB", `text_green` = "#BFFFD0", `background_panel` = "gray28", `background_plot` = "gray22",
                  `grid_lines` = "gray44")

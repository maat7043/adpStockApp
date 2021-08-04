library(ffscrapr)
library(tidyverse)

# Load Cached Objects
load("adpData.RData")
adpObject <- readRDS("data/appData.rds")
processedLeagues <- readRDS("data/processedLeagues.rds")

# Establish Season for API
conn <- mfl_connect(season = 2021)

# Get raw ADP
raw_adp <- mfl_getendpoint(
  conn = conn,
  endpoint = "adp",
  PERIOD = "DRAFT",
  IS_MOCK = 0,
  DETAILS = 1
)

# Get players
players <- mfl_players()

# Get list of league objects
leagues <- raw_adp$content$adp$leagues$league

# # Initialize Tibble()
# adpObject <- tibble(
#   timestamp = as.POSIXct(NA),
#   adp = numeric(),
#   player_id = character(),
#   franchise_count = integer(),
#   qb_type = character(),
#   idp = logical(),
#   scoring_flags = character(),
#   best_ball = logical(),
#   salary_cap = logical(),
#   player_copies = integer(),
#   draft_type = character(),
#   league_id = character()
# )
# 
# processedLeagues <- tibble(league_id = character())

# Loop through leagues
for (l in leagues) {
  # Check to see if league has already been processed
  if (l$id %in% processedLeagues$league_id == FALSE){
    private = FALSE
    
    tryCatch(
      {
        conn <- mfl_connect(
          season = 2021,
          league_id = l$id,
          user_agent = "maatspencer",
          rate_limit_number = 2,
          rate_limit_seconds = 5
          )
        draft <- ff_draft(conn)
        settings <- ff_league(conn)
      },
      error=function(cond) {
        message(paste("This league seems to be private:", l$id))
        # Skip League
        private = TRUE
      }
    )
    
    if (private == FALSE) {
      # Get League Specifics
      l.draft_type <- NULL
      l.franchise_count <-settings$franchise_count
      l.qb_type <- settings$qb_type
      l.idp <- settings$idp
      l.scoring_flags <- settings$scoring_flags
      l.best_ball <- settings$best_ball
      l.salary_cap <- settings$salary_cap
      l.player_copies <- settings$player_copies
      
      # Show success in log
      message(paste(l$id, "Has been added"))
      
      # add row to tibble
      for (i in 0:(dim(draft)[1]-1)) {
        adpObject <- adpObject %>% add_row(
          timestamp = draft$timestamp[i],
          adp = draft$overall[i]/l.player_copies,
          player_id = draft$player_id[i],
          franchise_count = l.franchise_count,
          qb_type = l.qb_type,
          idp = l.idp,
          scoring_flags = l.scoring_flags,
          best_ball = l.best_ball,
          salary_cap = l.salary_cap,
          player_copies = l.player_copies,
          draft_type = l.draft_type,
          league_id = l$id
        )
      }
    }
    
    # store League ID for caching
    processedLeagues <- processedLeagues %>% add_row(
      league_id = l$id
    )
  }
}

# Remove custom players
adpObject <- filter(adpObject, player_id != "" & is.null(player_id) == FALSE & player_id != "0000" & player_id != "----")

# Remove Duplicate Rows
adpObject <- distinct(adpObject)
processedLeagues <- distinct(processedLeagues)

# sort, add name/pos, filter positons
adpObject <- adpObject %>%
  arrange(timestamp) %>%
  left_join(players, by = "player_id") %>%
  select(timestamp, adp, player_id, franchise_count, qb_type, idp, scoring_flags, best_ball,
         player_copies, league_id, draft_type, player_name, pos) %>%
  filter(pos %in% c('WR','QB','TE','RB')) 

# count times player was drafted
counts <- adpObject %>%
  count(player_id)

# join and filter
adpObject <- adpObject %>%
  left_join(counts, by = "player_id") %>%
  filter(n >= 100)

# Change timestamp to date
adpObject$timestamp <- as.Date(adpObject$timestamp)

# Save Cached Objects
write_rds(adpObject, "data/appData.rds", compress = "gz")
write_rds(processedLeagues, "data/processedLeagues.rds")
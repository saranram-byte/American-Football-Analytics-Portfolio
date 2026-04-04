# ============================================
# WR Combine & Draft Capital Study — Part 1
# Historical EDA: Athleticism vs Draft Capital
# Data: nflreadr combine data, 2010-2026
# Author: [Saran Ram]
# Date: April 2026
# ============================================

# install.packages(c("nflreadr", "tidyverse", "ggplot2", "janitor"))

library(nflreadr)
library(tidyverse)
library(janitor)

# Load combine data
combine_raw <- load_combine()

# Quick look at what we have
glimpse(combine_raw)


combine_raw |> filter(pos == "WR") |> glimpse()

# Checking to see how many WRs are in each draft class w/ combine data
combine_raw |>
  filter(pos == "WR") |> 
  count(season) |>
  print(n = Inf) # Allows for max number of rows to all be printed w/o being explicitly mentioned


# Step 1: Starting the Pipeline
wr_clean <- combine_raw |> 
  filter(pos == "WR", season >= 2010)# Chose "season" over "draft_year" (draft_year had NAs)

nrow(wr_clean)

wr_clean |> count(season)


wr_clean <- combine_raw |>
  filter(pos == "WR", season >= 2010) |>
  mutate(draft_status = if_else(is.na(draft_round),"UDFA", "DRAFTED"),
         draft_round_clean = if_else(is.na(draft_round), "UDFA", as.character(draft_round)),
         draft_ovr_clean = if_else(is.na(draft_ovr), 999, draft_ovr))

# General side by side of the raw value row vs. the cleaned row to compare
wr_clean |> 
  select(player_name, draft_round, draft_round_clean, draft_ovr, draft_ovr_clean) |> 
  head(20)


# Now for adding/converting the "6-2" height into inches

wr_clean <- combine_raw |>
  filter(pos == "WR", season >= 2010) |>
  mutate(draft_status = if_else(is.na(draft_round),"UDFA", "DRAFTED"),
         draft_round_clean = if_else(is.na(draft_round), "UDFA", as.character(draft_round)),
         draft_ovr_clean = if_else(is.na(draft_ovr), 999, draft_ovr),
         ht_inches = map_dbl(ht, function(x) {
           parts <- str_split(x, "-")[[1]]
           feet = as.numeric(parts[1]) * 12
           inches = as.numeric(parts[2])
           ht_inches = feet + inches
         }))
         
# Verify the measurment was converted properly
wr_clean |> 
  select(player_name, ht, ht_inches) |> 
  head(10)


# Went in and added all necessary rows into the select() portion of our pipeline
wr_clean <- combine_raw |>
  filter(pos == "WR", season >= 2010) |>
  mutate(draft_status = if_else(is.na(draft_round),"UDFA", "DRAFTED"),
         draft_round_clean = if_else(is.na(draft_round), "UDFA", as.character(draft_round)),
         draft_ovr_clean = if_else(is.na(draft_ovr), 999, draft_ovr),
         ht_inches = map_dbl(ht, function(x) {
           parts <- str_split(x, "-")[[1]]
           feet = as.numeric(parts[1]) * 12
           inches = as.numeric(parts[2])
           ht_inches = feet + inches
         })) |>
  select(player_name,season,school, draft_status, draft_round_clean, draft_ovr_clean, ht_inches, wt, cone, shuttle, forty, vertical, broad_jump)

# Final glimpse to check
glimpse(wr_clean)
         
  
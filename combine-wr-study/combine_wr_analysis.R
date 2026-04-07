# ============================================
# WR Combine & Draft Capital Study — Part 1
# Historical EDA: Athleticism vs Draft Capital
# Data: nflreadr combine data, 2010-2026
# Author: [Saran Ram]
# Date: April 2026
# ============================================

# install.packages(c("nflreadr", "tidyverse", "ggplot2", "janitor"))


#install.packages("showtext")
library(showtext)
font_add_google("Inter", "inter")
showtext_auto()
showtext_opts(dpi = 300)


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



# Addition of feature engineered variables for future analysis

wr_clean <- combine_raw |>
  filter(pos == "WR", season >= 2010) |>
  mutate( ht_inches = map_dbl(ht, function(x) {
    parts <- str_split(x, "-")[[1]]
    feet = as.numeric(parts[1]) * 12
    inches = as.numeric(parts[2])
    ht_inches = feet + inches
  }),draft_status = if_else(is.na(draft_round),"UDFA", "DRAFTED"),
         draft_round_clean = if_else(is.na(draft_round), "UDFA", as.character(draft_round)),
         draft_ovr_clean = if_else(is.na(draft_ovr), 999, draft_ovr),
         speed_score = (wt*200) / (forty^4),
         explosion_index = vertical + broad_jump,
         bmi = (wt / (ht_inches ^ 2)) * 703
        ) |>
  select(player_name,season,school, draft_status, draft_round_clean, draft_ovr_clean, ht_inches, wt, cone, shuttle, forty, vertical, broad_jump, speed_score, explosion_index, bmi)


# Verify the feature engineered metrics work
wr_clean |> 
  select(player_name, forty, wt, ht_inches, speed_score, explosion_index, bmi) |> 
  head(10)


# Final glimpse to check
glimpse(wr_clean)


## VIZ BUILDER

# Signature Colors
base_dark <- "#2b2d42"    # charcoal - primary bars
base_light <- "#f8f9fa"   # off white - background
wr_accent <- "#06d6a0"    # mint green - WR series accent
wr_gray <- "#6c757d"      # gray - subtitle/caption text


theme_football <- function() {
  theme_minimal(base_family = "inter") +
    theme(
      # Background
      plot.background = element_rect(fill = wr_light, color = NA),
      panel.background = element_rect(fill = wr_light, color = NA),
      
      # Title styling
      plot.title = element_text(
        face = "bold", size = 16, color = wr_blue,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = 11, color = wr_gray,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = 8, color = wr_gray,
        hjust = 1, margin = margin(t = 10)
      ),
      
      # Axis styling
      axis.title.x = element_text(
        face = "bold", size = 10, color = wr_blue,
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        face = "bold", size = 10, color = wr_blue,
        margin = margin(r = 12)
      ),
      axis.text = element_text(size = 9, color = wr_gray),
      
      # Grid lines - horizontal only, very subtle
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.4),
      
      # Plot margins
      plot.margin = margin(20, 20, 20, 20)
    )
}




# DRAFT ROUND DISTRIBUTION PLOT #


wr_clean |>
  filter(season >= 2010, season <= 2024) |>
  mutate(
    draft_round_clean = factor(draft_round_clean,
                               levels = c("1","2","3","4","5","6","7","UDFA")),
    is_udfa = draft_round_clean == "UDFA"
  ) |>
  ggplot(aes(x = draft_round_clean, fill = is_udfa)) +
  geom_bar() +
  scale_fill_manual(
    values = c("FALSE" = wr_blue, "TRUE" = wr_green),
    guide = "none"
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 4.5
  ) +
  labs(
    title = "WR Draft Round Distribution (2010-2024)",
    subtitle = "Does the combine create hype that distorts draft decisions?",
    caption = "Data: nflreadr | 2010-2024 NFL Combine",
    x = "Draft Round Selected",
    y = "Player Count"
  ) +
  theme_football() +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = -Inf, yend = -Inf,
           color = wr_green, linewidth = 3)

# PNG SAVE
showtext_opts(dpi = 300)
ggsave("chart1_draft_distribution.png",
       width = 10, height = 6, dpi = 300,
       bg = base_light)


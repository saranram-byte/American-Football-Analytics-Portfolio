# ============================================
# WR Combine & Draft Capital Study — Part 2
# Predictive Modeling: Combine -> Draft Position
# Data: nflreadr combine data, 2010-2026
# Author: [Saran Ram]
# Date: April 2026
# ============================================

library(ggrepel)
library(tidymodels)
library(nflreadr)
library(tidyverse)
library(showtext)
library(gt)

font_add_google("Inter", "inter")
showtext_auto()
showtext_opts(dpi = 300)

# Signature Colors
base_dark <- "#2b2d42"
base_light <- "#f8f9fa"
wr_accent <- "#06d6a0"
wr_gray <- "#6c757d"

theme_football <- function() {
  theme_minimal(base_family = "inter") +
    theme(
      plot.background = element_rect(fill = base_light, color = NA),
      panel.background = element_rect(fill = base_light, color = NA),
      plot.title = element_text(face = "bold", size = 16, color = base_dark, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, color = wr_gray, margin = margin(b = 15)),
      plot.caption = element_text(size = 8, color = wr_gray, hjust = 1, margin = margin(t = 10)),
      axis.title.x = element_text(face = "bold", size = 10, color = base_dark, margin = margin(t = 12)),
      axis.title.y = element_text(face = "bold", size = 10, color = base_dark, margin = margin(r = 12)),
      axis.text = element_text(size = 9, color = wr_gray),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.4),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# ============================================
# Data
# ============================================

combine_raw <- load_combine()

wr_clean <- combine_raw |>
  filter(pos == "WR", season >= 2010) |>
  mutate(
    ht_inches = map_dbl(ht, function(x) {
      parts <- str_split(x, "-")[[1]]
      feet = as.numeric(parts[1]) * 12
      inches = as.numeric(parts[2])
      ht_inches = feet + inches
    }),
    draft_status = if_else(is.na(draft_round), "UDFA", "DRAFTED"),
    draft_round_clean = if_else(is.na(draft_round), "UDFA", as.character(draft_round)),
    draft_ovr_clean = if_else(is.na(draft_ovr), 999, draft_ovr),
    speed_score = (wt * 200) / (forty ^ 4),
    explosion_index = vertical + broad_jump,
    explosion_adjusted = (vertical + broad_jump) / (wt ^ 0.5),
    bmi = (wt / (ht_inches ^ 2)) * 703
  ) |>
  select(player_name, season, school, draft_status, draft_round_clean,
         draft_ovr_clean, ht_inches, wt, cone, shuttle, forty,
         vertical, broad_jump, speed_score, explosion_index,
         explosion_adjusted, bmi)

glimpse(wr_clean)

# ============================================
# Model Data Prep
# ============================================

# Training: 2010-2023
# Validation: 2024 (known results)
# Prediction: 2025 (known results, retrospective)
# Prediction: 2026 (draft just happened)

model_data <- wr_clean |>
  filter(
    draft_status == "DRAFTED",
    season >= 2010,
    season <= 2023,
    !is.na(speed_score),
    !is.na(explosion_adjusted),
    !is.na(ht_inches)
  )

validation_data <- wr_clean |>
  filter(
    draft_status == "DRAFTED",
    season == 2024,
    !is.na(speed_score),
    !is.na(explosion_adjusted),
    !is.na(ht_inches)
  )

prediction_2025 <- wr_clean |>
  filter(
    season == 2025,
    !is.na(speed_score),
    !is.na(explosion_adjusted),
    !is.na(ht_inches)
  )

prediction_2026 <- wr_clean |>
  filter(
    season == 2026,
    !is.na(speed_score),
    !is.na(explosion_adjusted),
    !is.na(ht_inches)
  )

# Check sizes
nrow(model_data)
nrow(validation_data)
nrow(prediction_2025)
nrow(prediction_2026)

# ============================================
# Model Creation / Workflow
# ============================================

# Recipe — defines features and preprocessing
wr_model_recipe <- recipe(draft_ovr_clean ~ speed_score + explosion_adjusted + ht_inches,
                          data = model_data) |>
  step_naomit(all_predictors())

summary(wr_model_recipe)

# Model specification — linear regression
wr_model_spec <- linear_reg() |>
  set_engine("lm")

# Workflow — combines recipe and model, fits to training data
wr_model_workflow <- workflow() |>
  add_recipe(wr_model_recipe) |>
  add_model(wr_model_spec) |>
  fit(data = model_data)

# Coefficients
tidy(wr_model_workflow)
# speed_score (-2.545) — most reliable predictor (p = 0.00000001)
# explosion_adjusted (-13.547) — largest effect size (p = 0.018)
# ht_inches (4.123) — modest but significant (p = 0.036)

# Overall model fit
glance(wr_model_workflow)
# Adjusted R-squared: 0.112 — model explains 11.2% of draft position variance
# The other 88.8% lives in film rooms, not Indianapolis

# ============================================
# Model Validation
# ============================================

# 2024 validation
wr_predictions <- augment(wr_model_workflow, new_data = validation_data)

wr_predictions |>
  select(player_name, season, draft_ovr_clean, .pred) |>
  arrange(draft_ovr_clean) |>
  print(n = Inf)

wr_predictions |>
  metrics(truth = draft_ovr_clean, estimate = .pred)
# RMSE: 68.9 | R-squared: 0.100 | MAE: 59.8

# 2025 retrospective
predictions_2025 <- augment(wr_model_workflow, new_data = prediction_2025)

predictions_2025 |>
  select(player_name, season, draft_ovr_clean, .pred) |>
  arrange(.pred) |>
  print(n = Inf)

predictions_2025 |>
  filter(draft_ovr_clean != 999) |>
  mutate(
    gap = draft_ovr_clean - .pred,
    gap_type = if_else(gap < 0, "Reached", "Undervalued")
  ) |>
  arrange(gap) |>
  print(n = Inf)

predictions_2025 |>
  filter(draft_ovr_clean != 999) |>
  metrics(truth = draft_ovr_clean, estimate = .pred)
# RMSE: 67.1 | R-squared: 0.256 | MAE: 56.3

# ============================================
# Combined Validation Chart (2024 + 2025)
# ============================================

combined_validation <- bind_rows(
  wr_predictions,
  predictions_2025
) |>
  filter(draft_ovr_clean != 999) |>
  mutate(
    gap = draft_ovr_clean - .pred,
    gap_type = if_else(gap < 0, "Reached", "Undervalued")
  )

labels_data <- combined_validation |>
  filter(player_name %in% c(
    "Jayden Higgins",
    "Dominic Lovett",
    "Rome Odunze",
    "Joshua Lane",
    "Cleveland Harris",
    "Jimmy Horn Jr."
  ))

ggplot(combined_validation, aes(x = .pred, y = draft_ovr_clean)) +
  geom_point(aes(color = gap_type)) +
  geom_text_repel(
    data = labels_data,
    aes(label = player_name),
    size = 3,
    color = base_dark
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = wr_gray) +
  scale_color_manual(
    values = c("Reached" = "#e63946", "Undervalued" = wr_accent),
    name = "Player Type"
  ) +
  theme_football() +
  labs(
    title = "Model Validation: Predicted vs Actual Draft Pick (2024-2025)",
    subtitle = "Points below the line were drafted earlier than the model projected",
    caption = "Data: nflreadr | 2024-2025 NFL Draft | Dashed line = perfect prediction",
    x = "Projected Pick (Model)",
    y = "Actual Draft Pick"
  ) +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = -Inf, yend = -Inf,
           color = wr_accent, linewidth = 3)

showtext_opts(dpi = 300)
ggsave("chart6_model_validation.png",
       width = 10, height = 8, dpi = 300,
       bg = base_light)

# ============================================
# 2026 Draft Board
# ============================================

# Load actual 2026 draft results
draft_2026 <- load_draft_picks(seasons = 2026)

wr_draft_2026 <- draft_2026 |>
  filter(position == "WR") |>
  select(pfr_player_name, round, pick)

# Clean names for joining
predictions_2026 <- augment(wr_model_workflow, new_data = prediction_2026)

predictions_2026_clean <- predictions_2026 |>
  mutate(name_clean = clean_player_names(player_name))

draft_2026_clean <- wr_draft_2026 |>
  mutate(name_clean = clean_player_names(pfr_player_name))

# Build rankings
wr_rankings_2026 <- predictions_2026_clean |>
  mutate(model_rank = rank(.pred, ties.method = "first")) |>
  left_join(draft_2026_clean, by = "name_clean") |>
  mutate(
    actual_pick = case_when(
      player_name == "De'Zhaun-Ryan Stribling" ~ 33,
      is.na(pick) ~ 999,
      TRUE ~ as.numeric(pick)
    ),
    actual_wr_rank = if_else(
      actual_pick == 999,
      NA_real_,
      rank(actual_pick, ties.method = "first")
    ),
    result = case_when(
      actual_pick == 999 ~ "Undrafted",
      actual_pick <= 100 ~ "Day 1/2 Pick",
      actual_pick <= 200 ~ "Day 3 Pick",
      TRUE ~ "Late Round"
    )
  ) |>
  # Fix Reggie Virgil name mismatch
  mutate(
    actual_pick = case_when(
      player_name == "Reggie Virgil" ~ 143,
      TRUE ~ actual_pick
    ),
    gap = actual_pick - .pred,
    result = case_when(
      actual_pick == 999 ~ "Undrafted",
      actual_pick <= 100 ~ "Day 1/2 Pick",
      actual_pick <= 200 ~ "Day 3 Pick",
      TRUE ~ "Late Round"
    )
  ) |>
  # Add UDFA team info
  mutate(
    udfa_team = case_when(
      player_name == "Jeff Caldwell" ~ "Kansas City Chiefs",
      player_name == "J. Michael Sturdivant" ~ "Green Bay Packers",
      player_name == "Eric Rivers" ~ "Tampa Bay Buccaneers",
      player_name == "Caullin Lacy" ~ "New York Jets",
      player_name == "Chris Hilton" ~ "Dallas Cowboys",
      player_name == "Jalen Walthall" ~ "Houston Texans",
      player_name == "Dillon Bell" ~ "Minnesota Vikings",
      player_name == "Chase Roberts" ~ "Las Vegas Raiders",
      actual_pick != 999 ~ "Drafted",
      TRUE ~ "Undrafted - No Deal"
    )
  ) |>
  arrange(model_rank)

wr_rankings_2026 |>
  select(player_name, model_rank, actual_pick, result, udfa_team) |>
  print(n = Inf)

# ============================================
# Draft Board Table
# ============================================

draft_board_table <- wr_rankings_2026 |>
  select(model_rank, player_name, actual_pick, result, udfa_team) |>
  arrange(model_rank) |>
  mutate(
    actual_pick = ifelse(is.na(actual_pick), "UDFA", as.character(actual_pick))
  )

draft_board_table |>
  gt() |>
  cols_label(
    model_rank = "Model Rank",
    player_name = "Player",
    actual_pick = "Actual Pick",
    result = "Draft Result",
    udfa_team = "Team/Status"
  ) |>
  opt_table_font(font = "Arial") |>
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "#d0d0d0",
      weight = px(1)
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_borders(
      sides = "all",
      color = base_dark,
      weight = px(1.5)
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_fill(color = wr_accent, alpha = 0.25),
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = result == "Undrafted")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#e63946", alpha = 0.2),
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = player_name == "De'Zhaun-Ryan Stribling")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = base_dark),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_header(
    title = md("**2026 WR Draft Board — Model Rankings vs Reality**"),
    subtitle = "Ranked by athletic profile. Does draft capital follow the data?"
  ) |>
  tab_options(
    table.border.top.color = base_dark,
    table.border.bottom.color = base_dark,
    column_labels.border.bottom.color = base_dark,
    row.striping.include_table_body = FALSE,
    table.background.color = base_light,
    heading.background.color = base_light,
    table.font.size = px(13),
    heading.title.font.size = px(17),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(12),
    column_labels.font.size = px(12),
    data_row.padding = px(7),
    column_labels.padding = px(9),
    heading.padding = px(10)
  ) |>
  gtsave("chart7_draft_board_2026.png")

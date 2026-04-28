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
         bmi = (wt / (ht_inches ^ 2)) * 703,
        explosion_adjusted = (vertical + broad_jump) / (wt ^ 0.5)
        ) |>
  select(player_name,season,school, draft_status, draft_round_clean, draft_ovr_clean, ht_inches, wt, cone, shuttle, forty, vertical, broad_jump, speed_score, explosion_index, explosion_adjusted, bmi)


# Verify the feature engineered metrics work
wr_clean |> 
  select(player_name, forty, wt, ht_inches, speed_score, explosion_index,explosion_adjusted, bmi) |> 
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
      plot.background = element_rect(fill = base_light, color = NA),
      panel.background = element_rect(fill = base_light, color = NA),
      plot.title = element_text(
        face = "bold", size = 16, color = base_dark,
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
      axis.title.x = element_text(
        face = "bold", size = 10, color = base_dark,
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        face = "bold", size = 10, color = base_dark,
        margin = margin(r = 12)
      ),
      axis.text = element_text(size = 9, color = wr_gray),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.4),
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
    values = c("FALSE" = base_dark, "TRUE" = wr_accent),
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
           color = wr_accent, linewidth = 3)

# Export Chart 1
showtext_opts(dpi = 300)
ggsave("chart1_draft_distribution.png",
       width = 10, height = 6, dpi = 300,
       bg = base_light)

# CHART 2: 40 Time by Draft Round #
wr_clean |>
  filter(season >= 2010, season <= 2024, !is.na(forty)) |>
  mutate(
    draft_round_clean = factor(draft_round_clean,
                               levels = c("1","2","3","4","5","6","7","UDFA")),
    is_round1 = draft_round_clean == "1"
  ) |>
  ggplot(aes(x = draft_round_clean, y = forty, fill = is_round1)) +
  geom_boxplot(
    color = base_dark,
    linewidth = 0.5,
    outlier.color = base_dark,
    outlier.size = 1.5,
    outlier.alpha = 0.6,
    median.linewidth = 1.5
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5,
    size = 3,
    fontface = "bold",
    color = base_dark
  ) +
  scale_y_reverse() +
  scale_fill_manual(
    values = c("FALSE" = "#a8dadc", "TRUE" = wr_accent),
    guide = "none"
  ) +
  labs(
    title = "WR 40 Time Distribution by Draft Round (2010-2024)",
    subtitle = " 0.04 seconds. That's all that separates Round 1 WRs from Round 2. Is speed a smokescreen?",
    caption = "Data: nflreadr | 2010-2024 NFL Combine",
    x = "Draft Round Selected",
    y = "40 Yard Dash Time (seconds)"
  ) +
  theme_football() +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = -Inf, yend = -Inf,
           color = wr_accent, linewidth = 3)

# Export Chart 2
showtext_opts(dpi = 300)
ggsave("chart2_forty_by_round.png",
       width = 10, height = 6, dpi = 300,
       bg = base_light)






# CHART 3: Speed Score vs. Draft Round
wr_clean |>
  filter(season >= 2010, season <= 2024, !is.na(speed_score)) |>
  mutate(
    draft_round_clean = factor(draft_round_clean,
                               levels = c("1","2","3","4","5","6","7","UDFA")),
    is_round1 = draft_round_clean == "1"
  ) |>
  ggplot(aes(x = draft_round_clean, y = speed_score, fill = is_round1)) +
  geom_boxplot(
    color = base_dark,
    linewidth = 0.5,
    outlier.color = base_dark,
    outlier.size = 1.5,
    outlier.alpha = 0.6,
    median.linewidth = 1.5
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5,
    size = 3,
    fontface = "bold",
    color = base_dark
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#a8dadc", "TRUE" = wr_accent),
    guide = "none"
  ) +
  labs(
    title = "WR Speed Score Distribution by Draft Round (2010-2024)",
    subtitle = "Raw speed is just the start. Size-speed combo tells a deeper story",
    caption = "Data: nflreadr | 2010-2024 NFL Combine\nSpeed Score = (Weight × 200) / (40 Time⁴) | Higher = better size-adjusted speed",
    x = "Draft Round Selected",
    y = "Speed Score"
  ) +
  theme_football() +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = -Inf, yend = -Inf,
           color = wr_accent, linewidth = 3)

# Export Chart 3
showtext_opts(dpi = 300)
ggsave("chart3_speed_score_by_round.png",
       width = 10, height = 6, dpi = 300,
       bg = base_light)




# CHART 4: Explosion Score vs. Draft Round
wr_clean |>
  filter(season >= 2010, season <= 2024, !is.na(explosion_index)) |>
  mutate(
    draft_round_clean = factor(draft_round_clean,
                               levels = c("1","2","3","4","5","6","7","UDFA")),
    is_round1 = draft_round_clean == "1"
  ) |>
  ggplot(aes(x = draft_round_clean, y = explosion_index, fill = is_round1)) +
  geom_boxplot(
    color = base_dark,
    linewidth = 0.5,
    outlier.color = base_dark,
    outlier.size = 1.5,
    outlier.alpha = 0.6,
    median.linewidth = 1.5
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5,
    size = 3,
    fontface = "bold",
    color = base_dark
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#a8dadc", "TRUE" = wr_accent),
    guide = "none"
  ) +
  labs(
    title = "WR Explosion Index Distribution by Draft Round (2010-2024)",
    subtitle = "Is the applied events what really make the difference?",
    caption = "Data: nflreadr | 2010-2024 NFL Combine\nExplosion Index = Vertical Jump + Broad Jump | Higher = better lower body power",
    x = "Draft Round Selected",
    y = "Explosion Index Score"
  ) +
  theme_football() +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = -Inf, yend = -Inf,
           color = wr_accent, linewidth = 3)

# Export Chart 4
showtext_opts(dpi = 300)
ggsave("chart4_explosion_index_by_round.png",
       width = 10, height = 6, dpi = 300,
       bg = base_light)




# CHART 4b: Explosion Score adjusted vs. Draft Round
wr_clean |>
  filter(season >= 2010, season <= 2024, !is.na(explosion_adjusted)) |>
  mutate(
    draft_round_clean = factor(draft_round_clean,
                               levels = c("1","2","3","4","5","6","7","UDFA")),
    is_round1 = draft_round_clean == "1"
  ) |>
  ggplot(aes(x = draft_round_clean, y = explosion_adjusted, fill = is_round1)) +
  geom_boxplot(
    color = base_dark,
    linewidth = 0.5,
    outlier.color = base_dark,
    outlier.size = 1.5,
    outlier.alpha = 0.6,
    median.linewidth = 1.5
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5,
    size = 3,
    fontface = "bold",
    color = base_dark
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#a8dadc", "TRUE" = wr_accent),
    guide = "none"
  ) +
  labs(
    title = "WR Weight-Adjusted Explosion by Draft Round (2010-2024)",
    subtitle = "Lower body power matters. But no single metric tells the whole story",
    caption = "Data: nflreadr | 2010-2024 NFL Combine\nExplosion Adjusted = (Vertical + Broad Jump) / √Weight | Higher = better weight-adjusted explosion",
    x = "Draft Round Selected",
    y = "Explosion Adjusted Index Score"
  ) +
  theme_football() +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = -Inf, yend = -Inf,
           color = wr_accent, linewidth = 3)

# Export Chart 4b
showtext_opts(dpi = 300)
ggsave("chart4b_explosion_adjusted_by_round.png",
       width = 10, height = 6, dpi = 300,
       bg = base_light)



# Before we go into modeling, we need to create a correlation heatmap and how it relates to draft position


cor_data <- wr_clean |>
  filter(draft_status == "DRAFTED", season >= 2010, season <= 2024) |>
  select(draft_ovr_clean, forty, speed_score, explosion_index, 
         explosion_adjusted, vertical, broad_jump, ht_inches, wt, bmi,
         cone, shuttle) |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("metric1") |>
  pivot_longer(-metric1, names_to = "metric2", values_to = "correlation")


cor_data |>
  filter(metric1 == "draft_ovr_clean") |>
  arrange(correlation)

# We get our R correlation values 

# Negative Values mean it has a stronger correlation and vice versa

# We can calculate the variance by take the correlation value which is the R value and squaring it
# Speed Score explains (-0.253)^2 = 0.064 ~ 6.4% variance explained


# Visualize this with a correlation heat map 

cor_data |>
  ggplot(aes(x = metric1, y = metric2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(correlation, 2)), 
            size = 2.5, color = base_dark) +
  scale_fill_gradient2(
    low = "#e63946",
    mid = "white",
    high = "#06d6a0",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    title = "Combine Metric Correlation Matrix",
    subtitle = "Every metric outpredicts the raw 40. The combine's biggest star is its weakest signal",
    caption = "Data: nflreadr | 2010-2024 NFL Combine",
    x = NULL,
    y = NULL
  ) +
  theme_football() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank()
  ) +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = -Inf, yend = -Inf,
           color = wr_accent, linewidth = 3)


showtext_opts(dpi = 300)
ggsave("chart5_correlation_heatmap.png",
       width = 10, height = 10, dpi = 300,
       bg = base_light)


# With this graph we need to extract and identify insights

# Key Takeaways in correlation with draft_ovr_clean:
# Speed Score(-0.25) is the strongest predictor 
# Explosion Index(-0.23) Second strongest predictor
# Forty(0.28) is surprisingly the weakest predictor! 



# Multicollinearity 
# Important to look into to avoid redundancy in the future model creation. 
# Explosion Index and Broad Jump(0.96) is nearly identical so will have to remove one for the model in next steps
# Explosion Index and Vertical Jump(0.82) also heavily overlaps 
# Forty and Speed Score(-0.60) expected since speed scores use the 40


# Should we consider adding an agility drill to the analysis or correlation heat map? 
wr_clean |>
  filter(draft_status == "DRAFTED", season >= 2010, season <= 2024) |>
  summarise(across(c(forty, speed_score, explosion_adjusted, 
                     ht_inches, cone, shuttle, bmi, wt), 
                   ~sum(is.na(.)), 
                   .names = "na_{.col}")) |>
  pivot_longer(everything(), 
               names_to = "metric", 
               values_to = "na_count") |>
  mutate(
    total = nrow(wr_clean |> filter(draft_status == "DRAFTED", season >= 2010, season <= 2024)),
    pct_missing = round(na_count / total * 100, 1)
  ) |>
  arrange(desc(pct_missing))

# With cone and shuttle being at 42% and 39.6% respectively I believe there is too much missing data to have any significance in the model or to further look into


# Final Feature Set for Modeling:
# speed_score: best single predictor (R = -0.253)
# explosion_adjusted: original engineered feature, weight-normalized explosion
# ht_inches:  independent size signal, near complete data
# Dropped:forty (redundant with speed_score), cone/shuttle (40%+ missing),
# explosion_index/vertical/broad_jump: (redundant with explosion_adjusted),
# bmi/wt: (near zero correlation or captured elsewhere)



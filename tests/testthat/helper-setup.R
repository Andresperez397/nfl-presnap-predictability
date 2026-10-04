# testthat runs from tests/testthat; source the project code and point it at the project data.
ROOT <- normalizePath(file.path("..", ".."))
for (f in list.files(file.path(ROOT, "R"), full.names = TRUE)) source(f, local = TRUE)
RAW_DIR <- file.path(ROOT, "data", "raw")
PGRP_MIN <- 100 # the synthetic data is small; real data uses 1,000
has_data <- file.exists(file.path(RAW_DIR, "play_by_play_2024.parquet"))

# Small synthetic play table with the columns the models need.
fake_plays <- function(n = 4000, seasons = 2017:2019, seed = 1) {
  set.seed(seed)
  d <- data.frame(
    season = sample(seasons, n, TRUE), week = sample(1:18, n, TRUE),
    down = sample(1:4, n, TRUE, prob = c(0.45, 0.32, 0.2, 0.03)), ydstogo = sample(1:20, n, TRUE),
    yardline_100 = sample(1:99, n, TRUE), qtr = sample(1:4, n, TRUE),
    half_seconds_remaining = sample(0:1800, n, TRUE), score_differential = sample(-21:21, n, TRUE),
    posteam_timeouts_remaining = sample(0:3, n, TRUE), defteam_timeouts_remaining = sample(0:3, n, TRUE),
    home = sample(0:1, n, TRUE), shotgun = sample(0:1, n, TRUE), no_huddle = rbinom(n, 1, 0.1),
    n_rb = sample(0:2, n, TRUE), n_te = sample(0:3, n, TRUE), n_wr = sample(1:4, n, TRUE),
    heavy_ol = rbinom(n, 1, 0.04), team = sample(c("AAA", "BBB", "CCC", "DDD"), n, TRUE)
  )
  d$game_seconds_remaining <- d$half_seconds_remaining + ifelse(d$qtr <= 2, 1800, 0)
  d$goal_to_go <- as.integer(d$ydstogo >= d$yardline_100)
  d$personnel <- paste0(d$n_rb, d$n_te)
  d$dist_bucket <- cut(d$ydstogo, c(0, 3, 6, 10, Inf), labels = c("1-3", "4-6", "7-10", "11+"))
  d$situation <- factor(ifelse(d$down == 1, "1st",
                               paste0(c("", "2nd", "3rd", "4th")[d$down], " ", d$dist_bucket)))
  d$game_id <- paste(d$season, d$week, sample(1:8, n, TRUE))
  lp <- -0.3 + 0.8 * d$shotgun + 0.05 * (d$ydstogo - 10) + 0.4 * (d$down == 3) - 0.3 * d$n_te
  d$pass <- rbinom(n, 1, plogis(lp))
  add_model_columns(d)
}

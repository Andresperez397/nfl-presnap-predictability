# Data audit (run before any outcome model). Writes reports/tables/data_audit.json.
source("R/data.R")
suppressPackageStartupMessages(library(jsonlite))

out <- list()

# 1. Completeness: every completed game on the official schedule has play-by-play, and vice versa.
sched <- read_parquet(file.path(RAW_DIR, "games.parquet")) |>
  filter(season %in% SEASONS, !is.na(result))
pbp_games <- bind_rows(lapply(SEASONS, function(s) {
  read_season("play_by_play", s, c("game_id", "season")) |> distinct()
}))
out$schedule <- list(
  scheduled_completed_games = nrow(sched),
  pbp_games = nrow(pbp_games),
  scheduled_missing_from_pbp = setdiff(sched$game_id, pbp_games$game_id),
  pbp_not_on_schedule = setdiff(pbp_games$game_id, sched$game_id),
  games_by_season = as.list(table(sched$season))
)
# The schedule file has betting columns; confirm we never carry them (only game_id/season read).
out$schedule_betting_columns_present_but_unused <- grep(BETTING_PATTERN, names(sched), value = TRUE)

# 2. Cleaning log and outcome.
r <- load_plays()
d <- r$plays
out$exclusions <- r$log
out$outcome <- list(
  pass_rate_by_season = as.list(round(tapply(d$pass, d$season, mean), 4)),
  scrambles_counted_as_pass = sum(d$qb_scramble == 1, na.rm = TRUE),
  sacks_counted_as_pass = sum(d$sack == 1, na.rm = TRUE),
  dropback_missing = sum(is.na(d$pass))
)

# 3. Missingness of every input.
inputs <- c(STATE_VARS, ALIGN_VARS, PERSONNEL_VARS)
out$missing_inputs_all_seasons <- as.list(colSums(is.na(d[inputs])))
f <- d[d$season >= 2022, ]
out$missing_ftn_2022_on <- as.list(colSums(is.na(f[FTN_VARS])))

# 4. Ranges and sign conventions.
rng <- function(x) c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
out$ranges <- lapply(d[c("down", "ydstogo", "yardline_100", "qtr", "half_seconds_remaining",
                         "game_seconds_remaining", "score_differential",
                         "posteam_timeouts_remaining", "defteam_timeouts_remaining")], rng)
# score_differential is from the offense's view: it must equal posteam score - defteam score.
chk <- read_season("play_by_play", 2024, c("game_id", "play_id", "posteam_score",
                                             "defteam_score", "score_differential"))
out$score_differential_is_offense_view <- all(
  chk$score_differential == chk$posteam_score - chk$defteam_score, na.rm = TRUE)
# yardline_100 is distance to the opponent's end zone: goal_to_go plays must have ydstogo == yardline_100.
g <- d[d$goal_to_go == 1, ]
out$goal_to_go_distance_equals_yardline <- mean(g$ydstogo == g$yardline_100)

# 5. Personnel harmonization across the 2023 source change (NGS -> FTN).
out$personnel_by_season <- d |>
  group_by(season) |>
  summarise(p11 = mean(personnel == "11", na.rm = TRUE), p12 = mean(personnel == "12", na.rm = TRUE),
            p21 = mean(personnel == "21", na.rm = TRUE), heavy_ol = mean(heavy_ol, na.rm = TRUE),
            sum_not_5_skill = mean(n_rb + n_te + n_wr + pmax(n_ol - 5, 0) != 5, na.rm = TRUE)) |>
  mutate(across(-season, \(x) round(x, 4)))

# 6. Agreement between the play-by-play shotgun flag and FTN's charted QB alignment (2022+).
f$ftn_qb <- trimws(f$ftn_qb)
known <- f[f$ftn_qb %in% c("U", "S", "P"), ]
out$shotgun_vs_ftn <- list(
  n = nrow(known),
  agree = round(mean((known$shotgun == 1) == (known$ftn_qb %in% c("S", "P"))), 4),
  pistol_flagged_shotgun = round(mean(known$shotgun[known$ftn_qb == "P"] == 1), 4),
  ftn_qb_unknown = sum(!f$ftn_qb %in% c("U", "S", "P"))
)
out$ftn_rates_by_season <- f |>
  group_by(season) |>
  summarise(motion = mean(motion, na.rm = TRUE), backfield = mean(backfield_n, na.rm = TRUE),
            hash_middle = mean(hash_middle, na.rm = TRUE)) |>
  mutate(across(-season, \(x) round(x, 4)))

# 7. Team codes and sample sizes.
out$plays_per_team_season <- rng(table(paste(d$team, d$season)))
out$teams <- sort(unique(d$team))

dir.create(file.path("reports", "tables"), recursive = TRUE, showWarnings = FALSE)
write_json(out, file.path("reports", "tables", "data_audit.json"), auto_unbox = TRUE, pretty = TRUE,
           digits = NA)
cat(toJSON(out[c("schedule", "exclusions", "outcome", "missing_inputs_all_seasons",
                 "missing_ftn_2022_on", "score_differential_is_offense_view",
                 "goal_to_go_distance_equals_yardline", "shotgun_vs_ftn",
                 "plays_per_team_season")], auto_unbox = TRUE, pretty = TRUE))
print(out$personnel_by_season)
print(out$ftn_rates_by_season)

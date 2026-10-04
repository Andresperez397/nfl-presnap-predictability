# Load and clean nflverse play-by-play into one row per offensive play with only pre-snap inputs.
#
# Outcome: `pass` = 1 if the play was a called dropback (qb_dropback, which counts sacks and
# scrambles as passes), 0 if it was a designed run.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

RAW_DIR <- file.path("data", "raw")
SEASONS <- 2016:2025

# Columns read from play-by-play. Anything not listed here is never loaded, so columns outside the
# scope of the question (market-line fields) cannot enter the data by accident.
PBP_COLS <- c(
  "game_id", "play_id", "season", "season_type", "week", "game_date", "home_team", "away_team",
  "posteam", "defteam", "drive", "play_type", "qb_dropback", "qb_scramble", "aborted_play",
  "down", "ydstogo", "yardline_100", "goal_to_go", "qtr", "half_seconds_remaining",
  "game_seconds_remaining", "score_differential", "posteam_timeouts_remaining",
  "defteam_timeouts_remaining", "shotgun", "no_huddle", "epa", "success", "desc",
  "receiver_player_id", "receiver_player_name", "pass_attempt", "sack", "qb_spike", "qb_kneel"
)
OUT_OF_SCOPE_PATTERN <- "spread|total_line|vegas|moneyline|odds|over_under|_line$"

# Inputs, grouped into the nested blocks used in ANALYSIS_PLAN.md.
STATE_VARS <- c("down", "ydstogo", "yardline_100", "goal_to_go", "qtr", "half_seconds_remaining",
                "game_seconds_remaining", "score_differential", "posteam_timeouts_remaining",
                "defteam_timeouts_remaining", "home")
ALIGN_VARS <- c("shotgun", "no_huddle")
PERSONNEL_VARS <- c("n_rb", "n_te", "n_wr", "heavy_ol")
FTN_VARS <- c("motion", "backfield_n", "qb_pistol", "hash_middle", "hash_left")

# Fields that are observed after the snap. Listed so a test can prove none of them is an input.
POST_SNAP <- c("epa", "success", "qb_scramble", "sack", "pass_attempt", "receiver_player_id",
               "receiver_player_name", "yards_gained", "air_yards", "pass_location",
               "run_location", "run_gap", "pass_length", "is_play_action", "is_screen_pass",
               "is_rpo", "time_to_throw", "was_pressure", "route", "defense_coverage_type",
               "number_of_pass_rushers", "n_pass_rushers", "n_blitzers")

# Franchise relocations, so a franchise keeps one code across seasons.
FRANCHISE <- c(OAK = "LV", SD = "LAC", STL = "LA")

# nflverse files carry R class metadata, so arrow rebuilds a data.table whenever that package is
# loaded (xgboost loads it). Always return a plain data frame so indexing behaves the same.
read_season <- function(kind, season, cols = NULL) {
  path <- file.path(RAW_DIR, sprintf("%s_%d.parquet", kind, season))
  x <- if (is.null(cols)) read_parquet(path) else read_parquet(path, col_select = all_of(cols))
  as.data.frame(x)
}

# Count positions in a personnel string. Handles both formats:
#   NGS (2016-2022): "1 RB, 1 TE, 3 WR", with "6 OL, ..." only when the line is not five.
#   FTN (2023+):     "1 C, 2 G, 1 QB, 1 RB, 2 T, 1 TE, 3 WR" (every player listed by position).
parse_personnel <- function(x) {
  count <- function(pos) {
    m <- regmatches(x, regexec(paste0("(\\d+) ", pos, "(,|$)"), x))
    vapply(m, function(v) if (length(v)) as.integer(v[2]) else 0L, 0L)
  }
  ol_ngs <- count("OL")
  ol_ftn <- count("C") + count("G") + count("T")
  ol <- ifelse(ol_ftn > 0, ol_ftn, ifelse(ol_ngs > 0, ol_ngs, 5L))
  out <- data.frame(n_rb = count("RB") + count("FB"), n_te = count("TE"), n_wr = count("WR"),
                    n_ol = ol)
  out[is.na(x) | x == "", ] <- NA
  out
}

load_participation <- function(seasons) {
  bind_rows(lapply(seasons, function(s) {
    read_season("pbp_participation", s, c("nflverse_game_id", "play_id", "offense_personnel"))
  })) |>
    rename(game_id = nflverse_game_id)
}

load_ftn <- function(seasons) {
  seasons <- intersect(seasons, 2022:2025)
  if (!length(seasons)) return(NULL)
  bind_rows(lapply(seasons, function(s) {
    read_season("ftn_charting", s, c("nflverse_game_id", "nflverse_play_id", "starting_hash",
                                     "qb_location", "n_offense_backfield", "is_motion"))
  })) |>
    transmute(game_id = nflverse_game_id, play_id = as.numeric(nflverse_play_id),
              ftn_hash = starting_hash, ftn_qb = qb_location, backfield_n = n_offense_backfield,
              motion = as.integer(is_motion))
}

# Returns list(plays, log): the cleaned play table and a row-count log of each exclusion step.
load_plays <- function(seasons = SEASONS) {
  pbp <- bind_rows(lapply(seasons, function(s) read_season("play_by_play", s, PBP_COLS)))
  stopifnot(!any(grepl(OUT_OF_SCOPE_PATTERN, names(pbp))))
  log <- new.env()
  log$all_rows <- nrow(pbp)
  step <- function(d, keep, label) {
    assign(label, sum(!keep, na.rm = TRUE) + sum(is.na(keep)), envir = log)
    d[!is.na(keep) & keep, , drop = FALSE]
  }
  d <- step(pbp, pbp$play_type %in% c("pass", "run"), "not_pass_or_run")
  d <- step(d, !is.na(d$down), "no_down_two_point")
  d <- step(d, d$aborted_play %in% 0, "aborted_snap")
  d <- step(d, !grepl("Punt formation|Field Goal formation", d$desc, ignore.case = TRUE),
            "fake_kick")

  part <- load_participation(seasons)
  stopifnot(!anyDuplicated(part[c("game_id", "play_id")]))
  pers <- parse_personnel(part$offense_personnel)
  part <- bind_cols(part[c("game_id", "play_id")], pers)
  d <- left_join(d, part, by = c("game_id", "play_id"), relationship = "one-to-one")

  ftn <- load_ftn(seasons)
  if (!is.null(ftn)) {
    stopifnot(!anyDuplicated(ftn[c("game_id", "play_id")]))
    d <- left_join(d, ftn, by = c("game_id", "play_id"), relationship = "one-to-one")
  } else {
    d[c("ftn_hash", "ftn_qb", "backfield_n", "motion")] <- NA
  }

  d <- d |>
    mutate(
      pass = as.integer(qb_dropback == 1),
      home = as.integer(posteam == home_team),
      heavy_ol = as.integer(n_ol >= 6),
      team = unname(ifelse(posteam %in% names(FRANCHISE), FRANCHISE[posteam], posteam)),
      opp = unname(ifelse(defteam %in% names(FRANCHISE), FRANCHISE[defteam], defteam)),
      qb_pistol = as.integer(ftn_qb == "P"),
      hash_middle = as.integer(ftn_hash == "M"),
      # Starting hash as charted (L, M, R); FTN does not document the frame of reference.
      hash_left = as.integer(ftn_hash == "L"),
      dist_bucket = cut(ydstogo, c(0, 3, 6, 10, Inf), labels = c("1-3", "4-6", "7-10", "11+")),
      situation = factor(ifelse(down == 1, "1st",
                                paste0(c("", "2nd", "3rd", "4th")[down], " ", dist_bucket))),
      personnel = ifelse(is.na(n_rb), NA, paste0(n_rb, n_te, ifelse(heavy_ol == 1, "+OL", "")))
    )
  log$analysis_rows <- nrow(d)
  steps <- c("all_rows", "not_pass_or_run", "no_down_two_point", "aborted_snap", "fake_kick", "analysis_rows")
  list(plays = d, log = mget(steps, envir = log))
}

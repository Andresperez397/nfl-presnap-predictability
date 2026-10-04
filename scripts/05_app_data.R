# Build the Shiny app's data (app/data/app_data.rds) from the analysis outputs.
#
# Scouting tables are descriptive: for each season, the Q2 partial-pooling model is fit on the
# whole season, with the out-of-sample league prediction as the offset. So every "team" rate is the
# team's tendency beyond what the game situation implies, shrunk toward the league in proportion to
# how little data supports it. Intervals come from 200 draws of the team effects from their
# conditional distribution (fixed effects treated as known; they rest on a full season of plays).
for (f in list.files("R", full.names = TRUE)) source(f)
suppressPackageStartupMessages(library(jsonlite))

DERIVED <- file.path("data", "derived")
OUT <- file.path("reports", "tables")
N_SIM <- 200
SMALL_N <- 30
league <- fromJSON(file.path(OUT, "q1_summary.json"))$league_model
P <- readRDS(file.path(DERIVED, "q1_predictions.rds"))

d <- add_model_columns(load_plays()$plays)
d <- d[!is.na(d$n_rb), ]
d <- merge(d, data.frame(game_id = P$game_id, play_id = P$play_id, p_league = P[[league]]),
           by = c("game_id", "play_id"))

# Simulated team-aware probabilities for every play of one season: plays x N_SIM matrix.
simulate_season <- function(ds, fit) {
  re <- ranef(fit, condVar = TRUE)
  draw <- function(term, key) {
    m <- re[[term]][, 1]
    sdv <- sqrt(as.numeric(attr(re[[term]], "postVar")))
    names(sdv) <- rownames(re[[term]])
    k <- match(key, rownames(re[[term]]))
    mu <- ifelse(is.na(k), 0, m[k])
    s <- ifelse(is.na(k), 0, sdv[k])
    list(mu = mu, s = s)
  }
  parts <- list(draw("team", ds$team), draw("team:situation", paste(ds$team, ds$situation, sep = ":")),
                draw("team:pgrp5", paste(ds$team, ds$pgrp5, sep = ":")),
                draw("team:sg", paste(ds$team, ds$sg, sep = ":")))
  base <- ds$lp + lme4::fixef(fit)[[1]]
  mu <- base + Reduce(`+`, lapply(parts, `[[`, "mu"))
  set.seed(0)
  # Draw each random effect once per level and simulation, so plays sharing a level share the draw.
  sims <- matrix(base, nrow(ds), N_SIM)
  keys <- list(ds$team, paste(ds$team, ds$situation), paste(ds$team, ds$pgrp5), paste(ds$team, ds$sg))
  for (j in seq_along(parts)) {
    lv <- unique(keys[[j]])
    k <- match(keys[[j]], lv)
    first <- match(lv, keys[[j]])
    z <- matrix(stats::rnorm(length(lv) * N_SIM), length(lv), N_SIM)
    eff <- parts[[j]]$mu[first] + parts[[j]]$s[first] * z
    sims <- sims + eff[k, , drop = FALSE]
  }
  list(point = stats::plogis(mu), league = stats::plogis(base), sims = stats::plogis(sims))
}

summarise_by <- function(ds, sim, by) {
  key <- interaction(ds[by], drop = TRUE, sep = " | ")
  grp <- split(seq_len(nrow(ds)), key)
  out <- do.call(rbind, lapply(names(grp), function(g) {
    i <- grp[[g]]
    s <- colMeans(sim$sims[i, , drop = FALSE])
    data.frame(group = g, n = length(i), raw = mean(ds$pass[i]), league = mean(sim$league[i]),
               team = mean(sim$point[i]), lo = stats::quantile(s, 0.025, names = FALSE),
               hi = stats::quantile(s, 0.975, names = FALSE))
  }))
  out$dev <- out$team - out$league
  out$small <- out$n < SMALL_N
  out
}

tables <- list()
for (s in sort(unique(d$season))) {
  ds <- team_columns(d[d$season == s, ], d$p_league[d$season == s])
  fit <- fit_shrunk(ds)
  sim <- simulate_season(ds, fit)
  for (tm in sort(unique(ds$team))) {
    i <- ds$team == tm
    sub <- ds[i, ]
    ss <- list(point = sim$point[i], league = sim$league[i], sims = sim$sims[i, , drop = FALSE])
    tables[[paste(tm, s)]] <- list(
      situation = summarise_by(sub, ss, "situation"),
      personnel = summarise_by(sub, ss, "pgrp5"),
      alignment = summarise_by(transform(sub, align = ifelse(shotgun == 1, "shotgun", "under center")),
                               ss, "align"),
      cell = summarise_by(transform(sub, align = ifelse(shotgun == 1, "shotgun", "under center")),
                          ss, c("situation", "pgrp5", "align")),
      overall = summarise_by(transform(sub, all = "all plays"), ss, "all")
    )
  }
  message("season ", s, " done")
}

# Third down: tendencies and targets (descriptive, post-snap fields allowed here).
third <- d[d$down == 3, ]
third_groups <- split(third, list(third$team, third$season, third$dist_bucket), drop = TRUE)
third_tab <- do.call(rbind, lapply(third_groups, function(x) {
  data.frame(team = x$team[1], season = x$season[1], distance = as.character(x$dist_bucket[1]),
             n = nrow(x), pass_rate = mean(x$pass), league_expected = mean(x$p_league),
             epa = mean(x$epa, na.rm = TRUE), success = mean(x$success, na.rm = TRUE))
}))
targets <- third |>
  filter(pass_attempt == 1, !is.na(receiver_player_id)) |>
  group_by(team, season, receiver = receiver_player_name) |>
  summarise(targets = n(), epa_per_target = mean(epa, na.rm = TRUE), .groups = "drop_last") |>
  mutate(share = targets / sum(targets)) |>
  arrange(team, season, desc(targets)) |>
  ungroup() |>
  as.data.frame()

# Predictability: team index with game-cluster standard errors, league calibration by season.
Q <- readRDS(file.path(DERIVED, "q2_predictions.rds"))
Q$gain <- 1000 * (logloss_i(Q$pass, Q$p_league) - logloss_i(Q$pass, Q$p_shrunk))
per_game <- aggregate(cbind(gain_sum = gain, n = 1) ~ team + season + game_id, Q, sum)
idx <- do.call(rbind, lapply(split(per_game, list(per_game$team, per_game$season), drop = TRUE), function(x) {
  mg <- x$gain_sum / x$n
  t_hat <- sum(x$gain_sum) / sum(x$n)
  # Ratio estimator SE over games.
  se <- sqrt(sum((x$gain_sum - t_hat * x$n)^2) / (nrow(x) * (nrow(x) - 1))) / mean(x$n)
  data.frame(team = x$team[1], season = x$season[1], T = t_hat, se = se, games = nrow(x),
             plays = sum(x$n))
}))
calib <- do.call(rbind, lapply(split(P, P$season), function(x) {
  b <- cut(rank(x[[league]], ties.method = "first"), 10, labels = FALSE)
  data.frame(season = x$season[1], bin = 1:10, predicted = tapply(x[[league]], b, mean),
             observed = tapply(x$pass, b, mean), n = as.integer(table(b)))
}))

app <- list(
  tables = tables, third = third_tab, targets = targets, index = idx, calibration = calib,
  q1_pooled = read.csv(file.path(OUT, "q1_pooled.csv")),
  q1_by_season = read.csv(file.path(OUT, "q1_by_season.csv")),
  q2_comparisons = read.csv(file.path(OUT, "q2_comparisons.csv")),
  q2_reliability = fromJSON(file.path(OUT, "q2_reliability.json")),
  league_model = league, small_n = SMALL_N, seasons = sort(unique(d$season)),
  teams = sort(unique(d$team)), built = format(Sys.Date())
)
dir.create(file.path("app", "data"), recursive = TRUE, showWarnings = FALSE)
saveRDS(app, file.path("app", "data", "app_data.rds"), compress = "xz")
message("app data: ", round(file.size(file.path("app", "data", "app_data.rds")) / 1e6, 2), " MB")

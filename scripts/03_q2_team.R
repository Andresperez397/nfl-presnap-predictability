# Q2 (ANALYSIS_PLAN.md): are offenses more predictable than the league model, with honest shrinkage?
for (f in list.files("R", full.names = TRUE)) source(f)
suppressPackageStartupMessages(library(jsonlite))

DERIVED <- file.path("data", "derived")
OUT <- file.path("reports", "tables")
league <- fromJSON(file.path(OUT, "q1_summary.json"))$league_model
P <- readRDS(file.path(DERIVED, "q1_predictions.rds"))

d <- add_model_columns(load_plays()$plays)
d <- d[!is.na(d$n_rb), ]
d <- merge(d, data.frame(game_id = P$game_id, play_id = P$play_id, p_league = P[[league]]),
           by = c("game_id", "play_id"))
stopifnot(nrow(d) == nrow(P))

for (s in sort(unique(d$season))) {
  ck <- file.path(DERIVED, sprintf("q2_season_%d.rds", s))
  if (file.exists(ck)) next
  ds <- team_columns(d[d$season == s, ], d$p_league[d$season == s])
  t0 <- Sys.time()
  res <- season_walk_forward(ds)
  saveRDS(res, ck)
  message(s, " done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}
Q <- do.call(rbind, lapply(sort(unique(d$season)), function(s) {
  readRDS(file.path(DERIVED, sprintf("q2_season_%d.rds", s)))
}))
Q$p_league <- stats::plogis(Q$lp)
saveRDS(Q[c("game_id", "play_id", "season", "week", "team", "pass", "epa", "situation", "pgrp5",
            "shotgun", "p_league", "p_recal", "p_nopool", "p_shrunk")],
        file.path(DERIVED, "q2_predictions.rds"))

# Pooled comparisons with the game-cluster bootstrap.
mods <- c(league = "p_league", recal = "p_recal", nopool = "p_nopool", shrunk = "p_shrunk")
ll <- sapply(mods, function(m) logloss_i(Q$pass, Q[[m]]))
w <- boot_weights(Q$game_id, Q$season, n_boot = 1000, seed = 0)
bs <- lapply(colnames(ll), function(m) boot_mean(ll[, m], Q$game_id, w))
names(bs) <- colnames(ll)
pooled <- do.call(rbind, lapply(names(mods), function(m) {
  bri <- boot_mean(brier_i(Q$pass, Q[[mods[m]]]), Q$game_id, w)
  data.frame(model = m, n = nrow(Q), logloss = mean(ll[, m]), logloss_lo = ci(bs[[m]])[1],
             logloss_hi = ci(bs[[m]])[2], brier = mean(brier_i(Q$pass, Q[[mods[m]]])),
             brier_lo = ci(bri)[1], brier_hi = ci(bri)[2])
}))
cmp <- function(label, a, b) {
  dlt <- bs[[a]] - bs[[b]]
  data.frame(comparison = label, worse = a, better = b,
             logloss_reduction_millinats = 1000 * (mean(ll[, a]) - mean(ll[, b])),
             ci_lo = 1000 * ci(dlt)[1], ci_hi = 1000 * ci(dlt)[2], interval_above_zero = ci(dlt)[1] > 0)
}
comparisons <- rbind(
  cmp("team tendencies (shrunk) over league [decision rule]", "league", "shrunk"),
  cmp("shrinkage over no pooling [decision rule]", "nopool", "shrunk"),
  cmp("in-season league recalibration over league [added check]", "league", "recal"),
  cmp("team tendencies over recalibrated league [added check]", "recal", "shrunk")
)
write.csv(pooled, file.path(OUT, "q2_pooled.csv"), row.names = FALSE)
write.csv(comparisons, file.path(OUT, "q2_comparisons.csv"), row.names = FALSE)

# Log loss by week of season (how fast team information accumulates).
by_week <- do.call(rbind, lapply(sort(unique(Q$week)), function(wk) {
  i <- Q$week == wk
  data.frame(week = wk, n = sum(i), league = mean(ll[i, "league"]), recal = mean(ll[i, "recal"]),
             nopool = mean(ll[i, "nopool"]), shrunk = mean(ll[i, "shrunk"]))
}))
write.csv(by_week, file.path(OUT, "q2_by_week.csv"), row.names = FALSE)

# Team tendency index T (millinats per play) and its reliability.
Q$gain <- 1000 * (ll[, "league"] - ll[, "shrunk"])
Q$gain_recal <- 1000 * (ll[, "recal"] - ll[, "shrunk"])
idx <- aggregate(cbind(T = gain, T_vs_recal = gain_recal) ~ team + season, Q, mean)
idx$n <- aggregate(gain ~ team + season, Q, length)$gain
odd <- aggregate(gain ~ team + season, Q[Q$week %% 2 == 1, ], mean)
even <- aggregate(gain ~ team + season, Q[Q$week %% 2 == 0, ], mean)
halves <- merge(odd, even, by = c("team", "season"), suffixes = c("_odd", "_even"))
r_half <- stats::cor(halves$gain_odd, halves$gain_even)
nxt <- merge(idx, transform(idx, season = season - 1), by = c("team", "season"),
             suffixes = c("", "_next"))
yy <- stats::cor.test(nxt$T, nxt$T_next)
reliability <- list(
  n_team_seasons = nrow(idx),
  split_half_r = r_half, split_half_spearman_brown = 2 * r_half / (1 + r_half),
  split_half_r_ci = unname(stats::cor.test(halves$gain_odd, halves$gain_even)$conf.int),
  year_to_year_r = unname(yy$estimate), year_to_year_ci = unname(yy$conf.int), year_to_year_n = nrow(nxt),
  T_mean = mean(idx$T), T_sd = stats::sd(idx$T), T_range = range(idx$T)
)
write.csv(idx[order(-idx$T), ], file.path(OUT, "q2_team_index.csv"), row.names = FALSE)
write_json(reliability, file.path(OUT, "q2_reliability.json"), auto_unbox = TRUE, pretty = TRUE,
           digits = NA)

print(pooled, digits = 4)
print(comparisons, digits = 3)
str(reliability)

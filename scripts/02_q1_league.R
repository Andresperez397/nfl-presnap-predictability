# Q1 (ANALYSIS_PLAN.md): league predictability on held-out seasons, rolling origin.
# Each test season's predictions are checkpointed in data/derived/ so an interrupted run resumes.
for (f in list.files("R", full.names = TRUE)) source(f)
suppressPackageStartupMessages(library(jsonlite))

TEST_SEASONS <- 2019:2025
SPECS <- data.frame(
  model = c("B0 constant", "B1 situation", "B2 enet", "B2 xgb", "B3 enet", "B3 xgb", "B4 enet",
            "B4 xgb"),
  block = c(NA, NA, "B2", "B2", "B3", "B3", "B4", "B4"),
  learner = c("const", "lookup", "enet", "xgb", "enet", "xgb", "enet", "xgb")
)
DERIVED <- file.path("data", "derived")
OUT <- file.path("reports", "tables")
dir.create(DERIVED, showWarnings = FALSE)

d <- add_model_columns(load_plays()$plays)
d <- d[!is.na(d$n_rb), ] # 17 plays without personnel (DATA_AUDIT.md)

for (s in TEST_SEASONS) {
  ck <- file.path(DERIVED, sprintf("q1_season_%d.rds", s))
  if (file.exists(ck)) next
  t0 <- Sys.time()
  train <- d[d$season < s, ]
  test <- d[d$season == s, ]
  preds <- test[c("game_id", "play_id", "season", "week", "team", "opp", "pass", "situation")]
  tuning <- list()
  for (i in seq_len(nrow(SPECS))) {
    r <- fit_predict(train, test, SPECS$block[i], SPECS$learner[i])
    preds[[SPECS$model[i]]] <- r$p
    tuning[[SPECS$model[i]]] <- r$tuning
    message(s, " ", SPECS$model[i], " logloss ", round(mean(logloss_i(test$pass, r$p)), 4))
  }
  saveRDS(list(preds = preds, tuning = tuning,
               minutes = as.numeric(difftime(Sys.time(), t0, units = "mins"))), ck)
}

runs <- lapply(TEST_SEASONS, function(s) readRDS(file.path(DERIVED, sprintf("q1_season_%d.rds", s))))
P <- do.call(rbind, lapply(runs, `[[`, "preds"))
saveRDS(P, file.path(DERIVED, "q1_predictions.rds"))
tuning <- setNames(lapply(runs, `[[`, "tuning"), TEST_SEASONS)
write_json(tuning, file.path(OUT, "q1_tuning.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

models <- SPECS$model
by_season <- do.call(rbind, lapply(TEST_SEASONS, function(s) {
  x <- P[P$season == s, ]
  do.call(rbind, lapply(models, function(m) {
    data.frame(season = s, model = m, n = nrow(x), t(score(x$pass, x[[m]])))
  }))
}))
write.csv(by_season, file.path(OUT, "q1_by_season.csv"), row.names = FALSE)

# Pooled metrics with game-cluster bootstrap intervals (games resampled within test season).
w <- boot_weights(P$game_id, P$season, n_boot = 1000, seed = 0)
ll <- sapply(models, function(m) logloss_i(P$pass, P[[m]]))
bs <- lapply(models, function(m) boot_mean(ll[, m], P$game_id, w))
names(bs) <- models
pooled <- do.call(rbind, lapply(models, function(m) {
  acc_b <- boot_mean(correct_i(P$pass, P[[m]]), P$game_id, w)
  bri_b <- boot_mean(brier_i(P$pass, P[[m]]), P$game_id, w)
  sc <- score(P$pass, P[[m]])
  data.frame(model = m, n = nrow(P), t(sc),
             logloss_lo = ci(bs[[m]])[1], logloss_hi = ci(bs[[m]])[2],
             brier_lo = ci(bri_b)[1], brier_hi = ci(bri_b)[2],
             accuracy_lo = ci(acc_b)[1], accuracy_hi = ci(acc_b)[2])
}))

best_at <- function(b) {
  cand <- paste(b, c("enet", "xgb"))
  cand[which.min(pooled$logloss[match(cand, pooled$model)])]
}
best <- c(B0 = "B0 constant", B1 = "B1 situation", B2 = best_at("B2"), B3 = best_at("B3"),
          B4 = best_at("B4"))

gain <- function(label, m, ref) {
  dlt <- bs[[ref]] - bs[[m]]
  pt <- pooled$logloss[pooled$model == ref] - pooled$logloss[pooled$model == m]
  data.frame(comparison = label, model = m, reference = ref, logloss_reduction = pt,
             ci_lo = ci(dlt)[1], ci_hi = ci(dlt)[2], adds_information = ci(dlt)[1] > 0)
}
gains <- rbind(
  gain("situation over constant", best["B1"], best["B0"]),
  gain("game state over situation", best["B2"], best["B1"]),
  gain("shotgun/no-huddle over game state", best["B3"], best["B2"]),
  gain("personnel over shotgun/no-huddle", best["B4"], best["B3"]),
  gain("headline: B4 over situation baseline", best["B4"], best["B1"]),
  gain("xgb minus enet at B2 (positive = xgb better)", "B2 xgb", "B2 enet"),
  gain("xgb minus enet at B3 (positive = xgb better)", "B3 xgb", "B3 enet"),
  gain("xgb minus enet at B4 (positive = xgb better)", "B4 xgb", "B4 enet")
)
write.csv(gains, file.path(OUT, "q1_block_gains.csv"), row.names = FALSE)

# AUC intervals for the headline comparison (bootstrap over the same games).
game_rows <- split(seq_len(nrow(P)), P$game_id)[rownames(w)]
auc_boot <- function(m, idx_b) auc(P$pass[idx_b], P[[m]][idx_b])
auc_ci <- sapply(setNames(nm = unname(best[c("B1", "B4")])), function(m) {
  vals <- vapply(seq_len(ncol(w)), function(b) {
    idx <- unlist(rep(game_rows, w[, b]), use.names = FALSE)
    auc_boot(m, idx)
  }, 0)
  ci(vals)
})
pooled$auc_lo <- NA
pooled$auc_hi <- NA
pooled[match(colnames(auc_ci), pooled$model), c("auc_lo", "auc_hi")] <- t(auc_ci)
write.csv(pooled, file.path(OUT, "q1_pooled.csv"), row.names = FALSE)

# League learner for Q2: lower pooled B4 log loss; elastic net unless xgb is clearly better.
g4 <- gains[gains$model == "B4 xgb" & gains$reference == "B4 enet", ]
league <- if (g4$ci_lo > 0) "B4 xgb" else "B4 enet"
write_json(list(best_by_block = as.list(best), league_model = league),
           file.path(OUT, "q1_summary.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
# Run times vary between machines and runs, so they go to logs/ rather than the compared outputs.
dir.create("logs", showWarnings = FALSE)
write_json(setNames(lapply(runs, `[[`, "minutes"), TEST_SEASONS), file.path("logs", "q1_minutes.json"),
           auto_unbox = TRUE, pretty = TRUE)

print(pooled[, c("model", "logloss", "logloss_lo", "logloss_hi", "accuracy", "auc", "cal_slope", "ece")],
      digits = 4)
print(gains, digits = 3)
cat("league model:", league, "\n")

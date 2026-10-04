# Q1b (ANALYSIS_PLAN.md, pre-specified exploratory): do FTN's charted pre-snap fields (motion,
# backfield count, pistol, starting hash) add information beyond B4? Test seasons 2024 and 2025,
# training from 2022, the league learner from Q1, and identical rows for both models.
for (f in list.files("R", full.names = TRUE)) source(f)
suppressPackageStartupMessages(library(jsonlite))

OUT <- file.path("reports", "tables")
learner <- sub("B4 ", "", fromJSON(file.path(OUT, "q1_summary.json"))$league_model)

d <- add_model_columns(load_plays(2022:2025)$plays)
keep <- stats::complete.cases(d[c(PERSONNEL_VARS, FTN_VARS)])
dropped <- sum(!keep)
d <- d[keep, ]

ALL_FTN <- FTN_VARS
preds <- do.call(rbind, lapply(c(2024, 2025), function(s) {
  train <- d[d$season < s, ]
  test <- d[d$season == s, ]
  out <- test[c("game_id", "play_id", "season", "pass")]
  out$B4 <- fit_predict(train, test, "B4", learner)$p
  out$B5 <- fit_predict(train, test, "B5", learner)$p
  # Added exploratory ablation (DEVIATIONS.md): B5 with one FTN field removed at a time.
  for (v in ALL_FTN) {
    FTN_VARS <<- setdiff(ALL_FTN, v)
    out[[paste0("B5_minus_", v)]] <- fit_predict(train, test, "B5", learner)$p
  }
  FTN_VARS <<- ALL_FTN
  out
}))

w <- boot_weights(preds$game_id, preds$season, n_boot = 1000, seed = 0)
b4 <- boot_mean(logloss_i(preds$pass, preds$B4), preds$game_id, w)
b5 <- boot_mean(logloss_i(preds$pass, preds$B5), preds$game_id, w)
res <- list(
  learner = learner, test_seasons = c(2024, 2025), n_test = nrow(preds),
  rows_dropped_missing_ftn = dropped,
  logloss_B4 = mean(logloss_i(preds$pass, preds$B4)), logloss_B5 = mean(logloss_i(preds$pass, preds$B5)),
  reduction_millinats = 1000 * (mean(logloss_i(preds$pass, preds$B4)) - mean(logloss_i(preds$pass, preds$B5))),
  reduction_ci_millinats = 1000 * ci(b4 - b5),
  accuracy_B4 = mean(correct_i(preds$pass, preds$B4)), accuracy_B5 = mean(correct_i(preds$pass, preds$B5)),
  ablation_millinats_lost_when_removed = sapply(FTN_VARS, function(v) {
    1000 * (mean(logloss_i(preds$pass, preds[[paste0("B5_minus_", v)]])) - mean(logloss_i(preds$pass, preds$B5)))
  }),
  pass_rate_by_backfield = as.list(tapply(d$pass, pmin(d$backfield_n, 3), mean)),
  plays_by_backfield = as.list(table(pmin(d$backfield_n, 3))),
  by_season = lapply(split(preds, preds$season), function(x) {
    list(logloss_B4 = mean(logloss_i(x$pass, x$B4)), logloss_B5 = mean(logloss_i(x$pass, x$B5)))
  })
)
write_json(res, file.path(OUT, "q1b_ftn.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
str(res)

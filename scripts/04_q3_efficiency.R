# Q3 (ANALYSIS_PLAN.md): association between predictability and offensive efficiency.
# Not causal: roster quality, game script, opponents and coaching changes are not controlled.
for (f in list.files("R", full.names = TRUE)) source(f)
suppressPackageStartupMessages({
  library(jsonlite)
  library(sandwich)
})

DERIVED <- file.path("data", "derived")
OUT <- file.path("reports", "tables")

d <- load_plays()$plays
d <- d[d$season >= 2019, ]
eff <- aggregate(epa ~ team + season, d, mean)
names(eff)[3] <- "epa_per_play"
idx <- read.csv(file.path(OUT, "q2_team_index.csv"))
ts <- merge(idx, eff, by = c("team", "season"))
ts$T10 <- ts$T / 10

# Primary: within-franchise association, franchise and season fixed effects, cluster-robust by franchise.
fit <- stats::lm(epa_per_play ~ T10 + factor(team) + factor(season), data = ts)
V <- vcovCL(fit, cluster = ~team, type = "HC1")
b <- unname(stats::coef(fit)["T10"])
se <- sqrt(V["T10", "T10"])
df <- length(unique(ts$team)) - 1
primary <- list(
  n_team_seasons = nrow(ts), n_franchises = length(unique(ts$team)),
  coef_epa_per_10_millinats = b, se = se,
  ci = b + c(-1, 1) * stats::qt(0.975, df) * se, p = 2 * stats::pt(-abs(b / se), df),
  raw_correlation = stats::cor(ts$T, ts$epa_per_play),
  sd_T = stats::sd(ts$T), sd_epa = stats::sd(ts$epa_per_play)
)

# Added robustness (DEVIATIONS.md): drop the four team-seasons with the most extreme index after
# removing franchise and season effects, and a rank-based (Spearman) version.
dm <- function(x) x - ave(x, ts$team) - ave(x, ts$season) + mean(x)
top <- order(-abs(dm(ts$T)))[1:4]
fit_r <- stats::lm(epa_per_play ~ T10 + factor(team) + factor(season), data = ts[-top, ])
V_r <- vcovCL(fit_r, cluster = ~team, type = "HC1")
robust <- list(
  dropped = paste(ts$team[top], ts$season[top]),
  coef_without_extremes = unname(stats::coef(fit_r)["T10"]), se_without_extremes = sqrt(V_r["T10", "T10"]),
  spearman_two_way_demeaned = stats::cor(dm(ts$T), dm(ts$epa_per_play), method = "spearman")
)

# Exploratory: play-level EPA against how surprising the call was under the team-aware model.
Q <- readRDS(file.path(DERIVED, "q2_predictions.rds"))
cols <- c("game_id", "play_id", "down", "ydstogo", "yardline_100", "goal_to_go", "qtr",
          "half_seconds_remaining", "game_seconds_remaining", "score_differential",
          "posteam_timeouts_remaining", "defteam_timeouts_remaining", "home")
pl <- merge(Q, d[cols], by = c("game_id", "play_id"))
pl <- add_model_columns(pl[!is.na(pl$epa), ])
pl$surprise <- abs(pl$pass - pl$p_shrunk)
pl$team_season <- paste(pl$team, pl$season)
rhs <- paste(deparse(glm_formula("B2")[[2]]), collapse = " ")
f <- stats::as.formula(paste("epa ~ surprise * pass +", rhs, "+ (1 | team_season)"))
m <- lme4::lmer(f, data = pl, REML = TRUE)
co <- summary(m)$coefficients
expl <- data.frame(term = c("surprise", "pass", "surprise:pass"),
                   estimate = co[c("surprise", "pass", "surprise:pass"), "Estimate"],
                   se = co[c("surprise", "pass", "surprise:pass"), "Std. Error"])
expl$ci_lo <- expl$estimate - 1.96 * expl$se
expl$ci_hi <- expl$estimate + 1.96 * expl$se
# Implied EPA difference between a fully surprising call and a fully expected one, by play type.
expl_effects <- list(run_surprise_effect = expl$estimate[1],
                     pass_surprise_effect = expl$estimate[1] + expl$estimate[3], n_plays = nrow(pl))

write.csv(ts, file.path(OUT, "q3_team_seasons.csv"), row.names = FALSE)
write.csv(expl, file.path(OUT, "q3_play_level_exploratory.csv"), row.names = FALSE)
write_json(list(primary = primary, robustness = robust, exploratory = expl_effects), file.path(OUT, "q3_summary.json"),
           auto_unbox = TRUE, pretty = TRUE, digits = NA)
str(primary)
str(robust)
print(expl, digits = 3)

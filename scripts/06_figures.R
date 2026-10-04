# Report figures from reports/tables/ and data/derived/ (run after scripts 02-05).
for (f in list.files("R", full.names = TRUE)) source(f)
suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
})

OUT <- file.path("reports", "tables")
FIG <- file.path("reports", "figures")
DERIVED <- file.path("data", "derived")
dir.create(FIG, showWarnings = FALSE)
RED <- "#c8102e"
GREY <- "#6b7480"
BLUE <- "#2a78d6"
theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
                  plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA)))
save <- function(p, name, w = 8, h = 4.5) ggsave(file.path(FIG, name), p, width = w, height = h, dpi = 200)

# 1. Information ladder: pooled held-out log loss by block and learner.
pooled <- read.csv(file.path(OUT, "q1_pooled.csv"))
pooled$block <- sub(" .*", "", pooled$model)
pooled$learner <- ifelse(grepl("enet", pooled$model), "Elastic net",
                         ifelse(grepl("xgb", pooled$model), "Gradient boosting", "Baseline"))
lab <- c(B0 = "Constant", B1 = "Down & distance", B2 = "+ full game state", B3 = "+ shotgun, no-huddle",
         B4 = "+ personnel")
pooled$block_lab <- factor(lab[pooled$block], levels = rev(lab))
p1 <- ggplot(pooled, aes(logloss, block_lab, colour = learner)) +
  geom_errorbar(aes(xmin = logloss_lo, xmax = logloss_hi), width = 0, linewidth = 1, orientation = "y",
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c(Baseline = GREY, `Elastic net` = BLUE, `Gradient boosting` = RED)) +
  labs(x = "Log loss on unseen seasons, 2019–2025 (lower is better)", y = NULL, colour = NULL,
       title = "How much each layer of pre-snap information helps") +
  theme(legend.position = "bottom")
save(p1, "fig1_information_ladder.png")

# 2. League model calibration, pooled over test seasons.
summ <- fromJSON(file.path(OUT, "q1_summary.json"))
P <- readRDS(file.path(DERIVED, "q1_predictions.rds"))
pl <- P[[summ$league_model]]
b <- cut(rank(pl, ties.method = "first"), 20, labels = FALSE)
cal <- data.frame(pred = tapply(pl, b, mean), obs = tapply(P$pass, b, mean))
p2 <- ggplot(cal, aes(pred, obs)) +
  geom_abline(linetype = "dashed", colour = "grey70") +
  geom_point(colour = RED, size = 2.4) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_equal() +
  labs(x = "Predicted pass probability", y = "Observed pass rate",
       title = "League model calibration on unseen seasons", subtitle = "20 equal-size bins, 2019–2025")
save(p2, "fig2_calibration.png", w = 5, h = 5)

# 3. Team information accumulating through the season.
wk <- read.csv(file.path(OUT, "q2_by_week.csv"))
wk <- wk[wk$n >= 2000, ] # playoff weeks have few plays
long <- rbind(data.frame(week = wk$week, model = "No pooling", gain = 1000 * (wk$league - wk$nopool)),
              data.frame(week = wk$week, model = "League recalibrated in season", gain = 1000 * (wk$league - wk$recal)),
              data.frame(week = wk$week, model = "Partial pooling (team tendencies)", gain = 1000 * (wk$league - wk$shrunk)))
long$panel <- factor(ifelse(long$model == "No pooling", "B. Raw team rates (no pooling)",
                            "A. Shrunk team tendencies vs in-season recalibration"),
                     levels = c("A. Shrunk team tendencies vs in-season recalibration",
                                "B. Raw team rates (no pooling)"))
p3 <- ggplot(long, aes(week, gain, colour = model)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(linewidth = 1) + geom_point(size = 1.6) +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = c(`No pooling` = GREY, `League recalibrated in season` = BLUE,
                                 `Partial pooling (team tendencies)` = RED)) +
  labs(x = "Week predicted (using only earlier weeks of the same season)",
       y = "Log-loss gain over league model\n(millinats per play; above 0 = better)",
       colour = NULL, title = "Team tendencies help once they are shrunk; raw team rates hurt") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", hjust = 0))
save(p3, "fig3_by_week.png", w = 10, h = 4.5)

# 4. Reliability of the team tendency index.
Q <- readRDS(file.path(DERIVED, "q2_predictions.rds"))
Q$gain <- 1000 * (logloss_i(Q$pass, Q$p_league) - logloss_i(Q$pass, Q$p_shrunk))
odd <- aggregate(gain ~ team + season, Q[Q$week %% 2 == 1, ], mean)
even <- aggregate(gain ~ team + season, Q[Q$week %% 2 == 0, ], mean)
h <- merge(odd, even, by = c("team", "season"), suffixes = c("_odd", "_even"))
rel <- fromJSON(file.path(OUT, "q2_reliability.json"))
p4 <- ggplot(h, aes(gain_odd, gain_even)) +
  geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(colour = RED, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7, formula = y ~ x) +
  labs(x = "Index from odd weeks", y = "Index from even weeks",
       title = "Is an offense's predictability a stable trait?",
       subtitle = sprintf("Split-half r = %.2f (%d team-seasons); consecutive seasons r = %.2f",
                          rel$split_half_r, rel$n_team_seasons, rel$year_to_year_r))
save(p4, "fig4_index_reliability.png", w = 6, h = 5)

# 5. Predictability and efficiency (within franchise).
ts <- read.csv(file.path(OUT, "q3_team_seasons.csv"))
# Two-way demeaning (exact for this balanced panel), so the line's slope equals the fixed-effects estimate.
demean <- function(x) x - ave(x, ts$team) - ave(x, ts$season) + mean(x)
stopifnot(all(table(ts$team) == length(unique(ts$season))))
ts$T_w <- demean(ts$T)
ts$epa_w <- demean(ts$epa_per_play)
q3 <- fromJSON(file.path(OUT, "q3_summary.json"))$primary
p5 <- ggplot(ts, aes(T_w, epa_w)) +
  geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(colour = GREY, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = RED, fill = "#f3c3cb", formula = y ~ x) +
  labs(x = "Tendency index (franchise and season effects removed)", y = "EPA per play (franchise and season effects removed)",
       title = "More scoutable seasons were more efficient, not less (association only)",
       subtitle = sprintf("%+.3f EPA/play per 10 millinats (95%% CI %+.3f to %+.3f), franchise and season fixed effects",
                          q3$coef_epa_per_10_millinats, q3$ci[1], q3$ci[2]))
save(p5, "fig5_efficiency.png", w = 7, h = 5)
message("figures written to ", FIG)

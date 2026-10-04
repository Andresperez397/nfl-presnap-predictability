# Analysis plan (frozen before any outcome model is fit)

Written 2026-10-03 after the data audit (`DATA_AUDIT.md`) and before any model of run vs pass was fit. Any later change is logged in `DEVIATIONS.md` with a date and reason.

**Unit:** one offensive play. There are 342,254 plays from 2016–2025, regular season and playoffs.
**Outcome:** `pass` (1 = called dropback, including sacks and scrambles; 0 = designed run).
**Inputs:** only the pre-snap fields listed in `DATA_AUDIT.md`. No post-snap, defensive-alignment or betting fields.

## Q1. How predictable is run vs pass from pre-snap information, for a season the model has not seen?

**Validation:** rolling origin by season.
- **Test seasons:** 2019–2025, seven in all.
- **Training:** for test season *s*, every season before *s* (expanding window).
- **Tuning:** tuning uses only training data, with a temporal holdout. Each setting is fit on seasons before *s*−1 and scored on season *s*−1. The chosen setting is then refit on all seasons before *s*. Both learners are tuned this same way.

**Nested information blocks**

| Block | Adds | Learners |
|---|---|---|
| B0 | Nothing: the training-set pass rate | constant |
| B1 | Down × distance situation (13 cells: 1st down; 2nd, 3rd and 4th down × 1–3, 4–6, 7–10, 11+ yards). Baseline = training pass rate in each cell | lookup table |
| B2 | Full game state: down, distance, yard line, goal-to-go, quarter, seconds left in half and game, score margin, both teams' timeouts, home | elastic net, gradient boosting |
| B3 | + shotgun, no-huddle | elastic net, gradient boosting |
| B4 | + personnel (RB, TE and WR counts, 6+ OL flag, personnel group) | elastic net, gradient boosting |

**Elastic net** (`glmnet`, α = 0.5, λ chosen on the holdout season from glmnet's default path of 100 values). It uses this fixed basis:
- **Game state:**
  - down as a factor
  - natural splines with df: min(distance, 25) 4, yard line 5, seconds left in half 4, seconds left in game 4, score margin clipped to ±28 5
  - quarter as a factor (OT = 5)
  - each team's timeouts as a factor
  - goal-to-go and home
- **Interactions:** down × distance spline; score spline × fourth quarter; score spline × last 2 minutes of a half.
- **B3 adds:** shotgun, no-huddle and shotgun × down.
- **B4 adds:**
  - RB, TE and WR counts and the 6+ OL flag
  - a personnel-group factor (11, 12, 21, 13, 10, 22, other; a group is kept if it has at least 1,000 training plays, otherwise it becomes "other")
  - personnel group × shotgun.

Inputs are standardized by glmnet.

**Gradient boosting** (`xgboost`, `tree_method = "hist"`, logistic loss) on the raw inputs of the block.
- **Grid:** 12 settings, max depth {3, 5, 7} × learning rate {0.05, 0.1} × min child weight {1, 20}. Subsample is 0.8 and column subsample 0.8.
- **Rounds:** up to 3,000. The number is chosen by early stopping (patience 50) on the holdout season's log loss, then reused when refitting on all training seasons.
- **Seed:** fixed.

**Metrics**, on each test season and pooled over 2019–2025 (play-weighted):
- **Primary:** log loss.
- **Secondary:** Brier score, AUC, accuracy at 0.5, and calibration. Calibration is measured three ways:
  - intercept and slope of a logistic recalibration on the test season
  - expected calibration error over 10 equal-count bins
  - a reliability plot
- **Clipping:** predicted probabilities are clipped to [0.001, 0.999] before log loss is computed, for every model.

**Uncertainty:** game-cluster bootstrap with 1,000 resamples.
- Games are resampled within each test season, and the pooled metric recomputed.
- Intervals are 95% percentile intervals.
- Differences between models use the same resamples (paired).

**Decision rules (pre-specified)**
- **Information gain:** a block adds information if the pooled log-loss reduction against the previous block has a 95% interval above zero. For B2–B4 the comparison uses the better learner at each block.
- **Learner choice:** the **league model** for Q2 is the learner with the lower pooled log loss at B4. If its advantage has an interval that includes zero, the elastic net is used, because it is simpler.
- **Headline:** B4 against the B1 baseline, as pooled log loss, accuracy and AUC with intervals.

**Q1b (pre-specified, exploratory): FTN pre-snap charting, 2022+.**
- **Test seasons:** 2024 and 2025.
- **Training:** 2022 through *s*−1, with the holdout season *s*−1. For 2024 that means training on 2022 and holding out 2023.
- **Comparison:** B4 against B4 plus motion, backfield count, pistol and starting hash. Both are fit on the same rows with the Q1 league learner, and reported with the same bootstrap.
- **Why exploratory:** only two test seasons, and motion use rose every year, so its relationship with the call may be drifting.

## Q2. Is each offense more predictable than the league model says, once small samples are handled honestly?

For each test season *s* (2019–2025):
- The **league model** is the Q1 B4 league learner trained on seasons before *s*.
- Its out-of-sample prediction for each play is *p*.

**Team tendencies are learned within the season, week by week.** For each week *w* ≥ 2, models are fit only on plays from weeks before *w* of season *s*, then used to predict week *w*. Playoff weeks continue the numbering. This is what an opponent preparing for week *w* could know from the season so far.

Three predictions per play:
1. **League:** *p*.
2. **No pooling:** logistic regression with offset logit(*p*) and fixed effects for team, team × situation (the 13 B1 cells), team × personnel group (11, 12, 21, 13, other) and team × shotgun. Cells a team has not yet shown fall back to the league prediction.
3. **Partial pooling (the shrunk model):** the same structure as random intercepts, with `lme4::glmer` (`nAGQ = 0`) and offset logit(*p*):
   (1 | team) + (1 | team:situation) + (1 | team:personnel group) + (1 | team:shotgun).
   A team's deviation in a cell is shrunk toward zero (the league) in proportion to how little data the cell has.

**Metrics:** log loss and Brier, pooled over all test weeks and seasons, plus log loss by week of season. They are compared with the same game-cluster bootstrap (1,000).

**Decision rules**
- **Team-specific tendencies improve prediction** if the pooled log-loss reduction (league minus shrunk) has a 95% interval above zero.
- **Shrinkage beats no pooling** if (no pooling minus shrunk) has a 95% interval above zero.

**Team tendency index.** For each team-season, *T* = the mean over its test-week plays of (league log loss − shrunk log loss), in millinats per play. A higher *T* means the offense gives away more than league norms would suggest: an opponent gains more from scouting it.

**Is *T* a stable trait?**
- **Split-half reliability:** *T* computed separately on odd and even test weeks, the Pearson correlation across team-seasons, and the Spearman–Brown corrected value.
- **Year-to-year stability:** the correlation of *T* for the same franchise in consecutive seasons.

## Q3 (association, not causation). Does predictability go with lower efficiency?

**Primary:** team-season offensive EPA per play (all analysis plays) regressed on *T*, for team-seasons in 2019–2025.
- **Fixed effects:** franchise and season.
- **Standard errors:** cluster-robust by franchise.
- **What it estimates:** the within-franchise association between how predictable an offense was and how efficient it was.
- **Reporting:** the coefficient per 10 millinats of *T* and its 95% interval. It is described as an association.
- **Confounders named in the README:**
  - roster quality
  - game script (teams that trail pass more and in more obvious situations)
  - opponent quality
  - coaching changes
  - reverse causation (an efficient offense has less reason to vary).

**Exploratory:** play-level EPA against surprise.
- **Surprise:** |pass − *p*_shrunk|.
- **Model:** a mixed model with a random intercept for team-season, an interaction with play type, and the B2 game-state basis as controls.
- **Labelled:** exploratory.

## Deliverables that use these results

- **README:** figures and the pooled results.
- **Two-page PDF summary.**
- **Shiny app.** All rates are shown with counts, and intervals and small cells are flagged. Tabs:
  - team scouting (shrunk tendencies by situation, personnel and alignment for each team-season, fit on the full season)
  - league predictability and calibration
  - third-down targets
  - self-scout (where a team deviates most from league norms)
- **Static HTML fallback.**

The app's full-season fits are descriptive. Every predictive claim comes from the held-out results above.

## Software

- **R 4.5:** glmnet, xgboost, lme4, splines, sandwich, testthat, shiny, bslib, DT, plotly.
- **Versions** are pinned in `renv.lock`, and the app's in `app/manifest.json`.
- **Seeds:** all random steps (bootstrap, xgboost) use fixed seeds.

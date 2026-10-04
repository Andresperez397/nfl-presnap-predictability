# Deviations from ANALYSIS_PLAN.md

Every change made after the plan was frozen (commit 8c7bcca) is logged here, dated, with its reason.

## Analysis

- **2026-10-03, added check (Q2):** a fourth prediction, the league model recalibrated in season.
  - **What it is:** a single intercept shift fit on the weeks before *w* of the same season.
  - **Why:** the team models include intercepts, so they can absorb league-wide drift (for example, a league that passes more than in past seasons). That gain is not team-specific.
  - **What it reports:** shrunk against recalibrated league, which isolates the team-specific part.
  - **What is unchanged:** the pre-specified decision rules (league against shrunk, no pooling against shrunk) are reported exactly as planned.

- **2026-10-03, added exploratory analysis (Q1b):** a drop-one ablation of the FTN fields, and pass rates by charted QB alignment and backfield count.
  - **Why:** FTN charting cut log loss by 33 millinats (95% interval 30 to 37), about 15 times what personnel added. A gain that large had to be traced to its source and checked for post-snap leakage.
  - **What it showed:** two pre-snap fields carry the gain.
    - **Pistol alignment (16 millinats lost when removed):** pistol plays pass 21–30% of the time, against 76–80% from true shotgun. The play-by-play shotgun flag counts pistol as shotgun, so the main analysis cannot tell them apart.
    - **Backfield count (12 millinats):** an empty backfield passes 95% of the time, two backs 33%. This is formation information the main analysis lacks, because formation labels are not comparable across the 2023 source change.
    - Motion adds 3 millinats; starting hash adds nothing.
  - **What is unchanged:** the pre-specified B4 against B5 comparison is reported as planned.

- **2026-10-03, added robustness check (Q3):** the primary association refit without the four team-seasons with the most extreme index (ATL 2022, BAL 2019, CHI 2022, IND 2024), plus a rank-based (Spearman) version.
  - **Why:** the scatter plot showed a few high-leverage points.
  - **Result:** the association holds: 0.018 EPA per play per 10 millinats without them (SE 0.007), and Spearman 0.25.

## Implementation notes (no effect on results)

- **2026-10-03:** the first Q1 run stopped while loading data, before any model was fit.
  - **Cause:** nflverse files carry R class metadata, so arrow returned a `data.table` once xgboost had loaded that package, and the loader's indexing failed.
  - **Fix:** reads now always return a plain data frame.
  - **Rerun:** Q1 was rerun from the start with identical code otherwise.
- **2026-10-03:** the first Q1 summary step failed on two bugs, after all predictions were saved, and no results were read before the fix.
  - **AUC overflow:** the AUC function multiplied two integer counts, which overflows on the pooled 240k-play sample.
  - **Mislabelled intervals:** the AUC interval columns were labelled by block instead of model.
  - **Fix:** both are fixed, with a test for large samples. The summary was rerun from the saved predictions.
- **2026-10-03:** the first Q2 run was stopped before finishing its first season, and none of its output was used.
  - **Problem:** the no-pooling model was fit with `glm.fit` on a dense design, which took up to 6 minutes per week. On this rank-deficient design (the team column equals the sum of its team × shotgun columns) with perfectly separated cells, it also diverged, giving a training log loss of 2.0–3.5.
  - **Fix:** it is now fit by sparse IRLS with glm.fit's stopping rule and iteration cap. A 1e-8 ridge gives the minimum-norm solution for the aliased columns.
  - **Check:** on a full-rank design the IRLS matches `glm` to within 1e-6, and there is a test for it.
  - **What is unchanged:** the estimator itself (unpenalized logistic regression with the same terms).

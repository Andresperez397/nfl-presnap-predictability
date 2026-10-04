test_that("scoring rules match their definitions", {
  y <- c(1, 0, 1, 0)
  p <- c(0.9, 0.2, 0.6, 0.4)
  expect_equal(mean(logloss_i(y, p)), -mean(log(c(0.9, 0.8, 0.6, 0.6))))
  expect_equal(mean(brier_i(y, p)), mean(c(0.01, 0.04, 0.16, 0.16)))
  expect_equal(logloss_i(1, 0), -log(CLIP)) # clipped, never infinite
  set.seed(2)
  yy <- rbinom(500, 1, 0.5)
  pp <- runif(500)
  roc <- pROC::roc(yy, pp, levels = c(0, 1), direction = "<", quiet = TRUE)
  expect_equal(auc(yy, pp), as.numeric(pROC::auc(roc)))
})

test_that("a perfectly calibrated forecast has slope near 1 and intercept near 0", {
  set.seed(3)
  p <- runif(2e5, 0.05, 0.95)
  y <- rbinom(length(p), 1, p)
  cal <- calibration(y, p)
  expect_lt(abs(cal[["cal_slope"]] - 1), 0.03)
  expect_lt(abs(cal[["cal_intercept"]]), 0.03)
  expect_lt(cal[["ece"]], 0.01)
})

test_that("game bootstrap keeps each season's game count and reproduces the plain mean", {
  game <- rep(paste0("g", 1:30), each = 5)
  season <- rep(c(2019, 2020, 2021), each = 50)
  w <- boot_weights(game, season, n_boot = 20, seed = 0)
  per_season <- rowsum(w, season[match(rownames(w), game)])
  expect_true(all(per_season == 10))
  loss <- seq_along(game) / 10
  ones <- matrix(1L, nrow(w), 1, dimnames = list(rownames(w), NULL))
  expect_equal(boot_mean(loss, game, ones), mean(loss))
})

test_that("AUC works on samples large enough to overflow integer products", {
  y <- rep(c(0, 1), each = 150000)
  p <- c(rep(0.4, 150000), rep(0.6, 150000))
  expect_equal(auc(y, p), 1)
})

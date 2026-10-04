test_that("spline knots and factor levels come from the training data only", {
  d <- fake_plays()
  train <- d[d$season < 2019, ]
  test <- d[d$season == 2019, ]
  a <- glm_matrices(train, test, "B4")$test
  # Appending extreme rows to the test set must not change how the original rows are encoded.
  extreme <- test[1:5, ]
  extreme$yardline_100 <- 1
  extreme$score28 <- 28
  b <- glm_matrices(train, rbind(test, extreme), "B4")$test[seq_len(nrow(test)), ]
  expect_equal(unname(a), unname(b))
})

test_that("a test season's outcomes cannot change its own predictions", {
  d <- fake_plays()
  train <- d[d$season < 2019, ]
  test <- d[d$season == 2019, ]
  flipped <- test
  flipped$pass <- 1 - flipped$pass
  old <- XGB_GRID
  XGB_GRID <<- XGB_GRID[1:2, ]
  on.exit(XGB_GRID <<- old)
  for (learner in c("lookup", "enet", "xgb")) {
    expect_equal(fit_predict(train, test, "B4", learner)$p,
                 fit_predict(train, flipped, "B4", learner)$p, info = learner)
  }
})

test_that("tuning holds out the latest training season", {
  d <- fake_plays()
  sp <- inner_split(d[d$season < 2019, ])
  expect_equal(unique(sp$hold$season), 2018)
  expect_true(all(sp$fit$season < 2018))
})

test_that("gradient boosting is deterministic with a fixed seed", {
  d <- fake_plays()
  old <- XGB_GRID
  XGB_GRID <<- XGB_GRID[1:2, ]
  on.exit(XGB_GRID <<- old)
  a <- fit_predict(d[d$season < 2019, ], d[d$season == 2019, ], "B3", "xgb")$p
  b <- fit_predict(d[d$season < 2019, ], d[d$season == 2019, ], "B3", "xgb")$p
  expect_identical(a, b)
})

test_that("a team model for week w never sees week w or later outcomes", {
  d <- fake_plays(seasons = 2019)
  d <- team_columns(d, rep(0.6, nrow(d)))
  a <- season_walk_forward(d)
  changed <- d
  late <- changed$week >= 10
  changed$pass[late] <- 1 - changed$pass[late]
  b <- season_walk_forward(changed)
  early <- a$week < 10
  for (col in c("p_nopool", "p_shrunk", "p_recal")) {
    expect_equal(a[[col]][early], b[[col]][early], info = col)
  }
})

test_that("the sparse IRLS used for the no-pooling model matches glm on a full-rank design", {
  set.seed(1)
  n <- 5000
  d <- data.frame(g = sample(paste0("L", 1:40), n, TRUE), lp = rnorm(n, 0.3, 1))
  eff <- setNames(rnorm(40, 0, 0.5), paste0("L", 1:40))
  d$y <- rbinom(n, 1, plogis(d$lp + eff[d$g]))
  x <- Matrix::sparse.model.matrix(~ 0 + factor(g), d)
  g <- glm(y ~ 0 + factor(g) + offset(lp), binomial, d)
  expect_equal(irls_logit(x, d$y, d$lp), unname(coef(g)), tolerance = 1e-6)
})

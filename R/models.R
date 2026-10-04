# League models for Q1: the B1 lookup baseline, elastic net and gradient boosting.
# Both learners are tuned the same way: fit on seasons before the holdout season, score on the
# holdout season, then refit the chosen setting on all training seasons.
suppressPackageStartupMessages({
  library(glmnet)
  library(xgboost)
})

XGB_GRID <- expand.grid(max_depth = c(3, 5, 7), eta = c(0.05, 0.1), min_child_weight = c(1, 20))
XGB_THREADS <- 8
ENET_ALPHA <- 0.5

fit_predict_b0 <- function(train, test) rep(mean(train$pass), nrow(test))

fit_predict_b1 <- function(train, test) {
  rate <- tapply(train$pass, train$situation, mean)
  p <- unname(rate[as.character(test$situation)])
  ifelse(is.na(p), mean(train$pass), p)
}

# Split training data into the inner fit set and the holdout (latest) season.
inner_split <- function(train) {
  hold <- max(train$season)
  list(fit = train[train$season < hold, ], hold = train[train$season == hold, ])
}

fit_predict_enet <- function(train, test, block) {
  sp <- inner_split(train)
  m_in <- glm_matrices(sp$fit, sp$hold, block)
  path <- glmnet(m_in$train, sp$fit$pass, family = "binomial", alpha = ENET_ALPHA)
  ll <- apply(predict(path, m_in$test, type = "response"), 2,
              function(p) mean(logloss_i(sp$hold$pass, p)))
  lambda <- path$lambda[which.min(ll)]
  m <- glm_matrices(train, test, block)
  fit <- glmnet(m$train, train$pass, family = "binomial", alpha = ENET_ALPHA)
  p <- as.numeric(predict(fit, m$test, s = lambda, type = "response"))
  list(p = p, tuning = list(lambda = lambda, holdout_logloss = min(ll),
                            lambda_index = which.min(ll), n_lambda = length(path$lambda)))
}

xgb_params <- function(row) {
  list(objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
       max_depth = row$max_depth, eta = row$eta, min_child_weight = row$min_child_weight,
       subsample = 0.8, colsample_bytree = 0.8, nthread = XGB_THREADS, seed = 0)
}

fit_predict_xgb <- function(train, test, block) {
  sp <- inner_split(train)
  d_fit <- xgb.DMatrix(tree_matrix(sp$fit, block), label = sp$fit$pass)
  d_hold <- xgb.DMatrix(tree_matrix(sp$hold, block), label = sp$hold$pass)
  res <- lapply(seq_len(nrow(XGB_GRID)), function(i) {
    set.seed(0)
    b <- xgb.train(xgb_params(XGB_GRID[i, ]), d_fit, nrounds = 3000,
                   evals = list(hold = d_hold), early_stopping_rounds = 50, verbose = 0)
    log <- attributes(b)$evaluation_log
    data.frame(i = i, best_iter = which.min(log$hold_logloss), holdout_logloss = min(log$hold_logloss))
  })
  res <- do.call(rbind, res)
  best <- res[which.min(res$holdout_logloss), ]
  set.seed(0)
  fit <- xgb.train(xgb_params(XGB_GRID[best$i, ]),
                   xgb.DMatrix(tree_matrix(train, block), label = train$pass),
                   nrounds = best$best_iter, verbose = 0)
  p <- predict(fit, xgb.DMatrix(tree_matrix(test, block)))
  list(p = p, tuning = c(as.list(XGB_GRID[best$i, ]), list(nrounds = best$best_iter,
                                                             holdout_logloss = best$holdout_logloss)),
       grid = res)
}

fit_predict <- function(train, test, block, learner) {
  switch(learner,
         const = list(p = fit_predict_b0(train, test)),
         lookup = list(p = fit_predict_b1(train, test)),
         enet = fit_predict_enet(train, test, block),
         xgb = fit_predict_xgb(train, test, block))
}

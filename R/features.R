# Design matrices for the nested information blocks in ANALYSIS_PLAN.md (Q1).
suppressPackageStartupMessages(library(splines))

BLOCKS <- c("B2", "B3", "B4")
PGRP_MIN <- 1000 # a personnel group needs this many training plays to get its own level

add_model_columns <- function(d) {
  d$dist25 <- pmin(d$ydstogo, 25)
  d$score28 <- pmax(pmin(d$score_differential, 28), -28)
  d$q4 <- as.integer(d$qtr >= 4)
  d$last2 <- as.integer(d$half_seconds_remaining <= 120)
  d$down_f <- factor(d$down, levels = 1:4)
  d$qtr_f <- factor(pmin(d$qtr, 5), levels = 1:5)
  d$pto_f <- factor(d$posteam_timeouts_remaining, levels = 0:3)
  d$dto_f <- factor(d$defteam_timeouts_remaining, levels = 0:3)
  d
}

# Personnel-group levels with at least PGRP_MIN plays in the training data; the rest are "other".
pgrp_levels <- function(train) {
  tab <- table(train$personnel)
  c(sort(names(tab)[tab >= PGRP_MIN]), "other")
}

set_pgrp <- function(d, levels) {
  g <- ifelse(d$personnel %in% levels, d$personnel, "other")
  factor(g, levels = levels)
}

glm_formula <- function(block) {
  f <- paste(
    "~ down_f * ns(dist25, df = 4) + ns(yardline_100, df = 5) + goal_to_go + qtr_f",
    "+ ns(half_seconds_remaining, df = 4) + ns(game_seconds_remaining, df = 4)",
    "+ ns(score28, df = 5) * (q4 + last2) + pto_f + dto_f + home"
  )
  if (block %in% c("B3", "B4", "B5")) f <- paste(f, "+ shotgun + no_huddle + shotgun:down_f")
  if (block %in% c("B4", "B5")) f <- paste(f, "+ n_rb + n_te + n_wr + heavy_ol + pgrp + pgrp:shotgun")
  if (block == "B5") f <- paste(f, "+", paste(FTN_VARS, collapse = " + "))
  stats::as.formula(f)
}

# Build train and test matrices with spline knots and factor levels fixed on the training data.
glm_matrices <- function(train, test, block) {
  lv <- pgrp_levels(train)
  train$pgrp <- set_pgrp(train, lv)
  test$pgrp <- set_pgrp(test, lv)
  mf <- stats::model.frame(glm_formula(block), train, na.action = stats::na.fail)
  tt <- stats::terms(mf)
  x_train <- stats::model.matrix(tt, mf)[, -1, drop = FALSE]
  mf_test <- stats::model.frame(tt, test, na.action = stats::na.fail)
  x_test <- stats::model.matrix(tt, mf_test)[, -1, drop = FALSE]
  stopifnot(identical(colnames(x_train), colnames(x_test)))
  list(train = x_train, test = x_test)
}

tree_vars <- function(block) {
  v <- STATE_VARS
  if (block %in% c("B3", "B4")) v <- c(v, ALIGN_VARS)
  if (block == "B4") v <- c(v, PERSONNEL_VARS)
  if (block == "B5") v <- c(v, ALIGN_VARS, PERSONNEL_VARS, FTN_VARS)
  v
}

tree_matrix <- function(d, block) {
  m <- as.matrix(d[tree_vars(block)])
  storage.mode(m) <- "double"
  m
}

# Q2: team-specific tendencies learned within a season, week by week (ANALYSIS_PLAN.md).
# Every model predicting week w is fit only on plays from earlier weeks of the same season.
suppressPackageStartupMessages(library(lme4))

PGRP5 <- c("11", "12", "21", "13")

team_columns <- function(d, p_league) {
  d$lp <- stats::qlogis(clip(p_league))
  d$pgrp5 <- factor(ifelse(d$personnel %in% PGRP5, d$personnel, "other"), levels = c(PGRP5, "other"))
  d$sg <- factor(d$shotgun, levels = 0:1)
  d$ts <- paste(d$team, d$situation)
  d$tp <- paste(d$team, d$pgrp5)
  d$tsg <- paste(d$team, d$sg)
  d
}

# No pooling: fixed effects for team, team x situation, team x personnel group and team x shotgun,
# with the league prediction as an offset. Levels a team has not shown yet contribute zero.
# Fit by sparse IRLS (glm.fit on the dense design took minutes per week). The design is rank
# deficient by construction (team = sum of its team x shotgun columns), so a 1e-8 ridge gives the
# minimum-norm solution; stopping rule and iteration cap match glm.fit's defaults.
nopool_design <- function(train) {
  terms <- c("team", "ts", "tp", "tsg")
  do.call(cbind, lapply(terms, function(t) {
    f <- factor(train[[t]])
    m <- Matrix::sparse.model.matrix(~ 0 + f)
    colnames(m) <- paste0(t, "=", levels(f))
    m
  }))
}

irls_logit <- function(x, y, offset, maxit = 25, tol = 1e-8, ridge = 1e-8) {
  b <- rep(0, ncol(x))
  dev_old <- Inf
  for (it in seq_len(maxit)) {
    eta <- offset + as.numeric(x %*% b)
    mu <- stats::plogis(eta)
    w <- pmax(mu * (1 - mu), 1e-10)
    z <- (eta - offset) + (y - mu) / w
    xtwx <- Matrix::crossprod(x, x * w)
    b <- as.numeric(Matrix::solve(xtwx + Matrix::Diagonal(ncol(x), ridge), Matrix::crossprod(x, w * z)))
    mu <- stats::plogis(offset + as.numeric(x %*% b))
    dev <- -2 * sum(y * log(pmax(mu, 1e-15)) + (1 - y) * log(pmax(1 - mu, 1e-15)))
    if (abs(dev - dev_old) / (abs(dev) + 0.1) < tol) break
    dev_old <- dev
  }
  b
}

fit_nopool <- function(train) {
  x <- nopool_design(train)
  b <- irls_logit(x, train$pass, train$lp)
  names(b) <- colnames(x)
  b
}

predict_nopool <- function(b, test) {
  eta <- test$lp
  for (t in c("team", "ts", "tp", "tsg")) {
    v <- b[paste0(t, "=", test[[t]])]
    v[is.na(v)] <- 0
    eta <- eta + v
  }
  stats::plogis(eta)
}

SHRUNK_FORMULA <- pass ~ 1 + offset(lp) + (1 | team) + (1 | team:situation) + (1 | team:pgrp5) +
  (1 | team:sg)

fit_shrunk <- function(train) {
  suppressMessages(glmer(SHRUNK_FORMULA, data = train, family = binomial(), nAGQ = 0,
                         control = glmerControl(optimizer = "bobyqa")))
}

predict_shrunk <- function(fit, test) {
  as.numeric(stats::predict(fit, newdata = test, type = "response", allow.new.levels = TRUE))
}

# League prediction recalibrated in season: a single intercept shift fit on earlier weeks.
# Separates league-wide drift from team-specific tendencies (added check; see DEVIATIONS.md).
fit_recal <- function(train) {
  unname(stats::coef(stats::glm(pass ~ 1 + offset(lp), family = stats::binomial(), data = train)))
}

# Walk forward through one season. Returns the test-week rows with all predictions.
season_walk_forward <- function(ds) {
  weeks <- sort(unique(ds$week))
  out <- lapply(weeks[-1], function(w) {
    train <- ds[ds$week < w, ]
    test <- ds[ds$week == w, ]
    test$p_nopool <- predict_nopool(fit_nopool(train), test)
    test$p_shrunk <- predict_shrunk(fit_shrunk(train), test)
    test$p_recal <- stats::plogis(test$lp + fit_recal(train))
    test
  })
  do.call(rbind, out)
}

# Scoring rules, calibration and the game-cluster bootstrap.

CLIP <- 0.001

clip <- function(p) pmin(pmax(p, CLIP), 1 - CLIP)

logloss_i <- function(y, p) {
  p <- clip(p)
  -(y * log(p) + (1 - y) * log(1 - p))
}

brier_i <- function(y, p) (y - p)^2

correct_i <- function(y, p) as.numeric((p >= 0.5) == (y == 1))

auc <- function(y, p) {
  r <- rank(p)
  n1 <- as.numeric(sum(y == 1)) # double: n1 * n0 overflows integers on large samples
  n0 <- length(y) - n1
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Logistic recalibration on the evaluation data: slope 1 and intercept 0 mean well calibrated.
calibration <- function(y, p) {
  lp <- stats::qlogis(clip(p))
  slope <- unname(stats::coef(stats::glm(y ~ lp, family = stats::binomial()))[2])
  intercept <- unname(stats::coef(stats::glm(y ~ 1 + offset(lp), family = stats::binomial()))[1])
  bins <- cut(rank(p, ties.method = "first"), 10, labels = FALSE)
  ece <- sum(tapply(seq_along(y), bins, function(i) length(i) * abs(mean(y[i]) - mean(p[i])))) /
    length(y)
  c(cal_intercept = intercept, cal_slope = slope, ece = ece)
}

score <- function(y, p) {
  c(logloss = mean(logloss_i(y, p)), brier = mean(brier_i(y, p)), accuracy = mean(correct_i(y, p)),
    auc = auc(y, p), calibration(y, p))
}

# Bootstrap weights: games are resampled with replacement within each stratum (test season).
# Returns a games x n_boot matrix of how many times each game is drawn.
boot_weights <- function(game, stratum, n_boot = 1000, seed = 0) {
  set.seed(seed)
  g <- unique(data.frame(game = game, stratum = stratum))
  w <- matrix(0L, nrow(g), n_boot, dimnames = list(g$game, NULL))
  for (s in unique(g$stratum)) {
    idx <- which(g$stratum == s)
    for (b in seq_len(n_boot)) {
      draw <- tabulate(sample.int(length(idx), length(idx), replace = TRUE), length(idx))
      w[idx, b] <- draw
    }
  }
  w
}

# Pooled per-play mean of a per-play loss under each bootstrap resample (play-weighted).
boot_mean <- function(loss, game, w) {
  sums <- rowsum(loss, game)[rownames(w), 1]
  n <- rowsum(rep(1, length(loss)), game)[rownames(w), 1]
  colSums(w * sums) / colSums(w * n)
}

ci <- function(x) stats::quantile(x, c(0.025, 0.975), names = FALSE)

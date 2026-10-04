test_that("personnel strings in both source formats parse to the same counts", {
  x <- c("1 RB, 1 TE, 3 WR", "1 C, 2 G, 1 QB, 1 RB, 2 T, 1 TE, 3 WR",
         "6 OL, 1 RB, 2 TE, 1 WR", "1 C, 1 FB, 2 G, 1 QB, 1 RB, 3 T, 2 TE, 0 WR", NA, "")
  p <- parse_personnel(x)
  expect_equal(p$n_rb, c(1, 1, 1, 2, NA, NA))
  expect_equal(p$n_te, c(1, 1, 2, 2, NA, NA))
  expect_equal(p$n_wr, c(3, 3, 1, 0, NA, NA))
  expect_equal(p$n_ol, c(5, 5, 6, 6, NA, NA))
})

test_that("no betting column is ever read", {
  expect_false(any(grepl(BETTING_PATTERN, PBP_COLS)))
  expect_true(all(grepl(BETTING_PATTERN, c("spread_line", "total_line", "vegas_wp", "away_moneyline"))))
})

test_that("no post-snap field is a model input", {
  inputs <- unique(c(tree_vars("B5"), all.vars(glm_formula("B5"))))
  expect_length(intersect(inputs, POST_SNAP), 0)
  expect_false(any(c("ep", "wp", "vegas_wp", "defenders_in_box") %in% inputs))
})

test_that("cleaned plays follow the documented rules", {
  skip_if_not(has_data)
  r <- load_plays(2024)
  d <- r$plays
  expect_true(all(d$play_type %in% c("pass", "run")))
  expect_false(any(is.na(d$down)))
  expect_true(all(d$aborted_play == 0))
  expect_false(any(grepl("Punt formation|Field Goal formation", d$desc, ignore.case = TRUE)))
  expect_false(any(grepl(BETTING_PATTERN, names(d))))
  # Scrambles and sacks are called passes; designed runs are not.
  expect_true(all(d$pass[d$qb_scramble == 1] == 1))
  expect_true(all(d$pass[d$sack == 1] == 1))
  expect_equal(nrow(d) + sum(unlist(r$log[setdiff(names(r$log), c("all_rows", "analysis_rows"))])),
               r$log$all_rows)
  expect_false(anyDuplicated(d[c("game_id", "play_id")]) > 0)
})

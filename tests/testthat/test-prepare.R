test_that("the city extract parses, separates grain, and keeps the duplicate", {
  rounds <- prepare_dobson_rounds()

  expect_s3_class(rounds, "golfops_rounds")
  expect_equal(nrow(rounds), 1838)
  expect_equal(sum(rounds$rounds_played), 868646)
  expect_equal(min(rounds$date), as.Date("2004-12-10"))
  expect_equal(max(rounds$date), as.Date("2026-04-30"))
  expect_equal(sum(rounds$grain == "day"), 1766)
  expect_equal(sum(rounds$grain == "month"), 72)
  expect_equal(sum(rounds$flag_duplicate_date), 2)
  expect_false(any(rounds$flag_calendar_mismatch))
  expect_true(all(rounds$day[rounds$grain == "month"] == 1))
})

test_that("per-played-round yield is undefined when nobody played", {
  rounds <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  zero <- rounds[rounds$rounds_played == 0, ]
  expect_true(all(is.na(zero$green_fee_per_played_round)))
  expect_true(all(is.na(zero$total_revenue_per_played_round)))
  expect_equal(zero$utilization, 0)
})

test_that("a missing city column stops preparation", {
  path <- tempfile(fileext = ".csv")
  toy_dobson_csv(path)
  raw <- utils::read.csv(path, check.names = FALSE)
  raw$Season <- NULL
  write.csv(raw, path, row.names = FALSE)
  expect_error(prepare_dobson_rounds(path), class = "golfops_schema_error")
})

test_that("metric definitions keep price and capacity claims narrow", {
  defs <- metric_definitions()
  expect_true(all(c("metric", "formula", "use", "do_not_use_as") %in% names(defs)))
  green <- defs[defs$metric == "green_fee_per_played_round", ]
  expect_match(green$do_not_use_as, "posted green fee")
  expect_match(green$do_not_use_as, "elasticity")
  possible <- defs[defs$metric == "rounds_possible", ]
  expect_match(possible$do_not_use_as, "Tee-sheet capacity")
})

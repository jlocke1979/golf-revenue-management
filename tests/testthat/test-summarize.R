test_that("period yield is a ratio of sums, not a mean of daily yields", {
  path <- toy_dobson_csv(tempfile(fileext = ".csv"))
  rounds <- prepare_dobson_rounds(path)
  pair <- rounds[rounds$date %in% as.Date(c("2024-01-02", "2024-01-05")), ]
  # Yields are 50 and 50 in the main toy. Build the contrast directly.
  pair$green_fee_revenue <- c(100, 10000)
  pair$total_revenue <- c(100, 10000)
  pair$ancillary_revenue <- c(0, 0)
  pair$rounds_played <- c(10, 100)
  pair$flag_zero_rounds <- FALSE
  summary <- summarize_golf_rounds(pair, by = "year", duplicates = "drop_extra")
  expect_equal(summary$green_fee_per_played_round, 10100 / 110)
  expect_false(isTRUE(all.equal(summary$green_fee_per_played_round, mean(c(10, 100)))))
})

test_that("weekday summaries reject monthly totals and duplicate dates stop by default", {
  rounds <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  monthly <- rounds[rounds$grain == "month", ]
  expect_error(
    summarize_golf_rounds(monthly, by = "weekday", grain = "month"),
    class = "golfops_input_error"
  )
  expect_error(
    summarize_golf_rounds(rounds, by = "year_month"),
    class = "golfops_duplicate_error"
  )
  dropped <- summarize_golf_rounds(rounds, by = "year_month", duplicates = "drop_extra")
  feb <- dropped[dropped$period == as.Date("2024-02-01"), ]
  expect_equal(feb$n_rows, 1)
  expect_equal(feb$rounds_played, 20)
})

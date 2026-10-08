daily_toy <- function() {
  rounds <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  rounds[rounds$grain == "day" & rounds$date >= as.Date("2024-01-02") &
           rounds$date <= as.Date("2024-01-05"), ]
}

test_that("an unchanged scenario reproduces observed revenue", {
  scenario <- simulate_price_volume(daily_toy(), price_change = 0, volume_change = 0)
  expect_equal(scenario$totals$scenario_total_revenue, scenario$totals$baseline_total_revenue)
  expect_equal(scenario$totals$scenario_rounds, scenario$totals$baseline_rounds)
  expect_match(
    paste(scenario$assumptions, collapse = " "),
    "not an estimate of price elasticity"
  )
})

test_that("price change rescales green-fee yield and leaves ancillary dollars fixed", {
  one <- daily_toy()
  one <- one[one$date == as.Date("2024-01-02"), ]
  scenario <- simulate_price_volume(one, price_change = 0.10, volume_change = 0)
  expect_equal(scenario$totals$scenario_green_fee_revenue, 5500)
  expect_equal(scenario$totals$scenario_ancillary_revenue, 5000)
  expect_equal(scenario$totals$scenario_total_revenue, 10500)
  expect_equal(scenario$totals$scenario_rounds, 100)
})

test_that("volume growth stops at the daylight estimate and zero-round days stay put", {
  scenario <- simulate_price_volume(
    daily_toy(),
    price_change = 0,
    volume_change = 0.5
  )
  totals <- scenario$totals
  expect_equal(totals$n_days_capped, 1)
  expect_equal(totals$n_zero_round_days, 1)
  expect_equal(totals$n_days_already_above_daylight, 1)
  expect_equal(totals$scenario_rounds, 725)
  expect_equal(totals$scenario_green_fee_revenue, 36350)
  expect_equal(totals$scenario_ancillary_revenue, 6500)
  expect_equal(totals$scenario_total_revenue, 42850)
  expect_equal(totals$green_fee_yield_scenario, 50)

  zero <- scenario$daily[scenario$daily$date == as.Date("2024-01-04"), ]
  expect_equal(zero$scenario_green_fee_revenue, 100)
  expect_true(zero$zero_rounds_unchanged)

  over <- scenario$daily[scenario$daily$date == as.Date("2024-01-05"), ]
  expect_equal(over$scenario_rounds, 375)
  expect_false(over$capped_at_daylight_estimate)
})

test_that("monthly rows and negative prices that erase the fee are rejected", {
  rounds <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  expect_error(
    simulate_price_volume(rounds, price_change = 0.1, volume_change = 0),
    class = "golfops_input_error"
  )
  expect_error(
    simulate_price_volume(daily_toy(), price_change = -1, volume_change = 0),
    class = "golfops_input_error"
  )
})

test_that("participation and revenue plots return ggplot objects", {
  rounds <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  summary <- summarize_golf_rounds(rounds, by = "weekday", duplicates = "drop_extra")
  expect_s3_class(plot_participation(summary), "ggplot")
  expect_s3_class(plot_revenue(summary, "per_played_round"), "ggplot")
})

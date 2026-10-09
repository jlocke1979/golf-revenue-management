test_that("the operating sample drops the isolated day and the extra duplicate only", {
  prepared <- prepare_dobson_rounds()
  sample <- dobson_operating_daily(prepared)
  expect_false(as.Date("2004-12-10") %in% sample$date)
  expect_true(as.Date("2004-12-10") %in% prepared$date)
  expect_false(as.Date("2024-12-10") %in% sample$date)
  expect_equal(sum(duplicated(sample$date)), 0)
  expect_equal(min(sample$date), as.Date("2021-07-01"))
  expect_true(all(sample$grain == "day"))
  expect_true(all(sample$capacity_basis == "daylight_estimate"))
  unknown <- sample[sample$flag_zero_rounds, ]
  expect_true(all(unknown$operating_status == "zero_rounds_unknown"))
  expect_true(all(is.na(unknown$unused_sellable_rounds)))
})

test_that("the utilization calendar keeps missing days distinct and does not cap utilization", {
  sample <- dobson_operating_daily()
  calendar <- utilization_calendar(sample)
  missing <- calendar[calendar$date == as.Date("2024-12-10"), ]
  expect_equal(missing$operating_status, "missing")
  expect_false(missing$on_utilization_scale)
  zero <- calendar[calendar$date == as.Date("2025-10-06"), ]
  expect_equal(zero$operating_status, "zero_rounds_unknown")
  expect_true(is.na(zero$scale_utilization))
  expect_gt(max(calendar$scale_utilization, na.rm = TRUE), 1)
  expect_false(as.Date("2004-12-10") %in% calendar$date)
  expect_s3_class(plot_utilization_heatmap(sample), "ggplot")
})

test_that("seasonal overlays mark partial years and omit the isolated 2004 day", {
  sample <- dobson_operating_daily()
  overlay <- seasonal_overlay_data(sample, "rounds")
  expect_false(2004 %in% overlay$calendar_year)
  expect_true(any(grepl("from", overlay$year_label)))
  expect_true(any(grepl("missing day", overlay$year_label)))
  december <- overlay[overlay$calendar_year == 2024 & overlay$month == "December", ]
  expect_true(december$partial)
  expect_s3_class(plot_seasonal_overlay(sample, "utilization"), "ggplot")
  weekly <- summarize_golf_rounds(sample, by = "week", duplicates = "drop_extra")
  expect_s3_class(plot_participation(weekly), "ggplot")
})

test_that("course capacity counts starts inside the window and does not roll past midnight", {
  capacity <- course_capacity(
    date = as.Date(c("2024-01-05", "2024-01-06", "2024-01-07")),
    window_start = "07:00",
    window_end = c("15:00", "07:00", "06:30"),
    tee_interval_minutes = 12,
    players_per_start = 4
  )
  expect_equal(capacity$tee_times, c(floor(480 / 12), 0, 0))
  expect_equal(capacity$player_slots, c(floor(480 / 12) * 4, 0, 0))
  expect_error(
    course_capacity(as.Date("2024-01-05"), "07:00", "15:00", NA, 4),
    class = "golfops_input_error"
  )
  expect_error(
    course_capacity(as.Date("2024-01-05"), "07:00", "15:00", 10, 0),
    class = "golfops_input_error"
  )
})

test_that("daily utilization keeps slot use and tee-time occupancy separate", {
  capacity <- course_capacity(
    date = as.Date(c("2024-06-01", "2024-06-02", "2024-06-03")),
    window_start = "07:00",
    window_end = c("07:40", "07:10", "07:00"),
    tee_interval_minutes = 10,
    players_per_start = 4
  )
  bookings <- data.frame(
    date = as.Date(c("2024-06-01", "2024-06-01", "2024-06-01", "2024-06-01", "2024-06-02")),
    tee_time = c("07:00", "07:00", "07:10", "07:20", "07:00"),
    players = c(4, 0, 1, 4, 5)
  )
  use <- daily_utilization(bookings, capacity)
  saturday <- use[use$date == as.Date("2024-06-01"), ]
  expect_equal(saturday$occupied_tee_times, 3)
  expect_equal(saturday$players, 9)
  expect_equal(saturday$tee_time_occupancy, 3 / 4)
  expect_equal(saturday$slot_utilization, 9 / 16)
  sunday <- use[use$date == as.Date("2024-06-02"), ]
  expect_equal(sunday$slot_utilization, 5 / 4)
  expect_equal(sunday$tee_time_occupancy, 1)
  closed <- use[use$date == as.Date("2024-06-03"), ]
  expect_equal(closed$players, 0)
  expect_true(is.na(closed$slot_utilization))
  expect_true(is.na(closed$tee_time_occupancy))
  expect_error(
    daily_utilization(
      data.frame(date = as.Date("2024-06-04"), tee_time = "07:00", players = 2),
      capacity
    ),
    class = "golfops_input_error"
  )
})

test_that("available rounds outrank a schedule, and a missing interval is not 15 minutes", {
  prepared <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  sample <- dobson_operating_daily(prepared)
  calendar <- operating_calendar(
    date = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05")),
    status = c("open", "unknown", "closed", "open"),
    window_start = "07:00",
    window_end = "15:00",
    tee_interval_minutes = c(10, NA, 10, 12),
    players_per_start = 4,
    blocked_rounds = c(0, 0, 0, 8),
    available_rounds = c(80, NA, NA, NA)
  )
  resolved <- apply_operating_capacity(sample, calendar)
  sold <- resolved[resolved$date == as.Date("2024-01-02"), ]
  expect_equal(sold$capacity_basis, "available_rounds")
  expect_equal(sold$capacity_rounds, 80)
  expect_gt(sold$capacity_utilization, 1)
  expect_equal(sold$unused_sellable_rounds, 0)

  daylight <- resolved[resolved$date == as.Date("2024-01-03"), ]
  expect_equal(daylight$capacity_basis, "daylight_estimate")
  expect_equal(daylight$capacity_rounds, 200)

  closed <- resolved[resolved$date == as.Date("2024-01-04"), ]
  expect_equal(closed$operating_status, "confirmed_closure")
  expect_true(is.na(closed$unused_sellable_rounds))

  scheduled <- resolved[resolved$date == as.Date("2024-01-05"), ]
  expect_equal(scheduled$capacity_basis, "schedule_estimate")
  expect_equal(scheduled$capacity_rounds, floor(480 / 12) * 4 - 8)
})

test_that("revenue ratios are sums, refuse a zero green-fee denominator, and keep zero-round revenue", {
  prepared <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  sample <- dobson_operating_daily(prepared)
  ratios <- revenue_relationships(sample, by = "all")
  food <- ratios[ratios$category == "food_beverage", ]
  expect_equal(food$ratio_per_green_fee_dollar, 1350 / 26800)
  expect_equal(food$ratio_status, "ratio_of_sums")
  expect_gt(food$n_zero_round_rows_in_match, 0)

  silent <- course_revenue(
    course_id = "example",
    date = as.Date(c("2024-01-01", "2024-01-02")),
    frequency = "day",
    green_fee = c(0, 0),
    food_beverage = c(40, 10),
    cart_green_fee_treatment = "unknown"
  )
  zero_ratio <- revenue_relationships(silent)
  food_zero <- zero_ratio[zero_ratio$category == "food_beverage", ]
  expect_true(is.na(food_zero$ratio_per_green_fee_dollar))
  expect_equal(food_zero$ratio_status, "unavailable_zero_green_fee")
  expect_false(any(is.infinite(zero_ratio$ratio_per_green_fee_dollar)))
})

test_that("course tables reject mixed grains and a revenue join does not cross them", {
  expect_error(
    course_period(
      course_id = "example",
      date = as.Date(c("2024-01-01", "2024-02-01")),
      frequency = c("day", "month"),
      rounds = c(10, 3000)
    ),
    class = "golfops_input_error"
  )
  periods <- course_period(
    course_id = "example",
    date = as.Date(c("2024-01-01", "2024-02-01")),
    frequency = "day",
    rounds = c(10, 12)
  )
  revenue <- course_revenue(
    course_id = "example",
    date = as.Date(c("2024-01-01", "2024-02-01")),
    frequency = c("month", "month"),
    green_fee = c(100, 200)
  )
  joined <- join_course_revenue(periods, revenue)
  expect_true(all(is.na(joined$green_fee)))
  expect_error(
    course_revenue(
      course_id = "example",
      date = as.Date(c("2024-01-01", "2024-01-01")),
      frequency = "day",
      green_fee = c(1, 2)
    ),
    class = "golfops_duplicate_error"
  )
})

test_that("a partial tournament block requires the caller's displacement share", {
  prepared <- prepare_dobson_rounds(toy_dobson_csv(tempfile(fileext = ".csv")))
  sample <- dobson_operating_daily(prepared)
  dates <- as.Date(c("2024-01-02", "2024-01-03"))
  expect_error(
    tournament_breakeven(sample, dates, event_revenue = 1000, block = "partial_day"),
    class = "golfops_input_error"
  )
  full <- tournament_breakeven(sample, dates, event_revenue = 1000)
  expect_equal(full$totals$displaced_green_fee_revenue, 14000)
  expect_true(is.na(full$totals$contribution))
  expect_false(full$totals$ancillary_included_in_break_even)
  expect_match(paste(full$assumptions, collapse = " "), "not a recommended price")
  partial <- tournament_breakeven(
    sample,
    dates,
    event_revenue = 20000,
    event_costs = 1000,
    variable_cost_per_round = 2,
    ancillary_margin = 0.5,
    block = "partial_day",
    displacement_share = 0.5
  )
  expect_equal(partial$totals$displaced_green_fee_revenue, 7000)
  expect_false(is.na(partial$totals$contribution))
})

test_that("anomaly review stays with the measured days", {
  review <- review_dobson_anomalies()
  expect_match(review$observation[review$topic == "participation_spike"], "2025-04-28")
  expect_match(review$observation[review$topic == "range_revenue_extremes"], "last calendar day")
  expect_false(any(grepl("Super Bowl|charity", review$observation, ignore.case = TRUE)))
  expect_match(paste(review$not_established, collapse = " "), "does not name an event")
})

test_that("a forecast refuses observed weather and does not read altered holdout rounds", {
  expect_error(
    estimate_observed_activity(
      as.Date("2025-03-03"),
      role = "forecast",
      weather = course_weather("dobson_ranch", as.Date("2025-03-03"), "observed")
    ),
    class = "golfops_input_error"
  )
  rounds <- prepare_dobson_rounds()
  shifted <- rounds
  holdout <- shifted$date >= as.Date("2025-01-01") & shifted$grain == "day"
  shifted$rounds_played[holdout] <- shifted$rounds_played[holdout] + 1000
  baseline <- estimate_observed_activity(as.Date("2025-03-03"), rounds = rounds)
  altered <- estimate_observed_activity(as.Date("2025-03-03"), rounds = shifted)
  expect_equal(baseline$estimates$demand_model_rounds, altered$estimates$demand_model_rounds)
  expect_equal(baseline$estimates$horizon_days, 62)
  expect_equal(baseline$estimates$use, "forecast")
  expect_match(paste(baseline$limits, collapse = " "), "not a posted price")
  expect_false(isTRUE(all.equal(
    baseline$estimates$actual_rounds,
    altered$estimates$actual_rounds
  )))
})

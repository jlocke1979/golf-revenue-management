test_that("bundled Dobson weather covers the operating window and records its sources", {
  weather <- prepare_dobson_weather()
  expect_s3_class(weather, "golfops_course_weather")
  expect_equal(range(weather$date), as.Date(c("2021-07-01", "2026-04-30")))
  expect_equal(nrow(weather), 1765)
  expect_true(as.Date("2024-12-10") %in% weather$date)
  expect_true(all(weather$timing == "observed"))
  expect_true(all(weather$course_id == "dobson_ranch"))
  expect_equal(sum(weather$temp_missing), 0)
  expect_equal(sum(weather$precip_missing), 139)
  expect_equal(sum(weather$precip_airport_missing), 0)
  expect_equal(sum(weather$wind_missing), 2)
  expect_equal(sum(weather$precip_trace), 58)
  expect_equal(sum(weather$precip_multiday), 8)
  expect_true(all(abs(weather$temp_avg_c - (weather$temp_min_c + weather$temp_max_c) / 2) < 0.011))
  expect_true(all(weather$temp_max_c > 0 & weather$temp_max_c < 55))
  expect_true(all(weather$precip_mm[!weather$precip_missing] >= 0))
  expect_true(all(weather$wind_speed_ms[!weather$wind_missing] > 0))

  stations <- attr(weather, "stations")
  expect_lt(stations$distance_km[stations$role == "precipitation"], 2)
  expect_gt(stations$distance_km[stations$role == "temperature"], 10)
  expect_lt(stations$distance_km[stations$role == "temperature"], 20)
  expect_match(paste(attr(weather, "decisions"), collapse = " "), "GHCND")
  expect_match(paste(attr(weather, "decisions"), collapse = " "), "not copied into precip_mm")
  expect_equal(attr(weather, "retrieved"), "2026-11-19")
  expect_equal(attr(weather, "coverage")$n_missing[attr(weather, "coverage")$variable == "precip_mm"], 139)
})

test_that("a forecast still refuses the bundled observed weather", {
  weather <- prepare_dobson_weather()
  expect_error(
    estimate_observed_activity(
      as.Date("2025-03-03"),
      role = "forecast",
      weather = weather
    ),
    class = "golfops_input_error"
  )
})

test_that("playable capacity keeps theoretical capacity until a stated rule applies", {
  days <- data.frame(
    date = as.Date("2024-07-01") + 0:2,
    rounds_possible = c(200, 0, 80),
    daylight_hours = c(14, 14, 10),
    unavailable_hours = c(0, 0, 2.5),
    unavailable_fraction = c(0.25, 0, 0)
  )
  untouched <- playable_capacity(days)
  expect_equal(untouched$playable_capacity, days$rounds_possible)
  expect_equal(untouched$theoretical_capacity, days$rounds_possible)
  expect_true(all(untouched$restriction_source == "none"))
  expect_match(paste(attr(untouched, "assumptions"), collapse = " "), "High temperature does not")

  hours <- playable_capacity(
    days,
    unavailable_hours = "unavailable_hours",
    daylight_hours = "daylight_hours"
  )
  expect_equal(hours$playable_capacity, c(200, 0, 60))
  expect_equal(hours$unavailable_hours_stated, c(0, 0, 2.5))
  expect_equal(hours$restriction_source[[3]], "stated_unavailable_hours")

  fraction <- playable_capacity(days[1, ], unavailable_fraction = "unavailable_fraction")
  expect_equal(fraction$playable_capacity, 150)
  expect_equal(fraction$unavailable_fraction_applied, 0.25)
})

test_that("closures, missing statements, and impossible hours are handled explicitly", {
  days <- data.frame(
    date = as.Date(c("2024-10-11", "2024-10-12")),
    rounds_possible = c(180, 180),
    unavailable_fraction = c(0.5, NA_real_)
  )
  calendar <- operating_calendar(date = days$date[[1]], status = "closed")
  closed <- playable_capacity(
    days,
    calendar = calendar,
    unavailable_fraction = "unavailable_fraction"
  )
  expect_equal(closed$playable_capacity, c(0, NA_real_))
  expect_equal(closed$restriction_source, c("observed_closure", "stated_time_missing"))

  too_long <- data.frame(
    date = as.Date("2024-07-01"),
    rounds_possible = 100,
    daylight_hours = 10,
    unavailable_hours = 12
  )
  expect_error(
    playable_capacity(
      too_long,
      unavailable_hours = "unavailable_hours",
      daylight_hours = "daylight_hours"
    ),
    "exceed daylight",
    class = "golfops_input_error"
  )
  expect_error(
    playable_capacity(
      days,
      unavailable_hours = "unavailable_hours",
      unavailable_fraction = "unavailable_fraction",
      daylight_hours = "daylight_hours"
    ),
    class = "golfops_input_error"
  )
})

test_that("a weather threshold is an assumption and heat is not a capacity cut", {
  weather <- course_weather(
    "dobson_ranch",
    as.Date("2024-07-01") + 0:2,
    "observed",
    source = "test",
    temp_max_c = c(46, 47, 48),
    precip_mm = c(0, 12, NA_real_),
    wind_speed_ms = c(3, 3, 3)
  )
  days <- data.frame(
    date = weather$date,
    rounds_possible = c(220, 220, 220)
  )
  hot <- playable_capacity(days, weather = weather)
  expect_equal(hot$playable_capacity, days$rounds_possible)
  expect_true(all(!hot$weather_assumption_met))

  rain <- playable_capacity(days, weather = weather, precip_unplayable_mm = 10)
  expect_equal(rain$playable_capacity, c(220, 0, NA_real_))
  expect_equal(
    rain$restriction_source,
    c("none", "weather_assumption", "weather_missing")
  )
  expect_equal(rain$restriction[[2]], "precipitation")
  expect_true(is.na(rain$unavailable_hours_stated[[2]]))
  expect_match(
    paste(attr(rain, "assumptions"), collapse = " "),
    "not an observed closure"
  )

  calendar <- operating_calendar(date = days$date[[2]], status = "closed")
  both <- playable_capacity(
    days,
    calendar = calendar,
    weather = weather,
    precip_unplayable_mm = 10
  )
  expect_equal(both$restriction_source[[2]], "observed_closure")
  expect_equal(both$playable_capacity[[2]], 0)

  expect_error(
    playable_capacity(
      transform(days, unavailable_fraction = 0.2),
      unavailable_fraction = "unavailable_fraction",
      weather = weather,
      precip_unplayable_mm = 10
    ),
    class = "golfops_input_error"
  )
})

test_that("unexpected weather units and a missing weather column are rejected", {
  days <- data.frame(date = as.Date("2024-07-01"), rounds_possible = 100)
  fahrenheit <- course_weather(
    "dobson_ranch",
    days$date,
    "observed",
    temp_min_c = 110,
    precip_mm = 0,
    wind_speed_ms = 2
  )
  expect_error(
    playable_capacity(days, weather = fahrenheit),
    "degrees Celsius",
    class = "golfops_input_error"
  )
  tenths <- course_weather(
    "dobson_ranch",
    days$date,
    "observed",
    precip_mm = 627,
    wind_speed_ms = 2
  )
  expect_error(
    playable_capacity(days, weather = tenths, precip_unplayable_mm = 5),
    "millimeters",
    class = "golfops_input_error"
  )
  fast <- course_weather(
    "dobson_ranch",
    days$date,
    "observed",
    precip_mm = 0,
    wind_speed_ms = 55
  )
  expect_error(
    playable_capacity(days, weather = fast),
    "meters per second",
    class = "golfops_input_error"
  )
  expect_error(
    playable_capacity(
      days,
      weather = course_weather("dobson_ranch", days$date, "observed", precip_in = 0.4),
      precip_unplayable_mm = 5,
      precip_column = "precip_in"
    ),
    "_mm",
    class = "golfops_input_error"
  )
  bare <- course_weather("dobson_ranch", days$date, "observed", precip_mm = 1)
  expect_error(
    playable_capacity(days, weather = bare, wind_unplayable_ms = 8),
    "wind_speed_ms",
    class = "golfops_input_error"
  )
})

test_that("real hot Dobson weather does not reduce daylight capacity", {
  weather <- prepare_dobson_weather()
  hot_day <- weather$date[[which.max(weather$temp_max_c)]]
  days <- data.frame(date = hot_day, rounds_possible = 400)
  play <- playable_capacity(days, weather = weather)
  expect_gt(weather$temp_max_c[weather$date == hot_day], 40)
  expect_equal(play$theoretical_capacity, 400)
  expect_equal(play$playable_capacity, 400)
  expect_equal(play$restriction_source, "none")
})

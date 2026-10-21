test_that("the Peoria card keeps the posted sheet and drops empty cells", {
  fences <- peoria_rate_fences()
  card <- peoria_rate_card()
  expect_match(attr(card, "source"), "Peoria")
  expect_false(isTRUE(attr(card, "synthetic")))
  expect_false(any(c("quantity", "revenue", "hour") %in% names(card)))
  expect_equal(nrow(card), 114)
  expect_true(all(card$price > 0))
  expect_setequal(card$facility, c("Kellogg", "Madison", "Newman"))
  expect_equal(length(unique(paste(card$facility, card$product))), 10L)
  expect_true(all(fences$posted[fences$segment %in% c("Junior twilight", "Military", "Holiday 18 with cart")] == FALSE))
  expect_false(any(card$segment %in% c("Military", "Junior twilight")))

  executive <- card[card$sheet_name == "Kellogg_Executive_09" & card$segment == "Adult", ]
  expect_equal(
    executive$price[executive$day == "Weekday" & executive$daypart == "Day"],
    24
  )
  expect_equal(
    executive$price[executive$day == "Weekday" & executive$daypart == "Twilight"],
    30
  )

  twilight <- card[card$segment == "Adult" & card$daypart == "Twilight", ]
  expect_setequal(
    unique(paste(twilight$facility, twilight$product)),
    c("Kellogg Executive 9", "Kellogg Back 9", "Madison Back 9")
  )

  walking <- card[card$segment == "Adult" & card$day == "Weekday" & card$daypart == "Day", c("sheet_name", "price")]
  bundled <- card[card$segment == "Adult with cart" & card$day == "Weekday", c("sheet_name", "price")]
  carts <- card[card$segment == "Cart", c("sheet_name", "price")]
  names(walking)[2] <- "adult"
  names(bundled)[2] <- "bundle"
  names(carts)[2] <- "cart"
  checked <- merge(merge(walking, bundled, by = "sheet_name"), carts, by = "sheet_name")
  expect_equal(checked$bundle, checked$adult + checked$cart)

  kellogg_18 <- card[card$sheet_name == "Kellogg_18_Hole_Course", ]
  expect_equal(
    kellogg_18$price[kellogg_18$segment == "Adult with cart" & kellogg_18$day == "Weekend"],
    76
  )
  expect_equal(kellogg_18$price[kellogg_18$segment == "Cart"], 17)

  newman <- card[card$sheet_name == "Neuman_18_Hole_Course", ]
  expect_equal(newman$price[newman$segment == "Adult with cart" & newman$day == "Weekend"], 68)
  expect_equal(newman$price[newman$segment == "Club rental"], 20)
  madison <- card[card$sheet_name == "Madison_18_Hole_Course", ]
  expect_equal(madison$price[madison$segment == "Club rental"], 10)
  expect_true(all(card$price[card$segment == "Pull cart"] == 5))
  expect_true(all(card$price[card$segment == "Club per club"] == 1))
  expect_true(all(card$price[card$segment == "Cart" & card$holes == 9] == 12))
  expect_true(all(card$price[card$segment == "Cart" & card$holes == 18] == 17))
})

test_that("a chronological split does not put future rows in the training period", {
  tickets <- synthetic_tournament_tickets()
  parts <- split_chronological(tickets, as.Date("2025-01-01"))
  expect_true(max(parts$train$date) < as.Date("2025-01-01"))
  expect_true(min(parts$test$date) >= as.Date("2025-01-01"))
  expect_true(attr(tickets, "synthetic"))
  expect_match(paste(attr(tickets, "decisions"), collapse = " "), "Not PGA")
})

test_that("the demand model does not use test outcomes", {
  tickets <- synthetic_tournament_tickets()
  parts <- split_chronological(tickets, as.Date("2025-01-01"))
  original <- forecast_demand_model(parts$train, parts$test, outcome ~ weekday + days_to_event)
  parts$test$outcome <- parts$test$outcome + 1000
  shifted <- forecast_demand_model(parts$train, parts$test, outcome ~ weekday + days_to_event)
  expect_equal(original$predicted, shifted$predicted)
})

test_that("forecast scores match a hand calculation", {
  score <- evaluate_forecast(c(10, 12), c(8, 14), "toy")
  expect_equal(score$mae, 2)
  expect_equal(score$rmse, 2)
  expect_equal(score$mean_error, 0)
})

test_that("Dobson forecasts use a 2025 holdout and do not claim latent demand", {
  demand <- dobson_rounds_demand()
  expect_false(any(demand$date < as.Date("2021-07-01")))
  expect_equal(sum(duplicated(demand$date)), 0)
  expect_false(as.Date("2024-12-10") %in% demand$date)
  expect_true(all(demand$include_in_fit[demand$date >= as.Date("2025-10-06") &
                                           demand$date <= as.Date("2025-10-16")] == FALSE))

  forecast <- forecast_dobson_rounds(demand)
  expect_true(min(forecast$predictions$date) >= as.Date("2025-01-01"))
  expect_true(max(forecast$predictions$date) <= as.Date("2025-12-31"))
  expect_equal(forecast$n_train_rows, sum(demand$date < as.Date("2025-01-01")))
  expect_lt(forecast$n_train_fit, forecast$n_train_rows)
  expect_setequal(unique(forecast$comparison$model), c("seasonal_baseline", "demand_model"))
  expect_match(paste(forecast$limits, collapse = " "), "latent demand")
  expect_match(paste(forecast$limits, collapse = " "), "not a price elasticity")
  expect_false(isTRUE(forecast$synthetic))
})

test_that("the ticket example is a separate synthetic model", {
  forecast <- forecast_tournament_tickets()
  expect_true(forecast$synthetic)
  expect_match(forecast$series, "synthetic")
  expect_true("days_to_event" %in% names(stats::coef(forecast$model)) ||
                any(grepl("days_to_event", names(stats::coef(forecast$model)))))
  scenario <- apply_volume_scenario(forecast$predictions$demand_model, 0.10)
  expect_match(paste(scenario$assumptions, collapse = " "), "not a price elasticity")
  expect_equal(scenario$totals$scenario_total, sum(forecast$predictions$demand_model) * 1.10)
})

test_that("a forecast plot returns a ggplot object", {
  expect_s3_class(plot_forecast(forecast_tournament_tickets()), "ggplot")
})

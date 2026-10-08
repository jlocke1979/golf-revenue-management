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

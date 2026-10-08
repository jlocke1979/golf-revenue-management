#' Estimate Dobson rounds and green-fee revenue for specific dates
#'
#' Uses the same chronological design as [forecast_dobson_rounds()]: a
#' weekday-and-month seasonal mean, and one linear model of observed rounds on
#' weekday and month. Both are fit on played days before the holdout. The
#' residual band is the 10th and 90th percentile of eligible holdout errors.
#' It is an empirical band from that holdout, not a regression confidence
#' interval, and it is not widened automatically for a longer horizon.
#'
#' Green-fee revenue is the rounds estimate times realized green-fee yield on
#' played training days. Yield is a ratio of sums. It is not a posted price.
#' The band around revenue uses that same yield, so it does not add a separate
#' yield uncertainty.
#'
#' Weather is accepted only to enforce a timing rule. Observed weather cannot
#' be used when `role = "forecast"`. No weather term is added, because the
#' extract does not include a validated weather series. An event table is
#' listed beside the date and does not change the number.
#'
#' @param dates Dates to estimate.
#' @param rounds Optional result of [prepare_dobson_rounds()].
#' @param holdout_start,holdout_end Chronological split used to fit and to
#'   build the residual band.
#' @param role `forecast` refuses observed weather. `retrospective` allows it
#'   and still does not add it to the formula.
#' @param weather Optional [course_weather()] table.
#' @param events Optional [course_events()] table.
#' @return A `golfops_activity_estimate` list.
#' @export
estimate_observed_activity <- function(
    dates,
    rounds = NULL,
    holdout_start = as.Date("2025-01-01"),
    holdout_end = as.Date("2025-12-31"),
    role = c("forecast", "retrospective"),
    weather = NULL,
    events = NULL) {
  role <- match.arg(role)
  dates <- as.Date(dates)
  if (length(dates) == 0 || any(is.na(dates))) {
    abort("`dates` must be complete.", class = "golfops_input_error")
  }
  if (any(duplicated(dates))) {
    abort("`dates` are repeated.", class = "golfops_input_error")
  }
  .reject_weather_timing(weather, role)
  sample <- dobson_operating_daily(rounds)
  demand <- dobson_rounds_demand(rounds)
  demand <- demand[demand$date <= as.Date(holdout_end), , drop = FALSE]
  forecast <- compare_demand_forecasts(
    demand,
    holdout_start = holdout_start,
    formula = outcome ~ weekday + month,
    by = c("weekday", "month")
  )
  information_time <- as.Date(holdout_start) - 1
  yield_rows <- sample[
    sample$date < as.Date(holdout_start) & sample$rounds_played > 0,
    ,
    drop = FALSE
  ]
  yield <- sum(yield_rows$green_fee_revenue) / sum(yield_rows$rounds_played)
  parts <- split_chronological(demand, holdout_start)
  newdata <- prepare_demand(
    data.frame(date = dates, outcome = rep(0, length(dates)), closure = FALSE),
    date = "date",
    outcome = "outcome",
    closure = "closure",
    series = "dobson_estimate_dates",
    decisions = "Placeholder outcomes are not used in the prediction."
  )
  baseline <- forecast_seasonal_baseline(parts$train, newdata, by = c("weekday", "month"))
  model_prediction <- as.numeric(stats::predict(forecast$model, newdata = newdata))
  eligible <- forecast$predictions[!forecast$predictions$closure, , drop = FALSE]
  baseline_error <- stats::quantile(
    eligible$seasonal_baseline - eligible$actual,
    c(0.1, 0.9)
  )
  model_error <- stats::quantile(
    eligible$demand_model - eligible$actual,
    c(0.1, 0.9)
  )
  order_back <- match(dates, newdata$date)
  actual <- sample$rounds_played[match(dates, sample$date)]
  actual_green <- sample$green_fee_revenue[match(dates, sample$date)]
  event_name <- rep("none supplied", length(dates))
  if (!is.null(events)) {
    if (!inherits(events, "golfops_course_events")) {
      abort("Expected events from course_events().", class = "golfops_input_error")
    }
    event_name <- vapply(dates, function(day) {
      listed <- events$event_name[events$date %in% day]
      if (length(listed) == 0) "not listed" else paste(listed, collapse = "; ")
    }, character(1))
  }
  estimates <- tibble(
    date = dates,
    horizon_days = as.integer(dates - information_time),
    use = ifelse(dates <= information_time, "in_sample_pattern", "forecast"),
    actual_rounds = actual,
    seasonal_baseline_rounds = pmax(0, baseline[order_back]),
    demand_model_rounds = pmax(0, model_prediction[order_back]),
    seasonal_baseline_rounds_p10 = pmax(0, baseline[order_back] + as.numeric(baseline_error[1])),
    seasonal_baseline_rounds_p90 = pmax(0, baseline[order_back] + as.numeric(baseline_error[2])),
    demand_model_rounds_p10 = pmax(0, model_prediction[order_back] + as.numeric(model_error[1])),
    demand_model_rounds_p90 = pmax(0, model_prediction[order_back] + as.numeric(model_error[2])),
    actual_green_fee_revenue = actual_green,
    seasonal_baseline_green_fee = pmax(0, baseline[order_back]) * yield,
    demand_model_green_fee = pmax(0, model_prediction[order_back]) * yield,
    listed_event = event_name
  )
  limits <- c(
    sprintf(
      "Information time is %s, the day before the holdout. horizon_days is the number of days after that date.",
      information_time
    ),
    "The residual band uses the 10th and 90th percentiles of eligible holdout errors. It is not a confidence interval and it does not grow with the horizon.",
    "Predictions below zero are shown as zero. Predictions are not capped at the daylight estimate and are not raised on days above it.",
    sprintf(
      "Green-fee revenue applies a training yield of $%s per played round, a ratio of sums. It is not a posted price.",
      format(round(yield, 2), nsmall = 2)
    ),
    "No weather variable is in the formula. Observed weather is refused for role = 'forecast'.",
    "Listed events do not change the estimate. The model does not invent an event explanation."
  )
  structure(
    list(
      estimates = estimates,
      yield_per_played_round = yield,
      information_time = information_time,
      role = role,
      limits = limits
    ),
    class = "golfops_activity_estimate"
  )
}

#' @export
print.golfops_activity_estimate <- function(x, ...) {
  cat(sprintf(
    "Observed-activity estimate (%s). Information time %s. Training yield $%.2f per played round.\n\n",
    x$role,
    x$information_time,
    x$yield_per_played_round
  ))
  print(x$estimates)
  cat("\n")
  cat(paste0("- ", x$limits), sep = "\n")
  invisible(x)
}

.reject_weather_timing <- function(weather, role) {
  if (is.null(weather)) {
    return(invisible(NULL))
  }
  if (!inherits(weather, "golfops_course_weather")) {
    abort("Expected weather from course_weather().", class = "golfops_input_error")
  }
  if (role == "forecast" && any(weather$timing == "observed")) {
    abort(
      "A forecast cannot use observed weather. Supply timing = 'available_at_forecast', or set role = 'retrospective'.",
      class = "golfops_input_error"
    )
  }
  invisible(NULL)
}

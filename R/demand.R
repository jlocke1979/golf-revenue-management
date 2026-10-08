#' Prepare a demand series for a shared forecast workflow
#'
#' The outcome is the observed count. Rows flagged as closures stay in the
#' table so they can be scored, and they are left out of model fitting.
#' Capacity, when supplied, is stored for documentation. It is not used to
#' invent unobserved demand.
#'
#' @param data A data frame.
#' @param date,outcome Column names for the date and the observed outcome.
#' @param closure Optional column name. TRUE marks a row to keep out of the fit.
#' @param capacity Optional column name for a capacity estimate.
#' @param keep Extra column names to retain.
#' @param series Short name for this application.
#' @param synthetic TRUE when the rows were generated rather than observed.
#' @param decisions Character vector of data decisions to store on the result.
#' @return A `golfops_demand` tibble.
#' @export
prepare_demand <- function(
    data,
    date,
    outcome,
    closure = NULL,
    capacity = NULL,
    keep = character(),
    series = "demand",
    synthetic = FALSE,
    decisions = character()) {
  if (!is.data.frame(data)) {
    abort("`data` must be a data frame.", class = "golfops_input_error")
  }
  required <- c(date, outcome, closure, capacity, keep)
  required <- required[!vapply(required, is.null, logical(1))]
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    abort(
      paste0("Missing columns: ", paste(missing, collapse = ", ")),
      class = "golfops_input_error"
    )
  }
  date_values <- as.Date(data[[date]])
  outcome_values <- as.numeric(data[[outcome]])
  if (any(is.na(date_values)) || any(is.na(outcome_values))) {
    abort(
      "Dates and outcomes must be complete.",
      class = "golfops_input_error"
    )
  }
  closure_values <- if (is.null(closure)) {
    rep(FALSE, nrow(data))
  } else {
    as.logical(data[[closure]])
  }
  if (any(is.na(closure_values))) {
    abort("Closure flags must be TRUE or FALSE.", class = "golfops_input_error")
  }
  capacity_values <- if (is.null(capacity)) {
    rep(NA_real_, nrow(data))
  } else {
    as.numeric(data[[capacity]])
  }

  out <- tibble(
    date = date_values,
    outcome = outcome_values,
    weekday = factor(expected_weekday(date_values), levels = weekday_names()),
    month = factor(month_names()[month(date_values)], levels = month_names()),
    closure = closure_values,
    include_in_fit = !closure_values,
    capacity = capacity_values,
    above_capacity = !is.na(capacity_values) & outcome_values > capacity_values
  )
  for (column in keep) {
    out[[column]] <- data[[column]]
  }
  out <- out[order(out$date), , drop = FALSE]
  .new_demand(out, series, synthetic, decisions)
}

.new_demand <- function(data, series, synthetic, decisions) {
  data <- as_tibble(data)
  class(data) <- unique(c("golfops_demand", class(data)))
  attr(data, "series") <- series
  attr(data, "synthetic") <- isTRUE(synthetic)
  attr(data, "decisions") <- as.character(decisions)
  data
}

.require_demand <- function(data) {
  required <- c("date", "outcome", "weekday", "month", "include_in_fit", "closure")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    abort(
      paste0("Expected a table from prepare_demand(). Missing: ", paste(missing, collapse = ", ")),
      class = "golfops_input_error"
    )
  }
  if (!inherits(data, "golfops_demand")) {
    data <- .new_demand(
      data,
      attr(data, "series") %||% "demand",
      attr(data, "synthetic") %||% FALSE,
      attr(data, "decisions") %||% character()
    )
  }
  data
}

#' Split a demand series on a date, without shuffling
#'
#' Training rows are strictly before `holdout_start`. Later rows are the test.
#'
#' @param data A `golfops_demand` table.
#' @param holdout_start First date of the test period.
#' @return A list with `train` and `test`.
#' @export
split_chronological <- function(data, holdout_start) {
  data <- .require_demand(data)
  holdout_start <- as.Date(holdout_start)
  if (length(holdout_start) != 1 || is.na(holdout_start)) {
    abort("`holdout_start` must be one date.", class = "golfops_input_error")
  }
  train <- data[data$date < holdout_start, , drop = FALSE]
  test <- data[data$date >= holdout_start, , drop = FALSE]
  if (nrow(train) == 0 || nrow(test) == 0) {
    abort(
      "The holdout date leaves either the training period or the test period empty.",
      class = "golfops_input_error"
    )
  }
  list(
    train = .require_demand(train),
    test = .require_demand(test),
    holdout_start = holdout_start
  )
}

#' Forecast with training means for the same calendar keys
#'
#' The default keys are weekday and month. A test row whose key never appears
#' in the fitting rows uses the weekday mean, then the overall fitting mean.
#' Means are computed only from rows with `include_in_fit` TRUE.
#'
#' @param train,test Demand tables from [split_chronological()].
#' @param by Column names used as the seasonal key.
#' @return A numeric vector with one prediction per test row.
#' @export
forecast_seasonal_baseline <- function(train, test, by = c("weekday", "month")) {
  train <- .require_demand(train)
  test <- .require_demand(test)
  missing <- setdiff(by, names(train))
  if (length(missing) > 0) {
    abort(
      paste0("Baseline keys are not on the series: ", paste(missing, collapse = ", ")),
      class = "golfops_input_error"
    )
  }
  fit <- train[train$include_in_fit, , drop = FALSE]
  if (nrow(fit) == 0) {
    abort("No rows are marked include_in_fit in the training period.", class = "golfops_input_error")
  }
  overall <- mean(fit$outcome)
  weekday_mean <- tapply(fit$outcome, fit$weekday, mean)
  key_mean <- tapply(fit$outcome, .demand_key(fit, by), mean)
  predicted <- key_mean[.demand_key(test, by)]
  missed <- is.na(predicted)
  if (any(missed)) {
    predicted[missed] <- weekday_mean[as.character(test$weekday[missed])]
  }
  predicted[is.na(predicted)] <- overall
  unname(as.numeric(predicted))
}

#' Forecast with one interpretable linear model
#'
#' The default formula is observed outcome on weekday and month. Fit it on a
#' different formula for a different application. The model sees training rows
#' with `include_in_fit` TRUE and does not see test outcomes.
#'
#' Predictions are not clipped at capacity. A prediction above capacity is
#' still a prediction of observed play, not an estimate of unmet demand.
#'
#' @param train,test Demand tables from [split_chronological()].
#' @param formula A model formula. The outcome column must be named `outcome`.
#' @return A list with `predicted` and `model`.
#' @export
forecast_demand_model <- function(train, test, formula = outcome ~ weekday + month) {
  train <- .require_demand(train)
  test <- .require_demand(test)
  fit <- train[train$include_in_fit, , drop = FALSE]
  if (nrow(fit) == 0) {
    abort("No rows are marked include_in_fit in the training period.", class = "golfops_input_error")
  }
  model <- stats::lm(formula, data = fit)
  list(
    predicted = as.numeric(stats::predict(model, newdata = test)),
    model = model
  )
}

#' Score forecasts against observed outcomes
#'
#' @param actual,predicted Numeric vectors of the same length.
#' @param slice Label for this score.
#' @return A one-row tibble with MAE, RMSE, and mean error (prediction minus actual).
#' @export
evaluate_forecast <- function(actual, predicted, slice = "all_rows") {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted) || length(actual) == 0) {
    abort("`actual` and `predicted` must have the same positive length.", class = "golfops_input_error")
  }
  if (any(is.na(actual)) || any(is.na(predicted))) {
    abort("Forecast scores cannot include missing actuals or predictions.", class = "golfops_input_error")
  }
  error <- predicted - actual
  tibble(
    slice = slice,
    n = length(actual),
    mae = mean(abs(error)),
    rmse = sqrt(mean(error^2)),
    mean_error = mean(error)
  )
}

#' Compare a seasonal baseline with one demand model
#'
#' The split is chronological. Both forecasts are fit on the training period
#' only. Scores are reported for every test row and, when some test rows were
#' kept out of the fit, for the test rows that were eligible for fitting.
#'
#' @param data A `golfops_demand` table.
#' @param holdout_start First test date.
#' @param formula Formula for [forecast_demand_model()].
#' @param by Keys for [forecast_seasonal_baseline()].
#' @return A `golfops_forecast` list.
#' @export
compare_demand_forecasts <- function(
    data,
    holdout_start,
    formula = outcome ~ weekday + month,
    by = c("weekday", "month")) {
  data <- .require_demand(data)
  parts <- split_chronological(data, holdout_start)
  baseline <- forecast_seasonal_baseline(parts$train, parts$test, by = by)
  fitted <- forecast_demand_model(parts$train, parts$test, formula = formula)
  comparison <- rbind(
    evaluate_forecast(parts$test$outcome, baseline, "all_rows") |>
      mutate(model = "seasonal_baseline", .before = 1),
    evaluate_forecast(parts$test$outcome, fitted$predicted, "all_rows") |>
      mutate(model = "demand_model", .before = 1)
  )
  eligible <- parts$test$include_in_fit
  if (any(!eligible)) {
    comparison <- rbind(
      comparison,
      evaluate_forecast(parts$test$outcome[eligible], baseline[eligible], "rows_eligible_for_fit") |>
        mutate(model = "seasonal_baseline", .before = 1),
      evaluate_forecast(parts$test$outcome[eligible], fitted$predicted[eligible], "rows_eligible_for_fit") |>
        mutate(model = "demand_model", .before = 1)
    )
  }
  predictions <- tibble(
    date = parts$test$date,
    actual = parts$test$outcome,
    seasonal_baseline = baseline,
    demand_model = fitted$predicted,
    closure = parts$test$closure,
    above_capacity = parts$test$above_capacity
  )
  limits <- c(
    "The forecasts predict the observed outcome. They do not estimate latent demand on sold-out or above-capacity days.",
    "Predictions are not capped at capacity, and days above capacity are not revised upward.",
    "The volume scenario, if used, is an assumption supplied by the user. It is not a price elasticity.",
    "Rows flagged for exclusion stay in the all-rows score and are omitted from the eligible-rows score. For Dobson Ranch those rows are zero-round days of unknown status, not confirmed closures. A prediction on one of those dates is expected play if the day had been open."
  )
  structure(
    list(
      comparison = comparison,
      predictions = predictions,
      model = fitted$model,
      holdout_start = parts$holdout_start,
      n_train_rows = nrow(parts$train),
      n_train_fit = sum(parts$train$include_in_fit),
      decisions = attr(data, "decisions"),
      limits = limits,
      series = attr(data, "series"),
      synthetic = attr(data, "synthetic")
    ),
    class = "golfops_forecast"
  )
}

#' Rescale a forecast by an assumed volume change
#'
#' @param predicted Numeric predictions.
#' @param volume_change Fractional change. A value of 0.05 means 5 percent more volume.
#' @return A `golfops_scenario` list whose totals are the assumed counts.
#' @export
apply_volume_scenario <- function(predicted, volume_change = 0) {
  if (!is.numeric(volume_change) || length(volume_change) != 1 ||
      is.na(volume_change) || volume_change <= -1) {
    abort("`volume_change` must be a single number greater than -1.", class = "golfops_input_error")
  }
  predicted <- as.numeric(predicted)
  scenario <- predicted * (1 + volume_change)
  assumptions <- c(
    "volume_change rescales the forecast. It is not estimated from the series.",
    "This rescaling is not a price elasticity and does not recover latent demand above capacity."
  )
  structure(
    list(
      totals = tibble(
        volume_change = volume_change,
        baseline_total = sum(predicted),
        scenario_total = sum(scenario),
        change_total = sum(scenario) - sum(predicted)
      ),
      daily = tibble(predicted = predicted, scenario = scenario),
      assumptions = assumptions
    ),
    class = "golfops_scenario"
  )
}

#' Plot holdout actuals against both forecasts
#'
#' `interval = "week"` sums the holdout to weeks beginning Monday. Weekly
#' totals are easier to read than a daily line. They still include zero-round
#' days in the actual total.
#'
#' @param forecast A result from [compare_demand_forecasts()].
#' @param interval `day` keeps one point per holdout date. `week` sums to the
#'   Monday of each week.
#' @return A ggplot object.
#' @export
plot_forecast <- function(forecast, interval = c("day", "week")) {
  if (!inherits(forecast, "golfops_forecast")) {
    abort("Expected a result from compare_demand_forecasts().", class = "golfops_input_error")
  }
  interval <- match.arg(interval)
  predictions <- forecast$predictions
  if (interval == "week") {
    predictions <- predictions |>
      mutate(date = floor_date(.data$date, "week", week_start = 1)) |>
      group_by(.data$date) |>
      summarise(
        actual = sum(.data$actual),
        seasonal_baseline = sum(.data$seasonal_baseline),
        demand_model = sum(.data$demand_model),
        .groups = "drop"
      )
  }
  long <- tibble(
    date = rep(predictions$date, 3),
    series = factor(
      rep(c("Actual", "Seasonal baseline", "Demand model"), each = nrow(predictions)),
      levels = c("Actual", "Seasonal baseline", "Demand model")
    ),
    value = c(predictions$actual, predictions$seasonal_baseline, predictions$demand_model)
  )
  ggplot(long, aes(
    x = .data$date,
    y = .data$value,
    color = .data$series,
    linetype = .data$series
  )) +
    geom_line(linewidth = 0.55) +
    scale_color_manual(values = c(
      "Actual" = "#000000",
      "Seasonal baseline" = "#0072B2",
      "Demand model" = "#E69F00"
    )) +
    ggplot2::scale_linetype_manual(values = c(
      "Actual" = "solid",
      "Seasonal baseline" = "dashed",
      "Demand model" = "solid"
    )) +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title = if (interval == "week") {
        "Holdout forecast of observed demand, by week"
      } else {
        "Holdout forecast of observed demand"
      },
      subtitle = sprintf(
        "%s%s",
        forecast$series,
        if (isTRUE(forecast$synthetic)) " (synthetic)" else ""
      ),
      x = NULL,
      y = "Observed outcome",
      color = NULL,
      caption = if (interval == "week") {
        "Weekly sums of the chronological holdout, including zero-round days. Predictions are observed counts, not latent demand and not a price effect."
      } else {
        "Chronological holdout. Predictions are observed counts, not latent demand and not a price effect."
      }
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' @export
print.golfops_forecast <- function(x, ...) {
  cat(sprintf(
    "Demand forecast for %s%s\nHoldout starts %s. Training rows: %s. Rows used in the fit: %s.\n\n",
    x$series,
    if (isTRUE(x$synthetic)) " [synthetic]" else "",
    x$holdout_start,
    x$n_train_rows,
    x$n_train_fit
  ))
  print(x$comparison)
  cat("\nLimits\n")
  cat(paste0("- ", x$limits), sep = "\n")
  invisible(x)
}

.demand_key <- function(data, by) {
  do.call(paste, c(data[by], list(sep = "|")))
}

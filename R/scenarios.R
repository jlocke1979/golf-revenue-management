#' Apply a user-specified price and volume scenario
#'
#' This rescales observed green-fee yield and observed rounds. It does not
#' estimate how many additional rounds a price change would produce. Set
#' `price_change = 0.10` to mean a 10 percent higher realized green-fee yield,
#' not a 10 percent higher posted rack rate. Set `volume_change` yourself.
#'
#' Ancillary revenue is total revenue minus green fees. `hold_constant` leaves
#' that remainder at its observed dollar amount. `scale_with_rounds` multiplies
#' it by the same ratio applied to rounds after any daylight cap.
#'
#' The daylight cap uses `rounds_possible`. It is applied only when the
#' scenario rounds exceed that estimate and the observed rounds did not
#' already exceed it. Days that already exceed the estimate are left uncapped,
#' because forcing them down would change the baseline. Days with zero played
#' rounds are not rescaled, because yield is undefined.
#'
#' @param data Daily rows from [prepare_dobson_rounds()]. Monthly rows are
#'   rejected.
#' @param price_change Fractional change in realized green-fee yield. Must be
#'   greater than -1.
#' @param volume_change Fractional change in played rounds. Must be greater
#'   than -1.
#' @param ancillary How to treat revenue other than green fees.
#' @param respect_daylight_capacity Whether to cap scenario rounds at
#'   `rounds_possible` when observed play did not already exceed it.
#' @return A `golfops_scenario` list with `totals`, `daily`, and `assumptions`.
#' @export
#' @examples
#' rounds <- prepare_dobson_rounds()
#' y2025 <- rounds[rounds$calendar_year == 2025 & rounds$grain == "day", ]
#' simulate_price_volume(y2025, price_change = 0.10, volume_change = -0.05)
simulate_price_volume <- function(
    data,
    price_change = 0,
    volume_change = 0,
    ancillary = c("hold_constant", "scale_with_rounds"),
    respect_daylight_capacity = TRUE) {
  data <- .require_rounds(data)
  ancillary <- match.arg(ancillary)
  if (!is.numeric(price_change) || length(price_change) != 1 ||
      is.na(price_change) || price_change <= -1) {
    abort(
      "`price_change` must be a single number greater than -1.",
      class = "golfops_input_error"
    )
  }
  if (!is.numeric(volume_change) || length(volume_change) != 1 ||
      is.na(volume_change) || volume_change <= -1) {
    abort(
      "`volume_change` must be a single number greater than -1.",
      class = "golfops_input_error"
    )
  }
  if (!isTRUE(respect_daylight_capacity) && !isFALSE(respect_daylight_capacity)) {
    abort(
      "`respect_daylight_capacity` must be TRUE or FALSE.",
      class = "golfops_input_error"
    )
  }
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort(
      "Price and volume scenarios use daily rows. Filter out grain == 'month' first.",
      class = "golfops_input_error"
    )
  }
  if (any(data$flag_duplicate_date)) {
    abort(
      "Duplicate dates are still in the table. Drop the extra copy before running a scenario.",
      class = "golfops_duplicate_error"
    )
  }
  if (any(is.na(data$green_fee_revenue) | is.na(data$total_revenue) | is.na(data$rounds_played))) {
    abort(
      "Every row needs rounds played, green-fee revenue, and total revenue.",
      class = "golfops_input_error"
    )
  }

  zero <- data$rounds_played == 0
  already_over <- !is.na(data$rounds_possible) &
    data$rounds_played > data$rounds_possible
  scenario_rounds <- data$rounds_played * (1 + volume_change)
  scenario_rounds[zero] <- data$rounds_played[zero]
  capped <- rep(FALSE, nrow(data))
  if (respect_daylight_capacity) {
    can_cap <- !zero & !already_over & !is.na(data$rounds_possible) &
      scenario_rounds > data$rounds_possible
    scenario_rounds[can_cap] <- data$rounds_possible[can_cap]
    capped[can_cap] <- TRUE
  }

  yield <- ifelse(zero, NA_real_, data$green_fee_revenue / data$rounds_played)
  scenario_green <- ifelse(
    zero,
    data$green_fee_revenue,
    scenario_rounds * yield * (1 + price_change)
  )
  baseline_ancillary <- data$total_revenue - data$green_fee_revenue
  volume_ratio <- ifelse(zero, NA_real_, scenario_rounds / data$rounds_played)
  scenario_ancillary <- if (ancillary == "hold_constant") {
    baseline_ancillary
  } else {
    ifelse(zero, baseline_ancillary, baseline_ancillary * volume_ratio)
  }
  scenario_total <- scenario_green + scenario_ancillary

  daily <- tibble(
    date = data$date,
    baseline_rounds = data$rounds_played,
    scenario_rounds = scenario_rounds,
    baseline_green_fee_revenue = data$green_fee_revenue,
    scenario_green_fee_revenue = scenario_green,
    baseline_ancillary_revenue = baseline_ancillary,
    scenario_ancillary_revenue = scenario_ancillary,
    baseline_total_revenue = data$total_revenue,
    scenario_total_revenue = scenario_total,
    capped_at_daylight_estimate = capped,
    zero_rounds_unchanged = zero,
    already_above_daylight_estimate = already_over
  )

  played <- !zero
  yield_baseline <- sum(data$green_fee_revenue[played]) / sum(data$rounds_played[played])
  yield_scenario <- sum(scenario_green[played]) / sum(scenario_rounds[played])
  totals <- tibble(
    price_change = price_change,
    volume_change = volume_change,
    ancillary = ancillary,
    respect_daylight_capacity = respect_daylight_capacity,
    n_days = nrow(data),
    n_days_capped = sum(capped),
    n_zero_round_days = sum(zero),
    n_days_already_above_daylight = sum(already_over),
    baseline_rounds = sum(data$rounds_played),
    scenario_rounds = sum(scenario_rounds),
    baseline_green_fee_revenue = sum(data$green_fee_revenue),
    scenario_green_fee_revenue = sum(scenario_green),
    baseline_ancillary_revenue = sum(baseline_ancillary),
    scenario_ancillary_revenue = sum(scenario_ancillary),
    baseline_total_revenue = sum(data$total_revenue),
    scenario_total_revenue = sum(scenario_total),
    change_total_revenue = sum(scenario_total) - sum(data$total_revenue),
    green_fee_yield_baseline = yield_baseline,
    green_fee_yield_scenario = yield_scenario
  )

  assumptions <- c(
    "price_change rescales realized green-fee revenue per played round. It is not a change in a posted rack rate.",
    "volume_change is an assumption supplied for this scenario. It is not estimated from these rows.",
    if (ancillary == "hold_constant") {
      "Ancillary revenue, defined as total revenue minus green fees, stays at the observed dollar amount."
    } else {
      "Ancillary revenue scales by the same ratio as scenario rounds to observed rounds, after any daylight cap. That ratio is assumed, not estimated."
    },
    if (respect_daylight_capacity) {
      "Scenario rounds are capped at rounds possible only when observed rounds did not already exceed that daylight estimate."
    } else {
      "Scenario rounds are not capped at rounds possible."
    },
    "Rows with zero played rounds keep their observed revenue. Yield is undefined when the denominator is zero.",
    "This scenario is not an estimate of price elasticity and not an optimal tee-time price."
  )

  structure(
    list(totals = totals, daily = daily, assumptions = assumptions),
    class = "golfops_scenario"
  )
}

#' @export
print.golfops_scenario <- function(x, ...) {
  cat("Price and volume scenario\n")
  cat("The totals apply the assumptions below. They are not an estimated demand response.\n\n")
  cat(paste0("- ", x$assumptions), sep = "\n")
  cat("\n")
  print(x$totals)
  invisible(x)
}

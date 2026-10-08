#' Dobson Ranch daily rounds as a demand series
#'
#' Uses [dobson_operating_daily()]. Zero-round days stay in the series and out
#' of the fit because their status is unknown, not because the extract confirms
#' a closure. `capacity` is the city's daylight estimate. Days above that
#' estimate remain observed play.
#'
#' @param rounds Optional result of [prepare_dobson_rounds()]. The bundled
#'   extract is read when this is omitted.
#' @return A `golfops_demand` table.
#' @export
dobson_rounds_demand <- function(rounds = NULL) {
  daily <- dobson_operating_daily(rounds)
  decisions <- c(
    attr(daily, "decisions"),
    "Zero-round days stay in the scored series and out of the fit. Leaving them out is not a finding that the course was closed.",
    "Forecasts store the daylight estimate as capacity. They do not add latent demand on days at or above that estimate."
  )
  prepare_demand(
    daily,
    date = "date",
    outcome = "rounds_played",
    closure = "flag_zero_rounds",
    capacity = "rounds_possible",
    series = "dobson_ranch_daily_rounds",
    synthetic = FALSE,
    decisions = decisions
  )
}

#' Compare Dobson Ranch round forecasts on a chronological holdout
#'
#' The seasonal baseline is the training mean for the same weekday and month.
#' The demand model is a linear regression of observed rounds on weekday and
#' month. The default holdout is calendar year 2025.
#'
#' @param demand Optional series from [dobson_rounds_demand()].
#' @param holdout_start First date of the test period.
#' @param holdout_end Last date included in the scored series. The default
#'   keeps calendar 2025 and leaves the partial 2026 year out of the score.
#' @return A `golfops_forecast` list.
#' @export
#' @examples
#' forecast_dobson_rounds()
forecast_dobson_rounds <- function(
    demand = NULL,
    holdout_start = as.Date("2025-01-01"),
    holdout_end = as.Date("2025-12-31")) {
  if (is.null(demand)) {
    demand <- dobson_rounds_demand()
  }
  demand <- .require_demand(demand)
  demand <- demand[demand$date <= as.Date(holdout_end), , drop = FALSE]
  compare_demand_forecasts(
    demand,
    holdout_start = holdout_start,
    formula = outcome ~ weekday + month,
    by = c("weekday", "month")
  )
}

#' Synthetic ticket sales for a hypothetical golf tournament
#'
#' Builds ninety advance-sale days for each of three fictional editions of the
#' Cedar Municipal Classic. Counts rise toward the event and are higher on
#' weekends, plus noise. This is not a PGA Tour series and not Dobson Ranch.
#'
#' @param seed Random seed used before the noise is drawn.
#' @return A `golfops_demand` table. The `days_to_event` column is retained
#'   for the ticket model.
#' @export
synthetic_tournament_tickets <- function(seed = 457) {
  set.seed(seed)
  editions <- data.frame(
    edition = c(2023, 2024, 2025),
    event_date = as.Date(c("2023-04-15", "2024-04-13", "2025-04-12"))
  )
  rows <- do.call(rbind, lapply(seq_len(nrow(editions)), function(i) {
    event_date <- editions$event_date[i]
    date <- event_date - seq.int(89, 0)
    days_to_event <- as.integer(event_date - date)
    weekend <- expected_weekday(date) %in% c("Saturday", "Sunday")
    expected <- 70 + 30 * weekend + 0.8 * (89 - days_to_event)
    tickets <- pmax(0, round(expected + stats::rnorm(length(date), sd = 8)))
    data.frame(
      date = date,
      tickets = tickets,
      days_to_event = days_to_event,
      edition = editions$edition[i]
    )
  }))
  decisions <- c(
    "Hypothetical ticket counts for the Cedar Municipal Classic, a fictional municipal tournament.",
    "Not PGA Tour sales, not Dobson Ranch revenue, and not an observed box office.",
    "The generator uses a weekend lift and a rise as the event approaches, plus normal noise at seed 457."
  )
  prepare_demand(
    rows,
    date = "date",
    outcome = "tickets",
    keep = c("days_to_event", "edition"),
    series = "synthetic_cedar_municipal_classic_tickets",
    synthetic = TRUE,
    decisions = decisions
  )
}

#' Forecast the synthetic tournament with its own model
#'
#' Uses the same split, baseline, and score as Dobson Ranch. The demand model
#' is separate: tickets on weekday and days remaining until the event. The
#' default holdout is the 2025 edition.
#'
#' @param tickets Optional series from [synthetic_tournament_tickets()].
#' @param holdout_start First date of the test edition.
#' @return A `golfops_forecast` list with `synthetic` TRUE.
#' @export
#' @examples
#' forecast_tournament_tickets()
forecast_tournament_tickets <- function(
    tickets = NULL,
    holdout_start = as.Date("2025-01-01")) {
  if (is.null(tickets)) {
    tickets <- synthetic_tournament_tickets()
  }
  compare_demand_forecasts(
    tickets,
    holdout_start = holdout_start,
    formula = outcome ~ weekday + days_to_event,
    by = c("weekday", "month")
  )
}

.drop_identical_dates <- function(rounds) {
  rounds <- rounds[order(rounds$row_id), , drop = FALSE]
  duplicate_dates <- unique(rounds$date[duplicated(rounds$date)])
  if (length(duplicate_dates) == 0) {
    return(rounds)
  }
  keys <- c(
    "rounds_played", "rounds_possible", "green_fee_revenue",
    "total_revenue", "flag_zero_rounds"
  )
  for (day in duplicate_dates) {
    block <- rounds[rounds$date == day, keys, drop = FALSE]
    if (nrow(unique(block)) > 1) {
      abort(
        paste0("Duplicate date ", day, " is not identical, so it was not dropped."),
        class = "golfops_duplicate_error"
      )
    }
  }
  kept <- rounds[!duplicated(rounds$date), , drop = FALSE]
  kept$flag_duplicate_date <- FALSE
  kept
}

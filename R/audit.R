#' Audit a prepared Dobson Ranch table
#'
#' Reports grain, coverage gaps, duplicate dates, and whether the published
#' revenue and booking-rate fields match the definitions in the city catalog.
#' The audit reads the prepared table only. It does not contact the open-data
#' portal.
#'
#' @param data A `golfops_rounds` table from [prepare_dobson_rounds()].
#' @return A `golfops_audit` list with a `checks` tibble. Each check has
#'   `status` `pass`, `watch`, or `info`.
#' @export
#' @examples
#' audit_golf_rounds(prepare_dobson_rounds())
audit_golf_rounds <- function(data) {
  data <- .require_rounds(data)
  threshold <- attr(data, "monthly_rounds_threshold") %||% 1000

  day <- data[data$grain == "day" & !is.na(data$grain), , drop = FALSE]
  month <- data[data$grain == "month" & !is.na(data$grain), , drop = FALSE]
  day_rounds <- day$rounds_played
  month_rounds <- month$rounds_played
  max_day <- if (length(day_rounds) == 0) NA_real_ else max(day_rounds, na.rm = TRUE)
  min_month <- if (length(month_rounds) == 0) NA_real_ else min(month_rounds, na.rm = TRUE)
  gap <- min_month - max_day

  dup_dates <- sort(unique(data$date[data$flag_duplicate_date]))
  coverage <- .coverage_report(day$date)

  complete <- !is.na(data$component_revenue) & !is.na(data$total_revenue)
  component_gap <- data$total_revenue[complete] - data$component_revenue[complete]
  max_component_gap <- if (length(component_gap) == 0) NA_real_ else max(abs(component_gap))

  yield_ok <- !is.na(data$revenue_per_round_source) &
    !is.na(data$total_revenue_per_played_round)
  yield_gap <- abs(
    data$revenue_per_round_source[yield_ok] -
      data$total_revenue_per_played_round[yield_ok]
  )
  max_yield_gap <- if (length(yield_gap) == 0) NA_real_ else max(yield_gap)

  rate_ok <- !is.na(data$booking_rate) & !is.na(data$utilization)
  rate_gap <- abs(data$booking_rate[rate_ok] - data$utilization[rate_ok])
  max_rate_gap <- if (length(rate_gap) == 0) NA_real_ else max(rate_gap)

  cart_blank <- is.na(data$cart_revenue)
  others_present <- !is.na(data$club_revenue) &
    !is.na(data$food_beverage_revenue) &
    !is.na(data$green_fee_revenue) &
    !is.na(data$merchandise_revenue) &
    !is.na(data$range_revenue) &
    !is.na(data$total_revenue)
  cart_blank_check <- cart_blank & others_present
  cart_blank_gap <- data$total_revenue[cart_blank_check] - (
    data$club_revenue[cart_blank_check] +
      data$food_beverage_revenue[cart_blank_check] +
      data$green_fee_revenue[cart_blank_check] +
      data$merchandise_revenue[cart_blank_check] +
      data$range_revenue[cart_blank_check]
  )
  cart_dates <- data$date[!is.na(data$cart_revenue)]
  first_cart <- if (length(cart_dates) == 0) NA else min(cart_dates)

  both_light <- !is.na(data$daylight_hours) & !is.na(data$rounds_possible)
  daylight_cor <- if (sum(both_light) >= 30) {
    stats::cor(data$daylight_hours[both_light], data$rounds_possible[both_light])
  } else {
    NA_real_
  }

  zero_days <- data$date[data$flag_zero_rounds]
  zero_months <- if (length(zero_days) == 0) {
    "none"
  } else {
    tab <- tabulate(month(zero_days), nbins = 12)
    kept <- tab > 0
    paste(paste0(month_names()[kept], " ", tab[kept]), collapse = "; ")
  }

  month_not_first <- sum(data$grain == "month" & data$day != 1, na.rm = TRUE)
  grain_status <- if (is.na(gap) || gap < 500) "watch" else "pass"

  checks <- tibble(
    check = c(
      "rows_and_dates",
      "grain_separation",
      "monthly_rows_dated_the_first",
      "daily_coverage",
      "duplicate_dates",
      "calendar_labels",
      "component_sum",
      "revenue_per_round_source",
      "booking_rate",
      "cart_revenue_coverage",
      "zero_round_rows",
      "above_daylight_estimate",
      "daylight_and_rounds_possible"
    ),
    status = c(
      "info",
      grain_status,
      if (month_not_first == 0) "pass" else "watch",
      if (length(coverage$internal_missing) == 0) "pass" else "watch",
      if (length(dup_dates) == 0) "pass" else "watch",
      if (!any(data$flag_calendar_mismatch)) "pass" else "watch",
      if (!is.na(max_component_gap) && max_component_gap <= 0.51) "pass" else "watch",
      if (!is.na(max_yield_gap) && max_yield_gap <= 0.02) "pass" else "watch",
      if (!is.na(max_rate_gap) && max_rate_gap <= 0.001) "pass" else "watch",
      "info",
      if (length(zero_days) == 0) "pass" else "watch",
      if (!any(data$flag_above_daylight_capacity)) "pass" else "watch",
      "info"
    ),
    detail = c(
      sprintf(
        "%s rows from %s to %s. Rounds played sum to %s.",
        format(nrow(data), big.mark = ","),
        min(data$date),
        max(data$date),
        format(sum(data$rounds_played, na.rm = TRUE), big.mark = ",")
      ),
      sprintf(
        "%s daily rows and %s monthly rows using a cutoff of %s played rounds. Largest daily total is %s and smallest monthly total is %s.",
        format(nrow(day), big.mark = ","),
        format(nrow(month), big.mark = ","),
        format(threshold, big.mark = ","),
        format(max_day, big.mark = ","),
        format(min_month, big.mark = ",")
      ),
      sprintf(
        "%s monthly rows are not dated on the 1st. Monthly totals in this extract are dated the first of the month.",
        month_not_first
      ),
      coverage$detail,
      if (length(dup_dates) == 0) {
        "No duplicate dates."
      } else {
        paste0(
          "Duplicate dates: ",
          paste(dup_dates, collapse = ", "),
          ". summarize_golf_rounds() will stop until duplicates are dropped or kept on purpose."
        )
      },
      sprintf(
        "%s rows disagree with the calendar implied by Date for month, weekday, day, season, or fiscal year. Fiscal year is July 1 through June 30. Season is meteorological.",
        sum(data$flag_calendar_mismatch)
      ),
      sprintf(
        "On %s rows with all six revenue categories, the largest absolute gap versus total revenue is %s. A gap of at most $0.50 is consistent with dollar rounding in the CSV.",
        sum(complete),
        if (is.na(max_component_gap)) "NA" else sprintf("$%.2f", max_component_gap)
      ),
      sprintf(
        "On %s rows, the city's Revenue/Round matches total revenue divided by rounds played within $0.02. Largest absolute gap is %s. The field is blank when rounds played are zero.",
        sum(yield_ok),
        if (is.na(max_yield_gap)) "NA" else sprintf("$%.4f", max_yield_gap)
      ),
      sprintf(
        "On %s rows, Round Booking Rate matches rounds played divided by rounds possible within 0.001 after converting the CSV percent to a proportion. Largest absolute gap is %s.",
        sum(rate_ok),
        if (is.na(max_rate_gap)) "NA" else sprintf("%.5f", max_rate_gap)
      ),
      sprintf(
        "Cart revenue is populated on %s rows. The earliest populated date is %s. On %s rows cart revenue is blank while the other five categories are present, and those five %s total revenue.",
        sum(!is.na(data$cart_revenue)),
        if (length(cart_dates) == 0) "NA" else as.character(first_cart),
        sum(cart_blank_check),
        if (length(cart_blank_gap) > 0 && max(abs(cart_blank_gap)) <= 0.51) {
          "already equal"
        } else {
          "do not equal"
        }
      ),
      sprintf(
        "%s rows have zero rounds played. Zero-round rows by month: %s. Revenue on those rows is left out of per-played-round yields because the denominator is zero.",
        length(zero_days),
        zero_months
      ),
      sprintf(
        "%s rows have more rounds played than rounds possible. Utilization can exceed 1. The daylight estimate is not a hard cap in the source data.",
        sum(data$flag_above_daylight_capacity)
      ),
      if (is.na(daylight_cor)) {
        "Not enough sunrise, sunset, and rounds-possible values to compare daylight hours with rounds possible."
      } else {
        sprintf(
          "Correlation between daylight hours and rounds possible is %.3f on %s rows. This is consistent with the catalog note that rounds possible comes from daylight, and it is not a recovered tee-sheet formula.",
          daylight_cor,
          sum(both_light)
        )
      }
    )
  )

  structure(list(checks = checks), class = "golfops_audit")
}

.coverage_report <- function(dates) {
  dates <- sort(unique(as.Date(dates)))
  if (length(dates) < 2) {
    return(list(
      internal_missing = as.Date(character()),
      detail = "Not enough daily dates to look for gaps."
    ))
  }
  gaps <- as.numeric(diff(dates))
  break_at <- which(gaps > 31)
  internal_at <- which(gaps > 1 & gaps <= 31)
  missing <- as.Date(character())
  if (length(internal_at) > 0) {
    missing <- unlist(lapply(internal_at, function(i) {
      seq(dates[i] + 1, dates[i + 1] - 1, by = "day")
    }))
    missing <- as.Date(missing, origin = "1970-01-01")
  }
  segments <- c(dates[c(1, break_at + 1)])
  segment_ends <- c(dates[c(break_at, length(dates))])
  segment_text <- paste(
    sprintf("%s to %s", segments, segment_ends),
    collapse = "; "
  )
  missing_text <- if (length(missing) == 0) {
    "No missing dates inside those segments."
  } else {
    paste0("Missing inside a segment: ", paste(missing, collapse = ", "), ".")
  }
  list(
    internal_missing = missing,
    detail = paste(segment_text, missing_text, sep = ". ")
  )
}

#' @export
print.golfops_audit <- function(x, ...) {
  cat("Dobson Ranch data audit\n\n")
  checks <- x$checks
  for (i in seq_len(nrow(checks))) {
    cat(sprintf("[%s] %s\n    %s\n", checks$status[i], checks$check[i], checks$detail[i]))
  }
  invisible(x)
}

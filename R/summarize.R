#' Summarize participation and revenue for one grain
#'
#' Period yields are ratios of sums. They are not averages of daily yields.
#' Green-fee and total revenue per played round exclude rows with zero played
#' rounds from both the numerator and the denominator. Revenue per daylight
#' round keeps those rows, because the city's rounds-possible value stays
#' positive when nobody played.
#'
#' @param data A `golfops_rounds` table.
#' @param by Grouping. `year_month` is a time series. `week` is the Monday of
#'   each week and is meant for charts that are hard to read one day at a
#'   time. `month_of_year`, `weekday`, and `season` pool every matching row.
#'   `year` and `fiscal_year` are annual totals. Fiscal year follows the City
#'   of Mesa calendar, July through June.
#' @param grain Which rows to keep. Monthly and daily rows are never added
#'   together.
#' @param duplicates What to do when a date appears more than once.
#'   `abort` stops. `drop_extra` keeps one copy when the copies are identical
#'   aside from `row_id`. `keep` retains every row and will double count.
#' @return A tibble of class `golfops_summary`.
#' @export
#' @examples
#' rounds <- prepare_dobson_rounds()
#' summarize_golf_rounds(rounds, by = "weekday", duplicates = "drop_extra")
summarize_golf_rounds <- function(
    data,
    by = c(
      "year_month", "week", "weekday", "season", "year",
      "fiscal_year", "month_of_year"
    ),
    grain = c("day", "month"),
    duplicates = c("abort", "drop_extra", "keep")) {
  data <- .require_rounds(data)
  by <- match.arg(by)
  grain <- match.arg(grain)
  duplicates <- match.arg(duplicates)

  if (grain == "month" && by %in% c("weekday", "week")) {
    abort(
      "Weekday and week summaries are not defined for monthly totals. Those rows are dated the first of the month.",
      class = "golfops_input_error"
    )
  }

  data <- data[data$grain == grain & !is.na(data$grain), , drop = FALSE]
  data <- .require_rounds(data)
  if (nrow(data) == 0) {
    abort(
      paste0("No rows with grain '", grain, "'."),
      class = "golfops_input_error"
    )
  }
  data <- .resolve_duplicates(data, duplicates)

  data$.period <- switch(
    by,
    year_month = floor_date(data$date, "month"),
    week = floor_date(data$date, "week", week_start = 1),
    weekday = data$weekday,
    season = data$season,
    year = data$calendar_year,
    fiscal_year = factor(data$fiscal_year, levels = unique(data$fiscal_year[order(data$date)])),
    month_of_year = data$month
  )

  data <- data |>
    mutate(
      green_on_played = if_else(.data$rounds_played > 0, .data$green_fee_revenue, NA_real_),
      total_on_played = if_else(.data$rounds_played > 0, .data$total_revenue, NA_real_),
      rounds_if_played = if_else(.data$rounds_played > 0, .data$rounds_played, NA_real_),
      open_i = pmax(.data$rounds_possible - .data$rounds_played, 0),
      above_i = pmax(.data$rounds_played - .data$rounds_possible, 0)
    )

  summary <- data |>
    group_by(.data$.period) |>
    summarise(
      n_rows = n(),
      rounds_played = sum_defined(.data$rounds_played),
      rounds_possible = sum_defined(.data$rounds_possible),
      open_daylight_rounds = sum_defined(.data$open_i),
      rounds_above_daylight = sum_defined(.data$above_i),
      green_fee_revenue = sum_defined(.data$green_fee_revenue),
      ancillary_revenue = sum_defined(.data$ancillary_revenue),
      total_revenue = sum_defined(.data$total_revenue),
      green_fee_revenue_on_played_rows = sum_defined(.data$green_on_played),
      total_revenue_on_played_rows = sum_defined(.data$total_on_played),
      rounds_on_played_rows = sum_defined(.data$rounds_if_played),
      n_zero_round_rows = sum(.data$flag_zero_rounds, na.rm = TRUE),
      n_above_capacity_rows = sum(.data$flag_above_daylight_capacity, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      period = .data$.period,
      rounds_per_row = .data$rounds_played / .data$n_rows,
      utilization = .data$rounds_played / .data$rounds_possible,
      green_fee_per_played_round = .data$green_fee_revenue_on_played_rows /
        .data$rounds_on_played_rows,
      total_revenue_per_played_round = .data$total_revenue_on_played_rows /
        .data$rounds_on_played_rows,
      green_fee_per_daylight_round = .data$green_fee_revenue / .data$rounds_possible,
      total_revenue_per_daylight_round = .data$total_revenue / .data$rounds_possible
    ) |>
    arrange(.data$period) |>
    select(-".period")

  summary <- as_tibble(summary)
  class(summary) <- unique(c("golfops_summary", class(summary)))
  attr(summary, "by") <- by
  attr(summary, "grain") <- grain
  attr(summary, "duplicates") <- duplicates
  summary
}

.resolve_duplicates <- function(data, duplicates) {
  if (!any(data$flag_duplicate_date)) {
    return(data)
  }
  dup_dates <- sort(unique(data$date[data$flag_duplicate_date]))
  if (duplicates == "abort") {
    abort(
      paste0(
        "Duplicate dates: ",
        paste(dup_dates, collapse = ", "),
        ". Use duplicates = 'drop_extra' when the copies match, or duplicates = 'keep' to retain them."
      ),
      class = "golfops_duplicate_error"
    )
  }
  if (duplicates == "keep") {
    warn(
      paste0(
        "Keeping duplicate dates. Totals will count these dates more than once: ",
        paste(dup_dates, collapse = ", ")
      ),
      class = "golfops_duplicate_warning"
    )
    return(data)
  }

  dup <- data[data$flag_duplicate_date, , drop = FALSE]
  keys <- setdiff(names(dup), "row_id")
  variant_counts <- tapply(seq_len(nrow(dup)), dup$date, function(idx) {
    nrow(unique(dup[idx, keys, drop = FALSE]))
  })
  if (any(variant_counts > 1)) {
    conflicted <- names(variant_counts)[variant_counts > 1]
    abort(
      paste0(
        "Duplicate dates are not identical, so drop_extra will not choose one: ",
        paste(conflicted, collapse = ", ")
      ),
      class = "golfops_duplicate_error"
    )
  }
  data <- data[order(data$row_id), , drop = FALSE]
  data <- data[!duplicated(data[, keys, drop = FALSE]), , drop = FALSE]
  data$flag_duplicate_date <- FALSE
  .require_rounds(data)
}

#' @export
print.golfops_summary <- function(x, ...) {
  cat(sprintf(
    "Golf operations summary by %s (%s grain, duplicates: %s)\n",
    attr(x, "by"),
    attr(x, "grain"),
    attr(x, "duplicates")
  ))
  print(tibble::as_tibble(x))
  invisible(x)
}

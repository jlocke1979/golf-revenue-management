#' Review the largest Dobson participation and range-revenue days
#'
#' The review measures two patterns in the operating sample. It does not assign
#' a cause. A busy day is not labeled a tournament, a holiday, or a broadcast
#' weekend unless an external event table says so.
#'
#' @param rounds Optional result of [prepare_dobson_rounds()].
#' @param events Optional [course_events()] table. Listed names are reported.
#'   They are not used to explain a day when the table is empty.
#' @return A tibble with the measured pattern and what the extract does not
#'   establish.
#' @export
review_dobson_anomalies <- function(rounds = NULL, events = NULL) {
  sample <- dobson_operating_daily(rounds)
  spike_at <- which.max(sample$rounds_played)
  spike <- sample[spike_at, , drop = FALSE]
  same_month <- sample$calendar_year == spike$calendar_year & sample$month == spike$month
  month_median <- stats::median(sample$rounds_played[same_month])
  month_end <- .is_month_end(sample$date)
  ranked <- sample[order(sample$range_revenue, decreasing = TRUE), , drop = FALSE]
  top <- ranked[seq_len(min(12, nrow(ranked))), , drop = FALSE]
  top_are_month_end <- .is_month_end(top$date)
  from_end <- .days_from_month_end(sample$date)
  range_by_distance <- tapply(sample$range_revenue, from_end, stats::median, na.rm = TRUE)
  event_note <- .events_on(events, spike$date)
  tibble(
    topic = c("participation_spike", "range_revenue_extremes"),
    observation = c(
      sprintf(
        "The busiest day in the operating sample is %s, a %s, with %s rounds. Daylight-estimated capacity that day is %s, so utilization is %s and is not capped. The median rounds in %s %s is %s. Green-fee revenue is $%s and range revenue is $%s.",
        spike$date,
        as.character(spike$weekday),
        format(spike$rounds_played, big.mark = ","),
        format(spike$rounds_possible, big.mark = ","),
        format(round(spike$capacity_utilization, 3), nsmall = 3),
        as.character(spike$month),
        spike$calendar_year,
        format(round(month_median, 1), nsmall = 1),
        format(round(spike$green_fee_revenue, 2), big.mark = ",", nsmall = 2),
        format(round(spike$range_revenue, 2), big.mark = ",", nsmall = 2)
      ),
      sprintf(
        "%s of the 12 highest range-revenue days fall on the last calendar day of a month. Rounds on those days run from %s to %s. The median range revenue on a last day of the month is $%s, compared with $%s on earlier days of the month.",
        sum(top_are_month_end),
        format(min(top$rounds_played[top_are_month_end]), big.mark = ","),
        format(max(top$rounds_played[top_are_month_end]), big.mark = ","),
        format(round(range_by_distance[["0"]], 0), big.mark = ","),
        format(round(stats::median(range_by_distance[names(range_by_distance) != "0"]), 0), big.mark = ",")
      )
    ),
    not_established = c(
      paste(
        "The extract does not name an event, a price change, or a weather condition for this day.",
        event_note
      ),
      "The extract does not say whether month-end range dollars were earned that day or posted that day. Service date versus posting date remains unknown. This is not identified as a tournament."
    )
  )
}

.is_month_end <- function(date) {
  as.integer(format(date, "%d")) == .days_in_month(date)
}

.days_in_month <- function(date) {
  as.integer(format(lubridate::ceiling_date(date, "month") - 1, "%d"))
}

.days_from_month_end <- function(date) {
  .days_in_month(date) - as.integer(format(date, "%d"))
}

.events_on <- function(events, date) {
  if (is.null(events)) {
    return("No event table was supplied.")
  }
  if (!inherits(events, "golfops_course_events")) {
    abort("Expected events from course_events().", class = "golfops_input_error")
  }
  names_on_day <- events$event_name[events$date %in% date]
  if (length(names_on_day) == 0) {
    "The supplied event table does not list this date."
  } else {
    paste0("The supplied event table lists: ", paste(names_on_day, collapse = "; "), ".")
  }
}

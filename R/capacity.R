#' Optional operating calendar for one course
#'
#' Supplies confirmed open or closed status and, when the course actually has
#' them, the operating window, tee interval, players per start, blocked
#' rounds, or a count of available rounds. No interval, opening hour, or
#' blocked-round value is filled in. A 15-minute tee interval is not assumed.
#'
#' `available_rounds` is sellable player-rounds already counted by the source.
#' It is not a tee-time count unless the source has already multiplied by
#' players per start.
#'
#' @param date Dates the calendar covers.
#' @param course_id Course identifier. Recycled when length 1.
#' @param status `open`, `closed`, or `unknown`.
#' @param window_start,window_end Clock times as `"HH:MM"` or `"HH:MM:SS"`.
#'   Both are required before a schedule estimate is calculated. An end at or
#'   before the start does not roll into the next day.
#' @param tee_interval_minutes Minutes between starts. Required for a schedule
#'   estimate. There is no default.
#' @param players_per_start Players on one start. Required for a schedule
#'   estimate. There is no default.
#' @param blocked_rounds Player-rounds held off the sellable inventory. A
#'   schedule estimate is not produced while this is missing, including when
#'   the caller might have meant zero.
#' @param available_rounds Observed or published sellable rounds. These take
#'   precedence over a schedule estimate and over the daylight proxy.
#' @return A `golfops_operating_calendar` tibble.
#' @export
operating_calendar <- function(
    date,
    course_id = NA_character_,
    status = "unknown",
    window_start = NA_character_,
    window_end = NA_character_,
    tee_interval_minutes = NA_real_,
    players_per_start = NA_real_,
    blocked_rounds = NA_real_,
    available_rounds = NA_real_) {
  date <- as.Date(date)
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  n <- length(date)
  course_id <- .recycle_atomic(course_id, n, "course_id")
  status <- .recycle_atomic(status, n, "status")
  window_start <- .recycle_atomic(window_start, n, "window_start")
  window_end <- .recycle_atomic(window_end, n, "window_end")
  tee_interval_minutes <- .recycle_atomic(tee_interval_minutes, n, "tee_interval_minutes")
  players_per_start <- .recycle_atomic(players_per_start, n, "players_per_start")
  blocked_rounds <- .recycle_atomic(blocked_rounds, n, "blocked_rounds")
  available_rounds <- .recycle_atomic(available_rounds, n, "available_rounds")
  if (!all(status %in% c("unknown", "open", "closed"))) {
    abort(
      '`status` must be "unknown", "open", or "closed".',
      class = "golfops_input_error"
    )
  }
  if (any(!is.na(tee_interval_minutes) & tee_interval_minutes <= 0)) {
    abort("`tee_interval_minutes` must be positive when supplied.", class = "golfops_input_error")
  }
  if (any(!is.na(players_per_start) & players_per_start <= 0)) {
    abort("`players_per_start` must be positive when supplied.", class = "golfops_input_error")
  }
  if (any(duplicated(paste(course_id, date)))) {
    abort("The operating calendar has a repeated course and date.", class = "golfops_input_error")
  }
  out <- tibble(
    course_id = as.character(course_id),
    date = date,
    status = status,
    window_start = as.character(window_start),
    window_end = as.character(window_end),
    tee_interval_minutes = as.numeric(tee_interval_minutes),
    players_per_start = as.numeric(players_per_start),
    blocked_rounds = as.numeric(blocked_rounds),
    available_rounds = as.numeric(available_rounds)
  )
  class(out) <- unique(c("golfops_operating_calendar", class(out)))
  out
}

#' Resolve capacity and operating status for prepared daily rows
#'
#' Precedence is published available rounds, then a schedule estimate from a
#' complete operating window, then the city's daylight estimate. The schedule
#' estimate is `floor(window minutes / tee interval) * players per start -
#' blocked rounds`. Any missing piece, including a missing blocked-round
#' count, skips that estimate. The daylight field is then the fallback and is
#' labeled `daylight_estimate`.
#'
#' Zero-round days stay `zero_rounds_unknown` unless the calendar says `open`
#' or `closed`. Unknown zero-round days do not contribute unused sellable
#' rounds. Confirmed closures do not either. Utilization is rounds divided by
#' the resolved capacity and is not capped at 1.
#'
#' @param data A `golfops_rounds` table.
#' @param calendar Optional result of [operating_calendar()].
#' @return The same rows with `operating_status`, `capacity_rounds`,
#'   `capacity_basis`, `unused_sellable_rounds`, and `capacity_utilization`.
#' @export
apply_operating_capacity <- function(data, calendar = NULL) {
  data <- .require_rounds(data)
  if (any(duplicated(data$date))) {
    abort(
      "Capacity is resolved after duplicate dates are removed.",
      class = "golfops_duplicate_error"
    )
  }
  data <- data[, setdiff(names(data), c(
    "calendar_status", "available_rounds", "window_start", "window_end",
    "tee_interval_minutes", "players_per_start", "blocked_rounds"
  )), drop = FALSE]
  if (!is.null(calendar)) {
    course_ids <- unique(calendar$course_id[!is.na(calendar$course_id)])
    if (length(course_ids) > 1) {
      abort(
        "Filter the operating calendar to one course before applying it.",
        class = "golfops_input_error"
      )
    }
    if (!inherits(calendar, "golfops_operating_calendar")) {
      abort(
        "Expected a calendar from operating_calendar().",
        class = "golfops_input_error"
      )
    }
    extra <- setdiff(names(calendar), "course_id")
    keep <- calendar[, extra, drop = FALSE]
    names(keep)[names(keep) == "status"] <- "calendar_status"
    data <- merge(data, keep, by = "date", all.x = TRUE, sort = FALSE)
    data <- as_tibble(data)
  } else {
    data$calendar_status <- NA_character_
    data$available_rounds <- NA_real_
    data$window_start <- NA_character_
    data$window_end <- NA_character_
    data$tee_interval_minutes <- NA_real_
    data$players_per_start <- NA_real_
    data$blocked_rounds <- NA_real_
  }

  schedule <- .schedule_rounds(data)
  basis <- rep("unavailable", nrow(data))
  capacity <- rep(NA_real_, nrow(data))
  use_available <- !is.na(data$available_rounds)
  use_schedule <- !use_available & !is.na(schedule)
  use_daylight <- !use_available & !use_schedule &
    !is.na(data$rounds_possible) & data$rounds_possible > 0
  capacity[use_available] <- data$available_rounds[use_available]
  basis[use_available] <- "available_rounds"
  capacity[use_schedule] <- schedule[use_schedule]
  basis[use_schedule] <- "schedule_estimate"
  capacity[use_daylight] <- data$rounds_possible[use_daylight]
  basis[use_daylight] <- "daylight_estimate"

  played <- !is.na(data$rounds_played) & data$rounds_played > 0
  zero <- !is.na(data$rounds_played) & data$rounds_played == 0
  closed <- data$calendar_status %in% "closed"
  open <- data$calendar_status %in% "open"
  status <- rep("played", nrow(data))
  status[zero] <- "zero_rounds_unknown"
  status[zero & open] <- "zero_on_confirmed_open_day"
  status[closed & zero] <- "confirmed_closure"
  status[closed & played] <- "conflict_play_on_closed_day"
  status[is.na(data$rounds_played)] <- "rounds_missing"
  status[played & !closed & !is.na(capacity) & data$rounds_played > capacity] <- "above_capacity"
  status[status == "above_capacity" & basis == "daylight_estimate"] <- "above_daylight_estimate"

  unused <- rep(NA_real_, nrow(data))
  sellable <- status %in% c("played", "above_capacity", "above_daylight_estimate", "zero_on_confirmed_open_day")
  usable <- sellable & !is.na(capacity)
  unused[usable] <- pmax(capacity[usable] - data$rounds_played[usable], 0)

  data$operating_status <- status
  data$capacity_rounds <- capacity
  data$capacity_basis <- basis
  data$unused_sellable_rounds <- unused
  data$capacity_utilization <- ifelse(
    !is.na(capacity) & capacity > 0,
    data$rounds_played / capacity,
    NA_real_
  )
  .new_rounds(
    data,
    attr(data, "monthly_rounds_threshold") %||% 1000,
    attr(data, "source_path") %||% NA_character_
  )
}

#' Calendar of daily utilization, including dates the extract skipped
#'
#' Builds every date from the first through the last row. A date inside that
#' span with no row is `missing`. It is not given zero rounds. Zero-round days
#' already in the extract keep their operating status. Utilization above 1 is
#' retained.
#'
#' @param data Daily `golfops_rounds` rows, usually [dobson_operating_daily()].
#' @param calendar Optional [operating_calendar()].
#' @return A tibble with one row per calendar date in the sample span.
#' @export
utilization_calendar <- function(data, calendar = NULL) {
  resolved <- apply_operating_capacity(data, calendar)
  start <- min(resolved$date)
  end <- max(resolved$date)
  all_dates <- seq(start, end, by = "day")
  observed <- tibble(
    date = resolved$date,
    rounds_played = resolved$rounds_played,
    capacity_rounds = resolved$capacity_rounds,
    capacity_basis = resolved$capacity_basis,
    utilization = resolved$capacity_utilization,
    operating_status = resolved$operating_status,
    unused_sellable_rounds = resolved$unused_sellable_rounds
  )
  missing_dates <- all_dates[!all_dates %in% observed$date]
  missing <- tibble(
    date = missing_dates,
    rounds_played = NA_real_,
    capacity_rounds = NA_real_,
    capacity_basis = "unavailable",
    utilization = NA_real_,
    operating_status = "missing",
    unused_sellable_rounds = NA_real_
  )
  out <- rbind(observed, missing)
  out <- out[order(out$date), , drop = FALSE]
  out$calendar_year <- year(out$date)
  out$month <- factor(month_names()[month(out$date)], levels = rev(month_names()))
  out$day <- as.integer(format(out$date, "%d"))
  out$weekday <- factor(expected_weekday(out$date), levels = weekday_names())
  out$on_utilization_scale <- out$operating_status %in% c(
    "played", "above_capacity", "above_daylight_estimate", "zero_on_confirmed_open_day"
  )
  out$scale_utilization <- ifelse(out$on_utilization_scale, out$utilization, NA_real_)
  as_tibble(out)
}

.schedule_rounds <- function(data) {
  start <- .clock_minutes(data$window_start)
  end <- .clock_minutes(data$window_end)
  minutes <- end - start
  minutes[!is.na(minutes) & minutes <= 0] <- NA_real_
  complete <- !is.na(minutes) &
    !is.na(data$tee_interval_minutes) &
    !is.na(data$players_per_start) &
    !is.na(data$blocked_rounds)
  out <- rep(NA_real_, nrow(data))
  out[complete] <- floor(minutes[complete] / data$tee_interval_minutes[complete]) *
    data$players_per_start[complete] -
    data$blocked_rounds[complete]
  if (any(!is.na(out) & out < 0)) {
    abort(
      "The schedule estimate is negative. Check the blocked-round count against the operating window.",
      class = "golfops_input_error"
    )
  }
  out
}

.clock_minutes <- function(clock) {
  out <- rep(NA_real_, length(clock))
  present <- !is.na(clock) & nzchar(clock)
  if (!any(present)) {
    return(out)
  }
  ok <- grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", clock[present])
  if (!all(ok)) {
    abort(
      'Clock times must look like "07:00" or "07:00:00".',
      class = "golfops_input_error"
    )
  }
  parts <- strsplit(clock[present], ":", fixed = TRUE)
  out[present] <- vapply(parts, function(piece) {
    piece <- as.numeric(piece)
    piece[1] * 60 + piece[2] + if (length(piece) == 3) piece[3] / 60 else 0
  }, numeric(1))
  out
}

.recycle_atomic <- function(x, n, name) {
  if (length(x) == 1) {
    return(rep(x, n))
  }
  if (length(x) != n) {
    abort(
      sprintf("`%s` must have length 1 or %s.", name, n),
      class = "golfops_input_error"
    )
  }
  x
}

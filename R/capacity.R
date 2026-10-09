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

#' Daily tee times and player slots from an operating window
#'
#' Counts starts inside the open window. The end clock closes the window. It
#' is not an extra start, and a window that ends at or before it starts does
#' not roll into the next day. That day has no starts. There is no default
#' interval and no daylight, weather, or closure adjustment.
#'
#' Player slots are tee times multiplied by players per start. With blocked
#' rounds of zero, that product is the schedule estimate used by
#' [apply_operating_capacity()].
#'
#' @param date Dates to schedule.
#' @param window_start,window_end Clock times as `"HH:MM"` or `"HH:MM:SS"`.
#' @param tee_interval_minutes Minutes between starts. Required. There is no
#'   default.
#' @param players_per_start Players on one start. Required. There is no
#'   default.
#' @return A tibble with one row per date, including `tee_times` and
#'   `player_slots`.
#' @export
course_capacity <- function(
    date,
    window_start,
    window_end,
    tee_interval_minutes,
    players_per_start) {
  date <- as.Date(date)
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  n <- length(date)
  window_start <- .recycle_atomic(window_start, n, "window_start")
  window_end <- .recycle_atomic(window_end, n, "window_end")
  tee_interval_minutes <- .recycle_atomic(
    tee_interval_minutes, n, "tee_interval_minutes"
  )
  players_per_start <- .recycle_atomic(players_per_start, n, "players_per_start")
  if (any(is.na(window_start) | !nzchar(window_start) | is.na(window_end) | !nzchar(window_end))) {
    abort("`window_start` and `window_end` are required.", class = "golfops_input_error")
  }
  if (any(is.na(tee_interval_minutes) | tee_interval_minutes <= 0)) {
    abort("`tee_interval_minutes` must be positive.", class = "golfops_input_error")
  }
  if (any(is.na(players_per_start) | players_per_start <= 0)) {
    abort("`players_per_start` must be positive.", class = "golfops_input_error")
  }
  minutes <- .clock_minutes(window_end) - .clock_minutes(window_start)
  minutes[minutes < 0] <- 0
  tee_times <- floor(minutes / tee_interval_minutes)
  tibble(
    date = date,
    window_start = as.character(window_start),
    window_end = as.character(window_end),
    tee_interval_minutes = as.numeric(tee_interval_minutes),
    players_per_start = as.numeric(players_per_start),
    tee_times = tee_times,
    player_slots = tee_times * as.numeric(players_per_start)
  )
}

#' Daily tee-sheet use against a course capacity schedule
#'
#' Compares groups on a tee sheet with the daily supply from
#' [course_capacity()]. One row is one group at one start. Groups that share
#' a start count as one occupied tee time, and their players add together.
#' A single player on a foursome start occupies the tee time and one player
#' slot.
#'
#' `tee_time_occupancy` is occupied tee times divided by scheduled tee times.
#' `slot_utilization` is players divided by player slots. Both are shares.
#' Neither is capped at 1. A day with no scheduled starts has no share,
#' because the denominator is zero. A day in the schedule with no groups is
#' zero use. A group on a date that is not in the schedule is an error.
#'
#' This is not the Dobson daylight comparison. That file has no tee time and
#' no players per group. Daily rounds stay on [utilization_calendar()].
#'
#' @param bookings Tee-sheet rows with `date`, `tee_time` (`"HH:MM"` or
#'   `"HH:MM:SS"`), and `players`.
#' @param capacity A result from [course_capacity()], one row per date.
#' @return A tibble with one row per scheduled date, including
#'   `tee_time_occupancy` and `slot_utilization`.
#' @export
daily_utilization <- function(bookings, capacity) {
  bookings <- .tee_sheet_bookings(bookings)
  capacity <- .capacity_schedule(capacity)
  if (any(duplicated(capacity$date))) {
    abort(
      "course_capacity() dates must be unique before utilization is calculated.",
      class = "golfops_input_error"
    )
  }
  extra <- setdiff(unique(bookings$date), capacity$date)
  if (length(extra) > 0) {
    abort(
      "Bookings include dates that are not in course_capacity().",
      class = "golfops_input_error"
    )
  }
  summary <- .tee_sheet_daily(bookings)
  out <- merge(
    capacity[, c("date", "tee_times", "player_slots")],
    summary,
    by = "date",
    all.x = TRUE,
    sort = FALSE
  )
  out$occupied_tee_times[is.na(out$occupied_tee_times)] <- 0
  out$players[is.na(out$players)] <- 0
  out$tee_time_occupancy <- ifelse(
    out$tee_times > 0,
    out$occupied_tee_times / out$tee_times,
    NA_real_
  )
  out$slot_utilization <- ifelse(
    out$player_slots > 0,
    out$players / out$player_slots,
    NA_real_
  )
  out <- out[order(out$date), c(
    "date", "tee_times", "player_slots", "occupied_tee_times", "players",
    "tee_time_occupancy", "slot_utilization"
  ), drop = FALSE]
  as_tibble(out)
}

.tee_sheet_bookings <- function(bookings) {
  if (!is.data.frame(bookings)) {
    abort("`bookings` must be a table.", class = "golfops_input_error")
  }
  required <- c("date", "tee_time", "players")
  missing <- setdiff(required, names(bookings))
  if (length(missing) > 0) {
    abort(
      paste0("Tee-sheet bookings need ", paste(missing, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  date <- as.Date(bookings$date)
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  players <- bookings$players
  if (!is.numeric(players) || any(is.na(players) | players < 0)) {
    abort("`players` must be zero or positive.", class = "golfops_input_error")
  }
  tee_time <- as.character(bookings$tee_time)
  if (any(is.na(tee_time) | !nzchar(tee_time))) {
    abort("`tee_time` is required.", class = "golfops_input_error")
  }
  tibble(
    date = date,
    tee_minutes = .clock_minutes(tee_time),
    players = as.numeric(players)
  )
}

.capacity_schedule <- function(capacity) {
  if (!is.data.frame(capacity)) {
    abort("`capacity` must be a table from course_capacity().", class = "golfops_input_error")
  }
  required <- c("date", "tee_times", "player_slots")
  missing <- setdiff(required, names(capacity))
  if (length(missing) > 0) {
    abort(
      paste0("Capacity needs ", paste(missing, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  date <- as.Date(capacity$date)
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  tee_times <- capacity$tee_times
  player_slots <- capacity$player_slots
  if (!is.numeric(tee_times) || any(is.na(tee_times) | tee_times < 0)) {
    abort("`tee_times` must be zero or positive.", class = "golfops_input_error")
  }
  if (!is.numeric(player_slots) || any(is.na(player_slots) | player_slots < 0)) {
    abort("`player_slots` must be zero or positive.", class = "golfops_input_error")
  }
  tibble(
    date = date,
    tee_times = as.numeric(tee_times),
    player_slots = as.numeric(player_slots)
  )
}

.tee_sheet_daily <- function(bookings) {
  empty <- tibble(
    date = as.Date(character()),
    occupied_tee_times = numeric(),
    players = numeric()
  )
  if (nrow(bookings) == 0) {
    return(empty)
  }
  starts <- aggregate(
    players ~ date + tee_minutes,
    data = bookings,
    FUN = sum
  )
  starts <- starts[starts$players > 0, , drop = FALSE]
  players <- aggregate(players ~ date, data = bookings, FUN = sum)
  if (nrow(starts) == 0) {
    players$occupied_tee_times <- 0
    return(as_tibble(players[, c("date", "occupied_tee_times", "players")]))
  }
  occupied <- aggregate(tee_minutes ~ date, data = starts, FUN = length)
  names(occupied)[names(occupied) == "tee_minutes"] <- "occupied_tee_times"
  out <- merge(players, occupied, by = "date", all.x = TRUE, sort = FALSE)
  out$occupied_tee_times[is.na(out$occupied_tee_times)] <- 0
  as_tibble(out[, c("date", "occupied_tee_times", "players")])
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

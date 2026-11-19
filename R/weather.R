#' Historical weather for the Dobson Ranch operating window
#'
#' Reads the bundled NOAA GHCND extract. Temperature and wind are Phoenix Sky
#' Harbor (`USW00023183`), about 13.91 km from the course centroid.
#' Precipitation is the Mesa 4.3 SW CoCoRaHS gauge (`US1AZMR0260`), about
#' 0.81 km away. That gauge does not report temperature or wind. Sky Harbor
#' precipitation is kept in `precip_mm_airport` and is not copied into the
#' local precipitation column.
#'
#' GHCND stores temperature and precipitation and wind in tenths. This file
#' stores degrees Celsius, millimeters, and meters per second. `temp_avg_c`
#' is the average of the daily minimum and maximum. GHCND `TAVG` is empty for
#' Sky Harbor on every day in this window, so the average is not an hourly
#' mean.
#'
#' A non-blank GHCND quality flag withdraws that value. The flag is kept.
#' A measurement flag of `T` is a trace: precipitation is 0 and
#' `precip_trace` is TRUE. Multiday accumulations are not assigned to one
#' date. Missing gauge days stay missing.
#'
#' The extract was retrieved from NCEI on 2026-11-19 and restricted to
#' 2021-07-01 through 2026-04-30, including 2024-12-10. Examples, tests, and
#' the vignette read this file. They do not call a weather service. GHCND is
#' a U.S. government work.
#'
#' The course coordinate is the OpenStreetMap centroid of relation 326342,
#' retrieved the same day. It is not a surveyed tee.
#'
#' @param path Optional path to a weather CSV built by
#'   `data-raw/dobson_weather.R`. The station CSV must sit beside it.
#' @return A `golfops_course_weather` tibble. `timing` is `observed`.
#'   Attributes `stations`, `coverage`, `units`, and `decisions` record
#'   provenance and missingness.
#' @export
#' @examples
#' weather <- prepare_dobson_weather()
#' range(weather$date)
#' attr(weather, "coverage")
prepare_dobson_weather <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file(
      "extdata",
      "dobson_weather.csv",
      package = "golfops",
      mustWork = TRUE
    )
  }
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    abort("`path` must be one existing weather CSV.", class = "golfops_input_error")
  }
  meta_path <- file.path(dirname(path), "dobson_weather_stations.csv")
  if (!file.exists(meta_path)) {
    abort(
      "dobson_weather_stations.csv must sit beside the weather CSV.",
      class = "golfops_input_error"
    )
  }
  raw <- .read_character_csv(path)
  required <- c(
    "date", "temp_min_c", "temp_max_c", "temp_avg_c",
    "temp_min_quality_flag", "temp_max_quality_flag",
    "precip_mm", "precip_trace", "precip_multiday", "precip_quality_flag",
    "precip_mm_airport", "precip_trace_airport", "precip_airport_quality_flag",
    "wind_speed_ms", "wind_quality_flag",
    "temp_missing", "precip_missing", "precip_airport_missing", "wind_missing"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    abort(
      paste0("Weather file is missing ", paste(missing, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  date <- as.Date(raw$date)
  if (any(is.na(date)) || anyDuplicated(date)) {
    abort("Weather dates must be complete and unique.", class = "golfops_input_error")
  }
  daily <- tibble(
    date = date,
    temp_min_c = .parse_number(raw$temp_min_c, "temp_min_c"),
    temp_max_c = .parse_number(raw$temp_max_c, "temp_max_c"),
    temp_avg_c = .parse_number(raw$temp_avg_c, "temp_avg_c"),
    temp_min_quality_flag = .empty_flag(raw$temp_min_quality_flag),
    temp_max_quality_flag = .empty_flag(raw$temp_max_quality_flag),
    precip_mm = .parse_number(raw$precip_mm, "precip_mm"),
    precip_trace = .parse_logical(raw$precip_trace, "precip_trace"),
    precip_multiday = .parse_logical(raw$precip_multiday, "precip_multiday"),
    precip_quality_flag = .empty_flag(raw$precip_quality_flag),
    precip_mm_airport = .parse_number(raw$precip_mm_airport, "precip_mm_airport"),
    precip_trace_airport = .parse_logical(raw$precip_trace_airport, "precip_trace_airport"),
    precip_airport_quality_flag = .empty_flag(raw$precip_airport_quality_flag),
    wind_speed_ms = .parse_number(raw$wind_speed_ms, "wind_speed_ms"),
    wind_quality_flag = .empty_flag(raw$wind_quality_flag),
    temp_missing = .parse_logical(raw$temp_missing, "temp_missing"),
    precip_missing = .parse_logical(raw$precip_missing, "precip_missing"),
    precip_airport_missing = .parse_logical(
      raw$precip_airport_missing, "precip_airport_missing"
    ),
    wind_missing = .parse_logical(raw$wind_missing, "wind_missing")
  )
  .reject_weather_units(daily)
  if (any(abs(daily$temp_avg_c - (daily$temp_min_c + daily$temp_max_c) / 2) > 0.011, na.rm = TRUE)) {
    abort(
      "temp_avg_c must be the average of temp_min_c and temp_max_c.",
      class = "golfops_input_error"
    )
  }
  if (any(daily$temp_missing != (is.na(daily$temp_min_c) | is.na(daily$temp_max_c))) ||
      any(daily$precip_missing != is.na(daily$precip_mm)) ||
      any(daily$precip_airport_missing != is.na(daily$precip_mm_airport)) ||
      any(daily$wind_missing != is.na(daily$wind_speed_ms))) {
    abort(
      "A missingness flag does not match its weather value.",
      class = "golfops_input_error"
    )
  }
  if (any(daily$precip_trace & (daily$precip_missing | daily$precip_mm != 0)) ||
      any(daily$precip_trace_airport & (daily$precip_airport_missing | daily$precip_mm_airport != 0))) {
    abort(
      "A trace must be a present precipitation of zero.",
      class = "golfops_input_error"
    )
  }

  stations <- .read_weather_stations(meta_path)
  weather <- course_weather(
    course_id = "dobson_ranch",
    date = daily$date,
    timing = "observed",
    source = "NOAA GHCND",
    temp_min_c = daily$temp_min_c,
    temp_max_c = daily$temp_max_c,
    temp_avg_c = daily$temp_avg_c,
    temp_min_quality_flag = daily$temp_min_quality_flag,
    temp_max_quality_flag = daily$temp_max_quality_flag,
    precip_mm = daily$precip_mm,
    precip_trace = daily$precip_trace,
    precip_multiday = daily$precip_multiday,
    precip_quality_flag = daily$precip_quality_flag,
    precip_mm_airport = daily$precip_mm_airport,
    precip_trace_airport = daily$precip_trace_airport,
    precip_airport_quality_flag = daily$precip_airport_quality_flag,
    wind_speed_ms = daily$wind_speed_ms,
    wind_quality_flag = daily$wind_quality_flag,
    temp_missing = daily$temp_missing,
    precip_missing = daily$precip_missing,
    precip_airport_missing = daily$precip_airport_missing,
    wind_missing = daily$wind_missing
  )
  weather <- weather[order(weather$date), , drop = FALSE]
  class(weather) <- unique(c("golfops_course_weather", class(weather)))
  coverage <- tibble(
    variable = c(
      "temp_min_c", "temp_max_c", "temp_avg_c",
      "precip_mm", "precip_mm_airport", "wind_speed_ms"
    ),
    n_days = nrow(weather),
    n_missing = c(
      sum(is.na(weather$temp_min_c)),
      sum(is.na(weather$temp_max_c)),
      sum(is.na(weather$temp_avg_c)),
      sum(weather$precip_missing),
      sum(weather$precip_airport_missing),
      sum(weather$wind_missing)
    )
  )
  coverage$pct_missing <- round(100 * coverage$n_missing / coverage$n_days, 2)
  local_km <- stations$distance_km[stations$role == "precipitation"]
  airport_km <- stations$distance_km[stations$role == "temperature"]
  recent <- weather$date >= as.Date("2026-01-01")
  recent_note <- if (any(recent)) {
    sprintf(
      "Local precipitation is missing on %.1f%% of the days in 2026 through the end of this file.",
      100 * mean(weather$precip_missing[recent])
    )
  } else {
    "This file has no 2026 dates."
  }
  decisions <- c(
    "Temperature and wind are Phoenix Sky Harbor, GHCND USW00023183.",
    sprintf(
      "The airport station is %.2f km from the OpenStreetMap centroid of Dobson Ranch.",
      airport_km
    ),
    "Local precipitation is MESA 4.3 SW, GHCND US1AZMR0260, a CoCoRaHS gauge.",
    sprintf(
      "The local gauge is %.2f km from the course centroid. It does not report temperature or wind.",
      local_km
    ),
    "Airport precipitation is precip_mm_airport. It is not copied into precip_mm.",
    "Monsoon rain is convective, so the airport gauge can disagree with the course on a single day.",
    "temp_avg_c is the average of the daily minimum and maximum. GHCND TAVG is empty for this station and window.",
    "Units are degrees Celsius, millimeters, and meters per second, converted from GHCND tenths.",
    "A non-blank GHCND quality flag withdraws the value. A measurement flag of T is a trace stored as 0.",
    "Multiday precipitation totals are not spread across the days they cover.",
    "The NCEI access files were retrieved on 2026-11-19. This function does not download them.",
    "The bundled date span is 2021-07-01 through 2026-04-30 and includes 2024-12-10.",
    recent_note
  )
  attr(weather, "stations") <- stations
  attr(weather, "coverage") <- coverage
  attr(weather, "units") <- c(
    temp_min_c = "degrees Celsius",
    temp_max_c = "degrees Celsius",
    temp_avg_c = "degrees Celsius; average of the daily minimum and maximum",
    precip_mm = "millimeters at MESA 4.3 SW",
    precip_mm_airport = "millimeters at Phoenix Sky Harbor",
    wind_speed_ms = "meters per second at Phoenix Sky Harbor"
  )
  attr(weather, "decisions") <- decisions
  attr(weather, "source_path") <- path
  attr(weather, "retrieved") <- "2026-11-19"
  weather
}

#' Playable capacity from stated time, closures, or an optional weather scenario
#'
#' `theoretical_capacity` is the capacity column you pass in. This function
#' does not change [course_capacity()] or `rounds_possible`.
#'
#' The primary adjustments are a confirmed closure, a stated unavailable
#' fraction, or stated unavailable hours. Hours reduce theoretical capacity
#' in proportion to daylight hours. Those hours come from the table. They are
#' not inferred from a daily weather total.
#'
#' A precipitation or wind threshold is a separate scenario. On a day that
#' meets it, playable capacity is set to zero for the whole day and the
#' source is `weather_assumption`. Lost hours stay missing. Pass that
#' scenario in its own call, not together with stated hours or a stated
#' fraction. High temperature is not an input and does not reduce capacity.
#'
#' A calendar status of `closed` sets playable capacity to zero and is
#' recorded as `observed_closure`. That status wins over a weather scenario
#' on the same date.
#'
#' @param data A table with `date` and the capacity column.
#' @param capacity Column name of theoretical capacity, in rounds.
#' @param calendar Optional [operating_calendar()].
#' @param unavailable_hours,daylight_hours Column names. Both are required
#'   together. Hours cannot exceed daylight hours.
#' @param unavailable_fraction Column name of a stated fraction from 0 to 1.
#' @param weather Optional [course_weather()] or [prepare_dobson_weather()]
#'   table. Used only when a precipitation or wind threshold is set.
#' @param precip_unplayable_mm Positive precipitation threshold, in
#'   millimeters. NULL skips the scenario.
#' @param precip_column Precipitation column on `weather`. The name must end
#'   in `_mm`.
#' @param wind_unplayable_ms Positive wind threshold, in meters per second.
#'   NULL skips the scenario.
#' @param wind_column Wind column on `weather`. The name must end in `_ms`.
#' @return A `golfops_playable_capacity` tibble. The attribute `assumptions`
#'   states which rules were applied.
#' @export
#' @examples
#' day <- data.frame(
#'   date = as.Date("2024-07-01"),
#'   rounds_possible = 240,
#'   daylight_hours = 14,
#'   unavailable_hours = 3
#' )
#' playable_capacity(
#'   day,
#'   unavailable_hours = "unavailable_hours",
#'   daylight_hours = "daylight_hours"
#' )
playable_capacity <- function(
    data,
    capacity = "rounds_possible",
    calendar = NULL,
    unavailable_hours = NULL,
    daylight_hours = NULL,
    unavailable_fraction = NULL,
    weather = NULL,
    precip_unplayable_mm = NULL,
    precip_column = "precip_mm",
    wind_unplayable_ms = NULL,
    wind_column = "wind_speed_ms") {
  if (!is.data.frame(data)) {
    abort("`data` must be a table.", class = "golfops_input_error")
  }
  capacity <- .column_name(capacity, "capacity")
  unavailable_hours <- .column_name(unavailable_hours, "unavailable_hours")
  daylight_hours <- .column_name(daylight_hours, "daylight_hours")
  unavailable_fraction <- .column_name(unavailable_fraction, "unavailable_fraction")
  precip_column <- .column_name(precip_column, "precip_column")
  wind_column <- .column_name(wind_column, "wind_column")
  if (!capacity %in% names(data)) {
    abort(
      paste0("`", capacity, "` is not a column of `data`."),
      class = "golfops_input_error"
    )
  }
  if (!"date" %in% names(data)) {
    abort("`data` needs a date column.", class = "golfops_input_error")
  }
  if (!is.null(unavailable_hours) && is.null(daylight_hours) ||
      is.null(unavailable_hours) && !is.null(daylight_hours)) {
    abort(
      "Unavailable hours and daylight hours must be supplied together.",
      class = "golfops_input_error"
    )
  }
  if (!is.null(unavailable_hours) && !is.null(unavailable_fraction)) {
    abort(
      "Pass unavailable hours or an unavailable fraction, not both.",
      class = "golfops_input_error"
    )
  }
  stated_time <- !is.null(unavailable_hours) || !is.null(unavailable_fraction)
  weather_scenario <- !is.null(precip_unplayable_mm) || !is.null(wind_unplayable_ms)
  if (stated_time && weather_scenario) {
    abort(
      paste0(
        "Run stated unavailable time and a weather threshold as separate calls. ",
        "They answer different questions."
      ),
      class = "golfops_input_error"
    )
  }
  precip_unplayable_mm <- .positive_threshold(precip_unplayable_mm, "precip_unplayable_mm")
  wind_unplayable_ms <- .positive_threshold(wind_unplayable_ms, "wind_unplayable_ms")
  if (!grepl("_mm$", precip_column) || !grepl("_ms$", wind_column)) {
    abort(
      "Name precipitation in millimeters with a `_mm` suffix and wind in meters per second with an `_ms` suffix.",
      class = "golfops_input_error"
    )
  }

  date <- as.Date(data$date)
  theoretical <- data[[capacity]]
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  if (anyDuplicated(date)) {
    abort("`date` values must be unique.", class = "golfops_input_error")
  }
  if (!is.numeric(theoretical) || any(!is.na(theoretical) & theoretical < 0)) {
    abort(
      paste0("`", capacity, "` must be zero or positive when it is present."),
      class = "golfops_input_error"
    )
  }

  n <- nrow(data)
  closed <- rep(FALSE, n)
  if (!is.null(calendar)) {
    closed <- .closure_from_calendar(calendar, date)
  }
  if (!is.null(weather)) {
    weather <- .require_weather(weather)
    .reject_weather_units(weather)
  } else if (weather_scenario) {
    abort(
      "A precipitation or wind threshold requires a weather table.",
      class = "golfops_input_error"
    )
  }

  fraction <- rep(0, n)
  hours_stated <- rep(NA_real_, n)
  time_missing <- rep(FALSE, n)
  if (!is.null(unavailable_fraction)) {
    stated <- .stated_fraction(data, unavailable_fraction)
    fraction <- stated$value
    time_missing <- stated$missing
  }
  if (!is.null(unavailable_hours)) {
    stated <- .stated_hours(data, unavailable_hours, daylight_hours)
    fraction <- stated$value
    hours_stated <- stated$hours
    time_missing <- stated$missing
  }

  precip_hit <- rep(FALSE, n)
  wind_hit <- rep(FALSE, n)
  weather_unknown <- rep(FALSE, n)
  if (weather_scenario) {
    scored <- .weather_threshold_hits(
      weather, date, precip_unplayable_mm, precip_column,
      wind_unplayable_ms, wind_column
    )
    precip_hit <- scored$precip_hit
    wind_hit <- scored$wind_hit
    weather_unknown <- scored$unknown
  }

  source <- rep("none", n)
  restriction <- rep("none", n)
  playable <- theoretical
  applied <- fraction
  assumption_met <- if (weather_scenario) precip_hit | wind_hit else rep(FALSE, n)

  capacity_missing <- is.na(theoretical)
  source[capacity_missing] <- "capacity_missing"
  restriction[capacity_missing] <- "capacity_missing"
  applied[capacity_missing] <- NA_real_
  playable[capacity_missing] <- NA_real_

  usable <- !capacity_missing
  source[usable & time_missing] <- "stated_time_missing"
  restriction[usable & time_missing] <- "stated_time_missing"
  applied[usable & time_missing] <- NA_real_
  playable[usable & time_missing] <- NA_real_
  hours_stated[time_missing] <- NA_real_

  active <- usable & !time_missing
  reduced <- active & fraction > 0
  if (!is.null(unavailable_fraction)) {
    source[reduced] <- "stated_unavailable_fraction"
    restriction[reduced] <- "stated_unavailable_fraction"
  }
  if (!is.null(unavailable_hours)) {
    source[reduced] <- "stated_unavailable_hours"
    restriction[reduced] <- "stated_unavailable_hours"
  }
  playable[active] <- theoretical[active] * (1 - fraction[active])

  if (weather_scenario) {
    open_for_weather <- active & fraction == 0
    assumed <- open_for_weather & assumption_met
    unknown <- open_for_weather & weather_unknown & !assumption_met
    source[assumed] <- "weather_assumption"
    both <- assumed & precip_hit & wind_hit
    restriction[assumed & precip_hit & !wind_hit] <- "precipitation"
    restriction[assumed & wind_hit & !precip_hit] <- "wind"
    restriction[both] <- "precipitation_and_wind"
    applied[assumed] <- 1
    playable[assumed] <- 0
    hours_stated[assumed] <- NA_real_
    source[unknown] <- "weather_missing"
    restriction[unknown] <- "weather_missing"
    applied[unknown] <- NA_real_
    playable[unknown] <- NA_real_
    assumption_met[unknown] <- NA
  }

  source[closed] <- "observed_closure"
  restriction[closed] <- "observed_closure"
  applied[closed] <- 1
  playable[closed] <- 0

  out <- tibble(
    date = date,
    theoretical_capacity = theoretical,
    playable_capacity = playable,
    unavailable_fraction_applied = applied,
    unavailable_hours_stated = hours_stated,
    restriction = restriction,
    restriction_source = source,
    weather_assumption_met = assumption_met
  )
  assumptions <- c(
    "theoretical_capacity is the capacity column passed into this function. course_capacity() and rounds_possible are not modified.",
    "High temperature does not reduce playable capacity. Temperature can be compared with rounds when the question is demand.",
    "Unavailable hours reduce theoretical capacity in proportion to daylight hours. The hours are a statement in the table. They are not calculated from precipitation, wind, or temperature.",
    "A stated fraction is applied as given. It is not inferred from a daily weather total."
  )
  if (!is.null(calendar)) {
    assumptions <- c(
      assumptions,
      "A calendar status of closed sets playable capacity to zero. That label is the caller's observed closure."
    )
  }
  if (weather_scenario) {
    assumptions <- c(
      assumptions,
      "A day at or above the precipitation or wind threshold is treated as fully unavailable. This is a scenario, not an observed closure. The daily total is not a count of lost hours, so unavailable hours stay missing."
    )
    if (!is.null(precip_unplayable_mm)) {
      assumptions <- c(
        assumptions,
        sprintf(
          "The precipitation scenario uses %s and treats a day as unavailable at %.3f millimeters or more.",
          precip_column, precip_unplayable_mm
        )
      )
    }
    if (!is.null(wind_unplayable_ms)) {
      assumptions <- c(
        assumptions,
        sprintf(
          "The wind scenario uses %s and treats a day as unavailable at %.3f meters per second or more.",
          wind_column, wind_unplayable_ms
        )
      )
    }
  }
  class(out) <- unique(c("golfops_playable_capacity", class(out)))
  attr(out, "assumptions") <- assumptions
  out
}

#' @export
print.golfops_playable_capacity <- function(x, ...) {
  cat("Playable capacity\n")
  cat(paste0("- ", attr(x, "assumptions")), sep = "\n")
  cat("\n")
  y <- x
  class(y) <- setdiff(class(y), "golfops_playable_capacity")
  print(y, ...)
  invisible(x)
}

.read_character_csv <- function(path) {
  read_csv(
    path,
    col_types = cols(.default = col_character()),
    na = c("", "NA"),
    progress = FALSE,
    show_col_types = FALSE
  )
}

.read_weather_stations <- function(path) {
  raw <- .read_character_csv(path)
  required <- c(
    "role", "station_id", "station_name", "latitude", "longitude",
    "elevation_m", "distance_km", "variables", "coordinate_note",
    "source_url", "retrieved"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    abort(
      paste0("Station file is missing ", paste(missing, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  stations <- tibble(
    role = raw$role,
    station_id = raw$station_id,
    station_name = raw$station_name,
    latitude = .parse_number(raw$latitude, "latitude"),
    longitude = .parse_number(raw$longitude, "longitude"),
    elevation_m = .parse_number(raw$elevation_m, "elevation_m"),
    distance_km = .parse_number(raw$distance_km, "distance_km"),
    variables = raw$variables,
    coordinate_note = raw$coordinate_note,
    source_url = raw$source_url,
    retrieved = raw$retrieved
  )
  needed <- c("course", "precipitation", "temperature", "wind", "airport_precipitation")
  if (!all(needed %in% stations$role)) {
    abort(
      "The station file must identify the course, the local gauge, and the airport.",
      class = "golfops_input_error"
    )
  }
  stations
}

.parse_number <- function(x, name) {
  x <- as.character(x)
  x[!nzchar(x) | is.na(x)] <- NA_character_
  bad <- !is.na(x) & grepl("[^0-9eE.+\\-]", x)
  if (any(bad)) {
    abort(
      paste0("`", name, "` contains a non-numeric value."),
      class = "golfops_input_error"
    )
  }
  suppressWarnings(as.numeric(x))
}

.parse_logical <- function(x, name) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(x))
  out[x %in% c("true", "t")] <- TRUE
  out[x %in% c("false", "f")] <- FALSE
  if (any(is.na(out))) {
    abort(
      paste0("`", name, "` must be TRUE or FALSE."),
      class = "golfops_input_error"
    )
  }
  as.logical(out)
}

.empty_flag <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

.reject_weather_units <- function(data) {
  .reject_range(data, "temp_min_c", -80, 60, "degrees Celsius")
  .reject_range(data, "temp_max_c", -80, 60, "degrees Celsius")
  .reject_range(data, "temp_avg_c", -80, 60, "degrees Celsius")
  .reject_range(data, "precip_mm", 0, 500, "millimeters")
  .reject_range(data, "precip_mm_airport", 0, 500, "millimeters")
  .reject_range(data, "wind_speed_ms", 0, 40, "meters per second")
  invisible(data)
}

.reject_range <- function(data, column, lower, upper, unit) {
  if (!column %in% names(data)) {
    return(invisible(NULL))
  }
  value <- data[[column]]
  if (!is.numeric(value)) {
    abort(
      paste0("`", column, "` must be numeric ", unit, "."),
      class = "golfops_input_error"
    )
  }
  bad <- !is.na(value) & (value < lower | value > upper)
  if (any(bad)) {
    abort(
      paste0(
        "`", column, "` has a value outside ", lower, " to ", upper,
        ". Those bounds are the guard for ", unit, "."
      ),
      class = "golfops_input_error"
    )
  }
  invisible(NULL)
}

.column_name <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    abort(
      paste0("`", arg, "` must be one column name."),
      class = "golfops_input_error"
    )
  }
  x
}

.positive_threshold <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) || x <= 0) {
    abort(
      paste0("`", arg, "` must be one positive number, or NULL to skip that scenario."),
      class = "golfops_input_error"
    )
  }
  x
}

.closure_from_calendar <- function(calendar, date) {
  if (!inherits(calendar, "golfops_operating_calendar")) {
    abort(
      "Expected a calendar from operating_calendar().",
      class = "golfops_input_error"
    )
  }
  if (anyDuplicated(calendar$date)) {
    abort("The operating calendar repeats a date.", class = "golfops_input_error")
  }
  status <- calendar$status[match(date, calendar$date)]
  !is.na(status) & status == "closed"
}

.require_weather <- function(weather) {
  if (!inherits(weather, "golfops_course_weather")) {
    abort(
      "Expected weather from course_weather() or prepare_dobson_weather().",
      class = "golfops_input_error"
    )
  }
  if (any(is.na(weather$date)) || anyDuplicated(weather$date)) {
    abort("Weather dates must be complete and unique.", class = "golfops_input_error")
  }
  weather
}

.stated_fraction <- function(data, column) {
  if (!column %in% names(data)) {
    abort(
      paste0("`", column, "` is not a column of `data`."),
      class = "golfops_input_error"
    )
  }
  value <- data[[column]]
  if (!is.numeric(value)) {
    abort(
      paste0("`", column, "` must be numeric."),
      class = "golfops_input_error"
    )
  }
  bad <- !is.na(value) & (value < 0 | value > 1)
  if (any(bad)) {
    abort(
      "An unavailable fraction must be from 0 to 1.",
      class = "golfops_input_error"
    )
  }
  list(value = ifelse(is.na(value), NA_real_, value), missing = is.na(value))
}

.stated_hours <- function(data, hours_column, daylight_column) {
  if (!hours_column %in% names(data) || !daylight_column %in% names(data)) {
    abort(
      "Unavailable hours and daylight hours must be columns of `data`.",
      class = "golfops_input_error"
    )
  }
  hours <- data[[hours_column]]
  daylight <- data[[daylight_column]]
  if (!is.numeric(hours) || !is.numeric(daylight)) {
    abort(
      "Unavailable hours and daylight hours must be numeric.",
      class = "golfops_input_error"
    )
  }
  if (any(!is.na(hours) & hours < 0) || any(!is.na(daylight) & daylight <= 0)) {
    abort(
      "Unavailable hours must be zero or positive. Daylight hours must be positive when supplied.",
      class = "golfops_input_error"
    )
  }
  comparable <- !is.na(hours) & !is.na(daylight)
  if (any(comparable & hours > daylight)) {
    abort(
      "Unavailable hours exceed daylight hours. State a fraction when the intended loss is the whole day.",
      class = "golfops_input_error"
    )
  }
  missing <- is.na(hours) | (hours > 0 & is.na(daylight))
  fraction <- ifelse(is.na(hours), NA_real_, ifelse(hours == 0, 0, hours / daylight))
  list(value = fraction, hours = hours, missing = missing)
}

.weather_threshold_hits <- function(
    weather,
    date,
    precip_unplayable_mm,
    precip_column,
    wind_unplayable_ms,
    wind_column) {
  n <- length(date)
  idx <- match(date, weather$date)
  precip_hit <- rep(FALSE, n)
  wind_hit <- rep(FALSE, n)
  unknown <- rep(FALSE, n)
  if (!is.null(precip_unplayable_mm)) {
    if (!precip_column %in% names(weather)) {
      abort(
        paste0("Weather is missing `", precip_column, "`."),
        class = "golfops_input_error"
      )
    }
    precip <- weather[[precip_column]][idx]
    if (!is.numeric(precip)) {
      abort(
        paste0("`", precip_column, "` must be numeric millimeters."),
        class = "golfops_input_error"
      )
    }
    precip_hit <- !is.na(precip) & precip >= precip_unplayable_mm
    unknown <- unknown | is.na(idx) | is.na(precip)
  }
  if (!is.null(wind_unplayable_ms)) {
    if (!wind_column %in% names(weather)) {
      abort(
        paste0("Weather is missing `", wind_column, "`."),
        class = "golfops_input_error"
      )
    }
    wind <- weather[[wind_column]][idx]
    if (!is.numeric(wind)) {
      abort(
        paste0("`", wind_column, "` must be numeric meters per second."),
        class = "golfops_input_error"
      )
    }
    wind_hit <- !is.na(wind) & wind >= wind_unplayable_ms
    unknown <- unknown | is.na(idx) | is.na(wind)
  }
  list(precip_hit = precip_hit, wind_hit = wind_hit, unknown = unknown)
}

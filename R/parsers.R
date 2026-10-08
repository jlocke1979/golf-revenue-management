parse_number_field <- function(x, field) {
  original <- trimws(as.character(x))
  original[original %in% c("", "NA", "null")] <- NA_character_
  cleaned <- gsub("[$,]", "", original)
  cleaned <- trimws(cleaned)
  out <- suppressWarnings(as.numeric(cleaned))
  bad <- !is.na(cleaned) & is.na(out)
  if (any(bad)) {
    abort(
      paste0("Could not parse ", field, " value: ", cleaned[which(bad)[1]]),
      class = "golfops_parse_error"
    )
  }
  out
}

parse_rate_field <- function(x) {
  original <- trimws(as.character(x))
  original[original %in% c("", "NA", "null")] <- NA_character_
  is_percent <- !is.na(original) & grepl("%", original, fixed = TRUE)
  cleaned <- gsub("[%,]", "", original)
  cleaned <- trimws(cleaned)
  out <- suppressWarnings(as.numeric(cleaned))
  bad <- !is.na(cleaned) & is.na(out)
  if (any(bad)) {
    abort(
      paste0("Could not parse Round Booking Rate value: ", cleaned[which(bad)[1]]),
      class = "golfops_parse_error"
    )
  }
  unmarked_percent <- !is.na(out) & !is_percent & out > 5
  if (any(unmarked_percent)) {
    abort(
      paste0(
        "Round Booking Rate looks like a percent but has no % sign: ",
        out[which(unmarked_percent)[1]],
        ". The City CSV stores this field as a percent."
      ),
      class = "golfops_parse_error"
    )
  }
  out[is_percent] <- out[is_percent] / 100
  out
}

parse_clock_hours <- function(x) {
  original <- trimws(as.character(x))
  original[original %in% c("", "NA")] <- NA_character_
  out <- rep(NA_real_, length(original))
  ok <- !is.na(original) & grepl("^\\d{1,2}:\\d{2}:\\d{2}$", original)
  if (!any(ok)) {
    return(out)
  }
  parts <- do.call(rbind, strsplit(original[ok], ":", fixed = TRUE))
  mode(parts) <- "numeric"
  out[ok] <- parts[, 1] + parts[, 2] / 60 + parts[, 3] / 3600
  out
}

month_names <- function() {
  c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
}

weekday_names <- function() {
  c(
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
  )
}

season_names <- function() {
  c("Winter", "Spring", "Summer", "Fall")
}

expected_fiscal_year <- function(date) {
  y <- year(date)
  m <- month(date)
  start <- ifelse(m >= 7, y, y - 1)
  end_yy <- (start + 1) %% 100
  start_yy <- start %% 100
  end_label <- ifelse(end_yy < 10, as.character(end_yy), sprintf("%02d", end_yy))
  sprintf("FY%02d-%s", start_yy, end_label)
}

expected_season <- function(date) {
  season_names()[c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 1)[month(date)]]
}

expected_weekday <- function(date) {
  sunday_first <- c(
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
  )
  sunday_first[wday(date)]
}

sum_defined <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    NA_real_
  } else {
    sum(x, na.rm = TRUE)
  }
}

.new_rounds <- function(data, threshold, source_path = NA_character_) {
  data <- as_tibble(data)
  class(data) <- unique(c("golfops_rounds", class(data)))
  attr(data, "monthly_rounds_threshold") <- threshold
  attr(data, "source_path") <- source_path
  data
}

.require_rounds <- function(data) {
  required <- c(
    "date", "grain", "rounds_played", "rounds_possible",
    "green_fee_revenue", "total_revenue", "flag_duplicate_date",
    "row_id"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    abort(
      paste0(
        "Expected a table from prepare_dobson_rounds(). Missing columns: ",
        paste(missing, collapse = ", ")
      ),
      class = "golfops_input_error"
    )
  }
  if (!inherits(data, "golfops_rounds")) {
    data <- .new_rounds(
      data,
      attr(data, "monthly_rounds_threshold") %||% 1000,
      attr(data, "source_path") %||% NA_character_
    )
  }
  data
}

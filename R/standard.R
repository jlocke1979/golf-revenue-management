#' One row per course and period for participation
#'
#' The reusable analysis table. Source files are converted into this shape by
#' an importer. Daily and monthly rows cannot share a table: adding them would
#' count a month as a day. Unknown definitions stay unknown. This constructor
#' does not infer tee-sheet capacity, cart bundling, taxes, or which rounds
#' were membership play.
#'
#' @param course_id Course identifier.
#' @param date Period date. A daily row is the service date when that is known.
#'   A monthly row is the period start used by the source.
#' @param frequency `day` or `month`. Every row in one table must match.
#' @param rounds Rounds played. Missing stays missing.
#' @param capacity Optional capacity count in rounds.
#' @param capacity_basis Why `capacity` is that number.
#' @param operating_status Open, closed, unknown, or a status already assigned
#'   by [apply_operating_capacity()].
#' @param currency ISO currency code, or `"unknown"`.
#' @param tax_treatment How tax sits in the amounts, or `"unknown"`.
#' @param refund_treatment How refunds sit in the amounts, or `"unknown"`.
#' @param holes `"9"`, `"18"`, `"mixed"`, or `"unknown"`.
#' @param player_type `"daily_fee"`, `"membership"`, `"mixed"`, or
#'   `"unknown"`.
#' @param service_or_posting `"service_date"`, `"posting_date"`, or `"unknown"`.
#' @return A `golfops_course_period` tibble.
#' @export
course_period <- function(
    course_id,
    date,
    frequency,
    rounds,
    capacity = NA_real_,
    capacity_basis = "unknown",
    operating_status = "unknown",
    currency = "unknown",
    tax_treatment = "unknown",
    refund_treatment = "unknown",
    holes = "unknown",
    player_type = "unknown",
    service_or_posting = "unknown") {
  date <- as.Date(date)
  n <- length(date)
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  frequency <- .recycle_atomic(frequency, n, "frequency")
  if (!all(frequency %in% c("day", "month"))) {
    abort('`frequency` must be "day" or "month".', class = "golfops_input_error")
  }
  if (length(unique(frequency)) != 1) {
    abort(
      "A course-period table cannot mix daily and monthly rows.",
      class = "golfops_input_error"
    )
  }
  course_id <- .recycle_atomic(as.character(course_id), n, "course_id")
  rounds <- .recycle_atomic(as.numeric(rounds), n, "rounds")
  capacity <- .recycle_atomic(as.numeric(capacity), n, "capacity")
  capacity_basis <- .recycle_atomic(as.character(capacity_basis), n, "capacity_basis")
  operating_status <- .recycle_atomic(as.character(operating_status), n, "operating_status")
  currency <- .recycle_atomic(as.character(currency), n, "currency")
  tax_treatment <- .recycle_atomic(as.character(tax_treatment), n, "tax_treatment")
  refund_treatment <- .recycle_atomic(as.character(refund_treatment), n, "refund_treatment")
  holes <- .recycle_atomic(as.character(holes), n, "holes")
  player_type <- .recycle_atomic(as.character(player_type), n, "player_type")
  service_or_posting <- .recycle_atomic(as.character(service_or_posting), n, "service_or_posting")
  allowed_basis <- c(
    "unknown", "unavailable", "daylight_estimate", "schedule_estimate", "available_rounds"
  )
  if (!all(capacity_basis %in% allowed_basis)) {
    abort(
      paste0("`capacity_basis` must be one of: ", paste(allowed_basis, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  if (!all(holes %in% c("9", "18", "mixed", "unknown"))) {
    abort('`holes` must be "9", "18", "mixed", or "unknown".', class = "golfops_input_error")
  }
  if (!all(player_type %in% c("daily_fee", "membership", "mixed", "unknown"))) {
    abort(
      '`player_type` must be "daily_fee", "membership", "mixed", or "unknown".',
      class = "golfops_input_error"
    )
  }
  if (!all(service_or_posting %in% c("service_date", "posting_date", "unknown"))) {
    abort(
      '`service_or_posting` must be "service_date", "posting_date", or "unknown".',
      class = "golfops_input_error"
    )
  }
  key <- paste(course_id, date, frequency)
  if (any(duplicated(key))) {
    abort(
      "Repeated course, date, and frequency. Joining this table would double-count rounds.",
      class = "golfops_duplicate_error"
    )
  }
  out <- tibble(
    course_id = course_id,
    date = date,
    frequency = frequency,
    rounds = rounds,
    capacity = capacity,
    capacity_basis = capacity_basis,
    operating_status = operating_status,
    currency = currency,
    tax_treatment = tax_treatment,
    refund_treatment = refund_treatment,
    holes = holes,
    player_type = player_type,
    service_or_posting = service_or_posting
  )
  class(out) <- unique(c("golfops_course_period", class(out)))
  out
}

#' Revenue categories for the same course period
#'
#' One row per course, date, and frequency. Categories that the source does
#' not report stay missing. A missing cart amount is not a measured zero.
#' `cart_green_fee_treatment` records whether those two amounts are known to
#' be separate. The default is `"unknown"`, so a later ratio does not assume
#' a bundled cart fee has already been removed from green fees.
#'
#' @param course_id,date,frequency Identifiers. `frequency` cannot mix `day`
#'   and `month`.
#' @param green_fee,food_beverage,range,merchandise,cart,club Revenue amounts.
#' @param currency,tax_treatment,refund_treatment,service_or_posting Source
#'   definitions. Unknown stays `"unknown"`.
#' @param cart_green_fee_treatment `"separated"`, `"bundled"`,
#'   `"published_as_separate_categories"`, or `"unknown"`.
#' @return A `golfops_course_revenue` tibble.
#' @export
course_revenue <- function(
    course_id,
    date,
    frequency,
    green_fee = NA_real_,
    food_beverage = NA_real_,
    range = NA_real_,
    merchandise = NA_real_,
    cart = NA_real_,
    club = NA_real_,
    currency = "unknown",
    tax_treatment = "unknown",
    refund_treatment = "unknown",
    service_or_posting = "unknown",
    cart_green_fee_treatment = "unknown") {
  date <- as.Date(date)
  n <- length(date)
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  frequency <- .recycle_atomic(frequency, n, "frequency")
  if (!all(frequency %in% c("day", "month")) || length(unique(frequency)) != 1) {
    abort(
      'Revenue rows must use one frequency, either "day" or "month".',
      class = "golfops_input_error"
    )
  }
  course_id <- .recycle_atomic(as.character(course_id), n, "course_id")
  treatment <- .recycle_atomic(
    as.character(cart_green_fee_treatment),
    n,
    "cart_green_fee_treatment"
  )
  if (!all(treatment %in% c(
    "unknown", "separated", "bundled", "published_as_separate_categories"
  ))) {
    abort(
      "`cart_green_fee_treatment` is not a recognized value.",
      class = "golfops_input_error"
    )
  }
  key <- paste(course_id, date, frequency)
  if (any(duplicated(key))) {
    abort(
      "Repeated course, date, and frequency. Joining this table would double-count revenue.",
      class = "golfops_duplicate_error"
    )
  }
  out <- tibble(
    course_id = course_id,
    date = date,
    frequency = frequency,
    green_fee = .recycle_atomic(as.numeric(green_fee), n, "green_fee"),
    food_beverage = .recycle_atomic(as.numeric(food_beverage), n, "food_beverage"),
    range = .recycle_atomic(as.numeric(range), n, "range"),
    merchandise = .recycle_atomic(as.numeric(merchandise), n, "merchandise"),
    cart = .recycle_atomic(as.numeric(cart), n, "cart"),
    club = .recycle_atomic(as.numeric(club), n, "club"),
    currency = .recycle_atomic(as.character(currency), n, "currency"),
    tax_treatment = .recycle_atomic(as.character(tax_treatment), n, "tax_treatment"),
    refund_treatment = .recycle_atomic(as.character(refund_treatment), n, "refund_treatment"),
    service_or_posting = .recycle_atomic(as.character(service_or_posting), n, "service_or_posting"),
    cart_green_fee_treatment = treatment
  )
  class(out) <- unique(c("golfops_course_revenue", class(out)))
  out
}

#' Attach revenue to participation without adding a month to a day
#'
#' The join keys are course, date, and frequency. A repeated key is an error.
#' A monthly revenue row does not attach to a daily participation row on the
#' same calendar date.
#'
#' @param periods A [course_period()] table.
#' @param revenue A [course_revenue()] table.
#' @return The participation rows with revenue columns added. Unmatched revenue
#'   is not silently summed into another period.
#' @export
join_course_revenue <- function(periods, revenue) {
  if (!inherits(periods, "golfops_course_period")) {
    abort("Expected a course_period() table.", class = "golfops_input_error")
  }
  if (!inherits(revenue, "golfops_course_revenue")) {
    abort("Expected a course_revenue() table.", class = "golfops_input_error")
  }
  period_key <- paste(periods$course_id, periods$date, periods$frequency)
  revenue_key <- paste(revenue$course_id, revenue$date, revenue$frequency)
  if (any(duplicated(period_key)) || any(duplicated(revenue_key))) {
    abort("Join keys are repeated, so the tables would double-count.", class = "golfops_duplicate_error")
  }
  meta <- intersect(
    c("currency", "tax_treatment", "refund_treatment", "service_or_posting"),
    intersect(names(periods), names(revenue))
  )
  matched <- merge(
    periods[, c("course_id", "date", "frequency", meta), drop = FALSE],
    revenue[, c("course_id", "date", "frequency", meta), drop = FALSE],
    by = c("course_id", "date", "frequency"),
    suffixes = c(".period", ".revenue")
  )
  for (column in meta) {
    left <- matched[[paste0(column, ".period")]]
    right <- matched[[paste0(column, ".revenue")]]
    if (any(left != right)) {
      abort(
        paste0("`", column, "` disagrees between participation and revenue on a matched period."),
        class = "golfops_input_error"
      )
    }
  }
  amounts <- c(
    "green_fee", "food_beverage", "range", "merchandise", "cart", "club",
    "cart_green_fee_treatment"
  )
  clash <- intersect(amounts, names(periods))
  if (length(clash) > 0) {
    abort(
      paste0("Participation rows already contain revenue columns: ", paste(clash, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  revenue_amounts <- revenue[, c("course_id", "date", "frequency", intersect(amounts, names(revenue))), drop = FALSE]
  merged <- merge(
    periods,
    revenue_amounts,
    by = c("course_id", "date", "frequency"),
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(merged) != nrow(periods)) {
    abort("The revenue join changed the number of participation rows.", class = "golfops_input_error")
  }
  merged <- as_tibble(merged)
  class(merged) <- unique(c("golfops_course_period", class(merged)))
  merged
}

#' Optional weather observations or forecast-time weather
#'
#' A retrospective description may use weather that was observed on the day.
#' A forecast may use only weather that was available when the prediction was
#' made. This table records that timing. It does not download a weather feed,
#' and the Dobson model does not gain a weather term until a real series is
#' validated.
#'
#' @param course_id,date Course and date.
#' @param timing `observed` or `available_at_forecast`.
#' @param source Where the values came from.
#' @param ... Optional weather columns, such as precipitation.
#' @return A `golfops_course_weather` tibble.
#' @export
course_weather <- function(course_id, date, timing, source = "unknown", ...) {
  date <- as.Date(date)
  n <- length(date)
  timing <- .recycle_atomic(timing, n, "timing")
  if (!all(timing %in% c("observed", "available_at_forecast"))) {
    abort(
      '`timing` must be "observed" or "available_at_forecast".',
      class = "golfops_input_error"
    )
  }
  extra <- tibble(...)
  if (ncol(extra) > 0 && nrow(extra) != n) {
    abort("Weather columns must have one value per date.", class = "golfops_input_error")
  }
  out <- tibble(
    course_id = .recycle_atomic(as.character(course_id), n, "course_id"),
    date = date,
    timing = timing,
    source = .recycle_atomic(as.character(source), n, "source")
  )
  if (ncol(extra) > 0) {
    out <- cbind(out, extra)
    out <- as_tibble(out)
  }
  class(out) <- unique(c("golfops_course_weather", class(out)))
  out
}

#' Optional event list documented independently of the rounds file
#'
#' An event name is attached only when the caller supplies it. The package
#' does not infer a charity tournament, a holiday, or a broadcast weekend from
#' a spike in rounds.
#'
#' @param course_id,date Course and date.
#' @param event_name Name from the external source.
#' @param source Where the event list came from.
#' @return A `golfops_course_events` tibble.
#' @export
course_events <- function(course_id, date, event_name, source = "unknown") {
  date <- as.Date(date)
  n <- length(date)
  out <- tibble(
    course_id = .recycle_atomic(as.character(course_id), n, "course_id"),
    date = date,
    event_name = .recycle_atomic(as.character(event_name), n, "event_name"),
    source = .recycle_atomic(as.character(source), n, "source")
  )
  class(out) <- unique(c("golfops_course_events", class(out)))
  out
}

#' Dobson operating rows in the course-period shape
#'
#' Currency is USD because the city extract is in dollars. Tax treatment,
#' refund treatment, 9-hole versus 18-hole counts, and membership versus
#' daily-fee rounds are not identified in the file, so those fields stay
#' unknown. Capacity is the daylight estimate.
#'
#' @param rounds Optional result of [prepare_dobson_rounds()].
#' @return A `golfops_course_period` table.
#' @export
dobson_course_period <- function(rounds = NULL) {
  daily <- dobson_operating_daily(rounds)
  course_period(
    course_id = "dobson_ranch",
    date = daily$date,
    frequency = "day",
    rounds = daily$rounds_played,
    capacity = daily$capacity_rounds,
    capacity_basis = daily$capacity_basis,
    operating_status = daily$operating_status,
    currency = "USD",
    tax_treatment = "unknown",
    refund_treatment = "unknown",
    holes = "unknown",
    player_type = "unknown",
    service_or_posting = "unknown"
  )
}

#' Dobson revenue categories in the course-revenue shape
#'
#' The city publishes cart, club, food and beverage, merchandise, range, and
#' green fees as separate columns, and complete rows sum to total revenue.
#' That supports `published_as_separate_categories`. It does not say whether a
#' posted green fee included a cart.
#'
#' @param rounds Optional result of [prepare_dobson_rounds()].
#' @return A `golfops_course_revenue` table.
#' @export
dobson_course_revenue <- function(rounds = NULL) {
  daily <- dobson_operating_daily(rounds)
  course_revenue(
    course_id = "dobson_ranch",
    date = daily$date,
    frequency = "day",
    green_fee = daily$green_fee_revenue,
    food_beverage = daily$food_beverage_revenue,
    range = daily$range_revenue,
    merchandise = daily$merchandise_revenue,
    cart = daily$cart_revenue,
    club = daily$club_revenue,
    currency = "USD",
    tax_treatment = "unknown",
    refund_treatment = "unknown",
    service_or_posting = "unknown",
    cart_green_fee_treatment = "published_as_separate_categories"
  )
}

#' Names of operator exports that are not yet importers
#'
#' These are possible sources after a course authorizes an export. They are
#' not open datasets, and this package does not contain their schemas.
#'
#' @return A tibble of source names and documentation links.
#' @export
future_operator_exports <- function() {
  tibble(
    source = c("foreUP reports", "Lightspeed Golf legacy reports", "GolfNow / G1"),
    role = rep(
      "Possible operator-authorized export. Not an open dataset and not a verified schema in this package.",
      3
    ),
    documentation = c(
      "https://foreup.zendesk.com/hc/en-us/sections/360001434454-Reports",
      "https://golf-support.lightspeedhq.com/hc/en-us/articles/12559256125595-About-legacy-business-intelligence-reports",
      "https://golfnowbusiness.com/capabilities/"
    )
  )
}

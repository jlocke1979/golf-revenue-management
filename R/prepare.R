#' Prepare the City of Mesa Dobson Ranch rounds export
#'
#' Reads the 23-column city extract, parses dates and currency, and separates
#' monthly totals from daily rows. In this extract the two grains do not
#' overlap: daily rows stay below 1,000 rounds and monthly rows stay above
#' 2,900. The default threshold of 1,000 sits in that gap. It is a rule for
#' this file, not a definition supplied by the catalog.
#'
#' The catalog describes `Rounds Possible` as a daylight-based estimate, not
#' as tee-sheet capacity. `Round Booking Rate` is the percent of those
#' possible rounds that were played. Cart and club rental counts are described
#' as derived from revenue. Revenue per played round is recomputed here. The
#' city's `Revenue/Round` column is retained for comparison.
#'
#' @param path Path to the city CSV. Defaults to the bundled example.
#' @param monthly_rounds_threshold Rounds-played cutoff used to label a row
#'   as a monthly total. Rows at or above the cutoff are `grain = "month"`.
#' @return A `golfops_rounds` tibble with one row per source record, including
#'   duplicate dates. Duplicates are flagged and are not removed.
#' @export
#' @examples
#' rounds <- prepare_dobson_rounds()
#' table(rounds$grain)
prepare_dobson_rounds <- function(
    path = dobson_rounds_example(),
    monthly_rounds_threshold = 1000) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    abort("`path` must be a single CSV path.", class = "golfops_input_error")
  }
  if (!file.exists(path)) {
    abort(paste0("File not found: ", path), class = "golfops_input_error")
  }
  if (!is.numeric(monthly_rounds_threshold) ||
      length(monthly_rounds_threshold) != 1 ||
      !(monthly_rounds_threshold > 0)) {
    abort(
      "`monthly_rounds_threshold` must be a single positive number.",
      class = "golfops_input_error"
    )
  }

  raw <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    na = character(),
    progress = FALSE,
    show_col_types = FALSE
  )
  missing_cols <- setdiff(dobson_column_names(), names(raw))
  if (length(missing_cols) > 0) {
    abort(
      paste0(
        "The CSV is missing City of Mesa columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      class = "golfops_schema_error"
    )
  }

  date <- as_date(parse_date_time(
    raw$Date,
    orders = "Ybd IMSp",
    quiet = TRUE
  ))
  if (any(is.na(date))) {
    abort(
      "Could not parse one or more Date values. Expected values such as '2024 Jun 15 12:00:00 AM'.",
      class = "golfops_parse_error"
    )
  }

  rounds_played <- parse_number_field(raw$`Rounds Played`, "Rounds Played")
  rounds_possible <- parse_number_field(raw$`Rounds Possible`, "Rounds Possible")
  green_fee_revenue <- parse_number_field(raw$`Green Fees`, "Green Fees")
  total_revenue <- parse_number_field(raw$`Total Revenue`, "Total Revenue")
  cart_revenue <- parse_number_field(raw$`Cart Revenue`, "Cart Revenue")
  club_revenue <- parse_number_field(raw$`Club Revenue`, "Club Revenue")
  food_beverage_revenue <- parse_number_field(raw$`Food and Beverage`, "Food and Beverage")
  merchandise_revenue <- parse_number_field(raw$`Merchandise Revenue`, "Merchandise Revenue")
  range_revenue <- parse_number_field(raw$`Range Sales`, "Range Sales")
  revenue_per_round_source <- parse_number_field(raw$`Revenue/Round`, "Revenue/Round")
  booking_rate <- parse_rate_field(raw$`Round Booking Rate`)

  sunrise_hours <- parse_clock_hours(raw$Sunrise)
  sunset_hours <- parse_clock_hours(raw$Sunset)
  daylight_hours <- sunset_hours - sunrise_hours
  daylight_hours[!is.na(daylight_hours) & daylight_hours <= 0] <- NA_real_

  grain <- ifelse(
    is.na(rounds_played),
    NA_character_,
    ifelse(rounds_played >= monthly_rounds_threshold, "month", "day")
  )

  month_label <- month_names()[month(date)]
  weekday_label <- expected_weekday(date)
  season_label <- expected_season(date)
  fiscal_label <- expected_fiscal_year(date)
  day_number <- as.integer(format(date, "%d"))

  calendar_mismatch <- raw$Month != month_label |
    raw$`Day of Week` != weekday_label |
    as.integer(raw$Day) != day_number |
    raw$Season != season_label |
    raw$`Fiscal Year` != fiscal_label

  component_names_present <- !is.na(cart_revenue) &
    !is.na(club_revenue) &
    !is.na(food_beverage_revenue) &
    !is.na(green_fee_revenue) &
    !is.na(merchandise_revenue) &
    !is.na(range_revenue)
  component_revenue <- ifelse(
    component_names_present,
    cart_revenue + club_revenue + food_beverage_revenue +
      green_fee_revenue + merchandise_revenue + range_revenue,
    NA_real_
  )

  out <- tibble(
    row_id = parse_number_field(raw$RowID, "RowID"),
    date = date,
    calendar_year = year(date),
    fiscal_year = fiscal_label,
    month = factor(month_label, levels = month_names()),
    day = day_number,
    weekday = factor(weekday_label, levels = weekday_names()),
    season = factor(season_label, levels = season_names()),
    daylight_hours = daylight_hours,
    grain = grain,
    rounds_played = rounds_played,
    rounds_possible = rounds_possible,
    booking_rate = booking_rate,
    utilization = ifelse(
      !is.na(rounds_possible) & rounds_possible > 0,
      rounds_played / rounds_possible,
      NA_real_
    ),
    open_daylight_rounds = pmax(rounds_possible - rounds_played, 0),
    cart_rentals = parse_number_field(raw$`Cart Rentals`, "Cart Rentals"),
    cart_revenue = cart_revenue,
    club_rentals = parse_number_field(raw$`Club Rentals`, "Club Rentals"),
    club_revenue = club_revenue,
    food_beverage_revenue = food_beverage_revenue,
    green_fee_revenue = green_fee_revenue,
    merchandise_revenue = merchandise_revenue,
    range_revenue = range_revenue,
    ancillary_revenue = ifelse(
      !is.na(total_revenue) & !is.na(green_fee_revenue),
      total_revenue - green_fee_revenue,
      NA_real_
    ),
    total_revenue = total_revenue,
    component_revenue = component_revenue,
    revenue_per_round_source = revenue_per_round_source,
    green_fee_per_played_round = ifelse(
      !is.na(rounds_played) & rounds_played > 0,
      green_fee_revenue / rounds_played,
      NA_real_
    ),
    total_revenue_per_played_round = ifelse(
      !is.na(rounds_played) & rounds_played > 0,
      total_revenue / rounds_played,
      NA_real_
    ),
    green_fee_per_daylight_round = ifelse(
      !is.na(rounds_possible) & rounds_possible > 0,
      green_fee_revenue / rounds_possible,
      NA_real_
    ),
    total_revenue_per_daylight_round = ifelse(
      !is.na(rounds_possible) & rounds_possible > 0,
      total_revenue / rounds_possible,
      NA_real_
    ),
    flag_duplicate_date = duplicated(date) | duplicated(date, fromLast = TRUE),
    flag_zero_rounds = !is.na(rounds_played) & rounds_played == 0,
    flag_above_daylight_capacity = !is.na(rounds_played) &
      !is.na(rounds_possible) &
      rounds_played > rounds_possible,
    flag_calendar_mismatch = calendar_mismatch %in% TRUE,
    flag_missing_total_revenue = is.na(total_revenue)
  )

  .new_rounds(out, monthly_rounds_threshold, normalizePath(path, mustWork = FALSE))
}

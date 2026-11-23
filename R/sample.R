#' Daily Dobson Ranch rows used for operating charts and forecasts
#'
#' This is the shared analytical sample. It keeps daily rows from 2021-07-01
#' through the last date in the extract, drops the identical extra copy of
#' 2022-04-24, and leaves 2024-12-10 missing. The isolated 2004-12-10 row stays
#' in [prepare_dobson_rounds()] and in the audit. It is not plotted here.
#' Monthly totals are not included.
#'
#' Zero-round days remain in the sample. Their status is unknown unless an
#' operating calendar says otherwise. The October clusters are not a confirmed
#' closure in this function. Nothing in this function invents a tee sheet or
#' the missing December day.
#'
#' @param rounds Optional result of [prepare_dobson_rounds()].
#' @return A `golfops_rounds` table with capacity columns from
#'   [apply_operating_capacity()].
#' @export
#' @examples
#' sample <- dobson_operating_daily()
#' range(sample$date)
dobson_operating_daily <- function(rounds = NULL) {
  if (is.null(rounds)) {
    rounds <- prepare_dobson_rounds()
  }
  rounds <- .require_rounds(rounds)
  daily <- rounds[
    rounds$grain == "day" & rounds$date >= as.Date("2021-07-01"),
    ,
    drop = FALSE
  ]
  daily <- .drop_identical_dates(daily)
  daily <- .new_rounds(
    daily,
    attr(rounds, "monthly_rounds_threshold") %||% 1000,
    attr(rounds, "source_path") %||% NA_character_
  )
  daily <- apply_operating_capacity(daily)
  attr(daily, "sample_id") <- "dobson_operating_daily"
  attr(daily, "decisions") <- c(
    "Operating charts and the Dobson forecast use daily rows from 2021-07-01 forward.",
    "Monthly totals stay in the prepared extract and are not mixed into this sample.",
    "The isolated 2004-12-10 row stays in the prepared extract and the audit. It is excluded from operating charts.",
    "2022-04-24 was duplicated with identical operating values. One copy is kept.",
    "2024-12-10 is absent. It is not imputed and it is not recorded as zero rounds.",
    "A day with zero played rounds has status zero_rounds_unknown until an operating calendar says otherwise. It is not unused sellable capacity.",
    "October zero-round days are not marked closed. The city file does not say the course was closed, and this sample does not supply a closure calendar.",
    "rounds_possible remains the city's daylight estimate. It is not tee-sheet inventory."
  )
  daily
}

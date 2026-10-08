#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr arrange group_by if_else mutate n select summarise ungroup
#' @importFrom ggplot2 aes geom_col geom_line geom_point ggplot labs
#' @importFrom ggplot2 scale_color_manual scale_y_continuous theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom lubridate as_date floor_date month parse_date_time wday year
#' @importFrom readr cols col_character read_csv
#' @importFrom rlang %||% .data abort warn
#' @importFrom scales label_comma label_dollar label_percent
#' @importFrom tibble as_tibble tibble
## usethis namespace: end
NULL

#' Column names in the City of Mesa Dobson Ranch export
#'
#' @return A character vector of the 23 catalog field names, in catalog order.
#' @export
#' @examples
#' dobson_column_names()
dobson_column_names <- function() {
  c(
    "RowID", "Date", "Calendar Year", "Fiscal Year", "Month", "Day",
    "Day of Week", "Sunrise", "Sunset", "Cart Rentals", "Cart Revenue",
    "Club Rentals", "Club Revenue", "Food and Beverage", "Green Fees",
    "Merchandise Revenue", "Range Sales", "Rounds Possible",
    "Round Booking Rate", "Rounds Played", "Total Revenue",
    "Revenue/Round", "Season"
  )
}

#' Path to the bundled Dobson Ranch example extract
#'
#' @return Absolute path to the CSV shipped in `inst/extdata`.
#' @export
#' @examples
#' dobson_rounds_example()
dobson_rounds_example <- function() {
  system.file(
    "extdata",
    "dobson_ranch_golf_rounds.csv",
    package = "golfops",
    mustWork = TRUE
  )
}

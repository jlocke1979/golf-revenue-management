#' Definitions for the participation and revenue metrics
#'
#' These definitions are accounting identities on the city extract. They are
#' not posted prices, tee-sheet utilization, or causal effects of a price change.
#'
#' @return A tibble with one row per metric.
#' @export
#' @examples
#' metric_definitions()
metric_definitions <- function() {
  tibble(
    metric = c(
      "rounds_played",
      "rounds_possible",
      "utilization",
      "open_daylight_rounds",
      "green_fee_per_played_round",
      "total_revenue_per_played_round",
      "green_fee_per_daylight_round",
      "total_revenue_per_daylight_round",
      "booking_rate"
    ),
    formula = c(
      "Rounds Played, as published",
      "Rounds Possible, as published",
      "rounds_played / rounds_possible",
      "max(rounds_possible - rounds_played, 0)",
      "green-fee revenue on rows with rounds played > 0, divided by those rounds",
      "total revenue on rows with rounds played > 0, divided by those rounds",
      "green-fee revenue, including zero-round rows, divided by rounds possible",
      "total revenue, including zero-round rows, divided by rounds possible",
      "City field Round Booking Rate, stored here as a proportion"
    ),
    use = c(
      "Count participation. Do not add monthly rows to daily rows.",
      "Daylight-based capacity estimate published by the city.",
      "Share of the daylight estimate that was played. Values above 1 mean play exceeded that estimate.",
      "Unused portion of the daylight estimate. This is not a count of open tee times.",
      "Realized green-fee yield per played round. Closest revenue figure to a price, and still not a rack rate.",
      "Facility revenue per played round. Includes food, merchandise, range, cart, and club revenue.",
      "Green-fee revenue per daylight-estimated round, filled or not.",
      "Facility revenue per daylight-estimated round. Analogous in form to revenue per available room, using the city's daylight estimate as the available base.",
      "The city's published played-over-possible rate. In this extract it matches utilization after percent conversion."
    ),
    do_not_use_as = c(
      "A booking, a tee time, or a forecast.",
      "Tee-sheet capacity, staffed capacity, or available player spaces after closures.",
      "A booking rate distinct from rounds played. The catalog defines it as percent of possible rounds played.",
      "Sellable empty tee times.",
      "A posted green fee or an estimate of price elasticity.",
      "A posted price. The city defines Revenue/Round as total revenue divided by rounds.",
      "Revenue per open tee time.",
      "A minimum acceptable price or an optimal price.",
      "Evidence that a round was reserved rather than played."
    )
  )
}

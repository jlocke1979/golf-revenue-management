#' Plot rounds or utilization from a summary
#'
#' For weekday, season, and month-of-year summaries, rounds are plotted as the
#' average per source row. For year-month, year, and fiscal-year summaries,
#' rounds are plotted as totals. Utilization is always played rounds divided
#' by the sum of daylight-estimated rounds.
#'
#' @param summary A `golfops_summary` from [summarize_golf_rounds()].
#' @param metric `rounds_played` or `utilization`.
#' @return A ggplot object.
#' @export
#' @examples
#' rounds <- prepare_dobson_rounds()
#' summary <- summarize_golf_rounds(rounds, by = "weekday", duplicates = "drop_extra")
#' plot_participation(summary)
plot_participation <- function(summary, metric = c("rounds_played", "utilization")) {
  metric <- match.arg(metric)
  summary <- .require_summary(summary)
  by <- attr(summary, "by")
  average_groups <- c("weekday", "season", "month_of_year")

  if (metric == "utilization") {
    y_label <- "Utilization of daylight-estimated rounds"
    summary$plot_y <- summary$utilization
    title <- "Share of daylight-estimated rounds played"
  } else if (by %in% average_groups) {
    y_label <- "Average rounds played per row"
    summary$plot_y <- summary$rounds_per_row
    title <- "Average rounds played"
  } else {
    y_label <- "Rounds played"
    summary$plot_y <- summary$rounds_played
    title <- "Rounds played"
  }

  p <- ggplot(summary, aes(x = .data$period, y = .data$plot_y))
  p <- if (by %in% c("year_month", "week")) {
    p + geom_line(linewidth = 0.5, color = "#0072B2") +
      geom_point(size = 1.1, color = "#0072B2")
  } else {
    p + geom_col(width = 0.72, fill = "#0072B2")
  }

  y_scale <- if (metric == "utilization") label_percent(accuracy = 1) else label_comma()
  p +
    scale_y_continuous(labels = y_scale) +
    labs(
      title = title,
      subtitle = sprintf("%s summary of %s rows", by, attr(summary, "grain")),
      x = NULL,
      y = y_label,
      caption = "Rounds possible is the city's daylight estimate, not tee-sheet capacity. Monthly totals are not mixed with daily rows."
    ) +
    theme_minimal() +
    theme(plot.caption = ggplot2::element_text(size = 8, color = "grey30"))
}

#' Plot realized revenue per round from a summary
#'
#' @param summary A `golfops_summary` from [summarize_golf_rounds()].
#' @param metric `per_played_round` uses rows with played rounds in both the
#'   numerator and the denominator. `per_daylight_round` divides revenue by
#'   the city's daylight-estimated rounds, including rows with no play.
#' @return A ggplot object.
#' @export
#' @examples
#' rounds <- prepare_dobson_rounds()
#' summary <- summarize_golf_rounds(
#'   rounds[rounds$grain == "day", ],
#'   by = "year_month",
#'   duplicates = "drop_extra"
#' )
#' plot_revenue(summary)
plot_revenue <- function(summary, metric = c("per_played_round", "per_daylight_round")) {
  metric <- match.arg(metric)
  summary <- .require_summary(summary)
  if (metric == "per_played_round") {
    green <- summary$green_fee_per_played_round
    total <- summary$total_revenue_per_played_round
    title <- "Revenue per played round"
    caption <- "Ratio of sums on rows with at least one round. Green-fee yield is not a posted price. Total revenue includes food, retail, range, cart, and club sales."
  } else {
    green <- summary$green_fee_per_daylight_round
    total <- summary$total_revenue_per_daylight_round
    title <- "Revenue per daylight-estimated round"
    caption <- "Denominator is the city's rounds-possible estimate, including unused and zero-round rows. This is not revenue per open tee time."
  }

  long <- tibble(
    period = rep(summary$period, 2),
    series = factor(
      rep(
        c("Green-fee revenue", "Total facility revenue"),
        each = nrow(summary)
      ),
      levels = c("Green-fee revenue", "Total facility revenue")
    ),
    value = c(green, total)
  )

  p <- ggplot(long, aes(x = .data$period, y = .data$value, color = .data$series))
  p <- if (attr(summary, "by") %in% c("year_month", "week")) {
    p + geom_line(linewidth = 0.5) + geom_point(size = 1.1)
  } else {
    p + geom_line(linewidth = 0.7) + geom_point(size = 2)
  }

  p +
    scale_y_continuous(labels = label_dollar()) +
    scale_color_manual(values = c(
      "Green-fee revenue" = "#0072B2",
      "Total facility revenue" = "#E69F00"
    )) +
    labs(
      title = title,
      subtitle = sprintf("%s summary of %s rows", attr(summary, "by"), attr(summary, "grain")),
      x = NULL,
      y = "Dollars per round",
      color = NULL,
      caption = caption
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

.require_summary <- function(summary) {
  required <- c(
    "period", "rounds_played", "rounds_per_row", "utilization",
    "green_fee_per_played_round", "total_revenue_per_played_round",
    "green_fee_per_daylight_round", "total_revenue_per_daylight_round"
  )
  missing <- setdiff(required, names(summary))
  if (length(missing) > 0 || is.null(attr(summary, "by"))) {
    abort(
      "Expected a summary from summarize_golf_rounds().",
      class = "golfops_input_error"
    )
  }
  summary
}

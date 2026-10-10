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

#' Daily revenue against golfers
#'
#' Each point is one day with at least one golfer. The horizontal axis is
#' golfers. Each panel is one revenue amount from the city extract: green
#' fees, food and beverage, merchandise, range, cart, club rental, and total
#' revenue. Food and beverage is a single amount. Alcohol is not separated
#' from dining.
#' A line on each panel is a straight fit. It describes the cloud of days.
#' It is not a causal effect of an extra golfer. Each panel also prints the
#' correlation of golfers with that amount, rounded to two decimals.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param year Optional calendar year. One year draws that year alone.
#' @return A ggplot object.
#' @export
plot_revenue_by_golfers <- function(data, year = NULL) {
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort("Revenue scatters use daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before plotting revenue against golfers.",
      class = "golfops_duplicate_error"
    )
  }
  played <- data[!is.na(data$rounds_played) & data$rounds_played > 0, , drop = FALSE]
  if (!is.null(year)) {
    if (length(year) != 1 || is.na(year)) {
      abort("`year` must be a single calendar year.", class = "golfops_input_error")
    }
    played <- played[played$calendar_year == as.integer(year), , drop = FALSE]
    if (nrow(played) == 0) {
      abort(
        paste0("No played days in ", year, "."),
        class = "golfops_input_error"
      )
    }
  }
  categories <- c(
    "Total revenue" = "total_revenue",
    "Green fees" = "green_fee_revenue",
    "Food and beverage" = "food_beverage_revenue",
    "Merchandise" = "merchandise_revenue",
    "Range" = "range_revenue",
    "Cart" = "cart_revenue",
    "Club rental" = "club_revenue"
  )
  pieces <- lapply(names(categories), function(label) {
    data.frame(
      golfers = played$rounds_played,
      revenue = played[[categories[[label]]]],
      category = label,
      year = as.character(played$calendar_year),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  long <- long[!is.na(long$revenue), , drop = FALSE]
  long$category <- factor(long$category, levels = names(categories))
  years <- sort(unique(long$year))
  long$year <- factor(long$year, levels = years)
  palette <- c(
    "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
    "#66A61E", "#E6AB02", "#A6761D", "#666666"
  )
  year_colors <- stats::setNames(palette[seq_along(years)], years)
  correlations <- do.call(rbind, lapply(split(long, long$category), function(block) {
    data.frame(
      category = block$category[1],
      label = sprintf(
        "r = %.2f",
        stats::cor(block$golfers, block$revenue, use = "complete.obs")
      ),
      stringsAsFactors = FALSE
    )
  }))
  correlations$category <- factor(correlations$category, levels = levels(long$category))
  one_year <- length(years) == 1
  span <- range(played$date)
  chart <- ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$golfers, y = .data$revenue, color = .data$year)
  ) +
    ggplot2::geom_point(alpha = 0.45, size = 0.9) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = .data$golfers, y = .data$revenue),
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      inherit.aes = FALSE,
      color = "#333333",
      linewidth = 0.4
    ) +
    ggplot2::geom_label(
      data = correlations,
      ggplot2::aes(x = -Inf, y = Inf, label = .data$label),
      inherit.aes = FALSE,
      hjust = -0.08,
      vjust = 1.15,
      size = 3.1,
      color = "grey20",
      fill = "white",
      label.size = 0,
      label.padding = grid::unit(0.15, "lines")
    ) +
    ggplot2::facet_wrap(~category, scales = "free_y", ncol = 3) +
    ggplot2::scale_x_continuous(labels = label_comma()) +
    ggplot2::scale_y_continuous(labels = label_dollar()) +
    ggplot2::scale_color_manual(values = year_colors, name = "Year") +
    ggplot2::labs(
      title = if (one_year) paste("Daily revenue and golfers,", years) else "Daily revenue and golfers",
      subtitle = sprintf(
        "%s through %s",
        format(span[1], "%b %d, %Y"),
        format(span[2], "%b %d, %Y")
      ),
      x = "Golfers",
      y = NULL,
      caption = "Days with at least one golfer. The number on each panel is the correlation of golfers with that amount. Food and beverage is one published amount; alcohol is not split from dining. Total revenue is the city's daily total. The line is a straight fit to the days in that panel. It is not a causal effect of an extra golfer. Range dollars on some month-end days are large, and the extract does not say whether they were earned that day."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = if (one_year) "none" else "bottom",
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
  chart
}

#' Annual revenue by type
#'
#' One bar per calendar year. Each panel is one revenue amount from the city
#' extract. Total revenue is the last panel. A year that does not run from
#' January 1 through December 31 is marked with an asterisk. The bar is the
#' sum of the daily amounts, including days with no golfers.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @return A ggplot object.
#' @export
plot_annual_revenue <- function(data) {
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort("Annual revenue uses daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before totaling annual revenue.",
      class = "golfops_duplicate_error"
    )
  }
  categories <- c(
    "Green fees" = "green_fee_revenue",
    "Food and beverage" = "food_beverage_revenue",
    "Merchandise" = "merchandise_revenue",
    "Range" = "range_revenue",
    "Cart" = "cart_revenue",
    "Club rental" = "club_revenue",
    "Total revenue" = "total_revenue"
  )
  years <- sort(unique(data$calendar_year))
  pieces <- lapply(names(categories), function(label) {
    rows <- lapply(years, function(year) {
      block <- data[data$calendar_year == year, , drop = FALSE]
      year_start <- as.Date(sprintf("%s-01-01", year))
      year_end <- as.Date(sprintf("%s-12-31", year))
      partial <- min(block$date) > year_start || max(block$date) < year_end
      data.frame(
        year = if (partial) paste0(year, "*") else as.character(year),
        revenue = sum(block[[categories[[label]]]], na.rm = TRUE),
        category = label,
        partial = partial,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
  annual <- do.call(rbind, pieces)
  annual$year <- factor(annual$year, levels = unique(annual$year))
  annual$category <- factor(annual$category, levels = names(categories))
  annual$span <- ifelse(annual$partial, "Half year", "Full year")
  ggplot2::ggplot(
    annual,
    ggplot2::aes(x = .data$year, y = .data$revenue, fill = .data$span)
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::facet_wrap(~category, ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = label_dollar()) +
    ggplot2::scale_fill_manual(
      values = c("Full year" = "#1B9E77", "Half year" = "#A6D9C9"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "Annual revenue",
      x = NULL,
      y = "Revenue",
      caption = "Each bar is the sum of daily amounts, including days with no golfers. An asterisk marks a year that does not cover January 1 through December 31. Total revenue is the city's daily total, drawn last."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' Revenue per golfer by year
#'
#' Each value is the sum of a revenue amount divided by the sum of golfers,
#' on days with at least one golfer. It is an average recorded amount per
#' golfer, not a posted price. A year that does not run from January 1
#' through December 31 is marked with an asterisk. A blank category amount
#' is left out of the numerator. Golfers on those days stay in the
#' denominator.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @return A data frame with one row per revenue type and year.
#' @export
revenue_per_golfer <- function(data) {
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort("Revenue per golfer uses daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before averaging revenue per golfer.",
      class = "golfops_duplicate_error"
    )
  }
  played <- data[!is.na(data$rounds_played) & data$rounds_played > 0, , drop = FALSE]
  if (nrow(played) == 0) {
    abort(
      "Revenue per golfer needs at least one day with a golfer.",
      class = "golfops_input_error"
    )
  }
  categories <- c(
    "Total revenue" = "total_revenue",
    "Green fees" = "green_fee_revenue",
    "Food and beverage" = "food_beverage_revenue",
    "Merchandise" = "merchandise_revenue",
    "Range" = "range_revenue",
    "Cart" = "cart_revenue",
    "Club rental" = "club_revenue"
  )
  years <- sort(unique(played$calendar_year))
  pieces <- lapply(names(categories), function(label) {
    rows <- lapply(years, function(year) {
      block <- played[played$calendar_year == year, , drop = FALSE]
      span <- data[data$calendar_year == year, , drop = FALSE]
      year_start <- as.Date(sprintf("%s-01-01", year))
      year_end <- as.Date(sprintf("%s-12-31", year))
      partial <- min(span$date) > year_start || max(span$date) < year_end
      data.frame(
        category = label,
        calendar_year = year,
        year_label = if (partial) paste0(year, "*") else as.character(year),
        dollars_per_golfer = sum(block[[categories[[label]]]], na.rm = TRUE) /
          sum(block$rounds_played),
        partial = partial,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
  out <- do.call(rbind, pieces)
  out$category <- factor(out$category, levels = names(categories))
  out$year_label <- factor(out$year_label, levels = unique(out$year_label))
  rownames(out) <- NULL
  out
}

#' Revenue mix per golfer
#'
#' One bar per calendar year. Each segment is one revenue type. `dollars`
#' stacks the average recorded amount per golfer. `share` scales each year
#' to 100 percent. Total revenue is the height of the dollar bar and is not
#' drawn as its own segment.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param scale `"dollars"` or `"share"`.
#' @return A ggplot object.
#' @export
plot_revenue_mix <- function(data, scale = c("dollars", "share")) {
  scale <- match.arg(scale)
  prices <- revenue_per_golfer(data)
  prices <- prices[prices$category != "Total revenue", , drop = FALSE]
  prices$category <- factor(prices$category, levels = levels(prices$category))
  fills <- c(
    "Green fees" = "#1B9E77",
    "Food and beverage" = "#D95F02",
    "Merchandise" = "#7570B3",
    "Range" = "#E6AB02",
    "Cart" = "#E7298A",
    "Club rental" = "#A6761D"
  )
  y_label <- if (scale == "share") "Share of revenue" else "Dollars per golfer"
  title <- if (scale == "share") "Mix of revenue per golfer" else "Revenue per golfer"
  ggplot2::ggplot(
    prices,
    ggplot2::aes(
      x = .data$year_label,
      y = .data$dollars_per_golfer,
      fill = .data$category
    )
  ) +
    ggplot2::geom_col(
      width = 0.72,
      position = if (scale == "share") "fill" else "stack"
    ) +
    ggplot2::scale_y_continuous(
      labels = if (scale == "share") label_percent() else label_dollar(),
      expand = if (scale == "share") c(0, 0) else ggplot2::waiver()
    ) +
    ggplot2::scale_fill_manual(values = fills, name = NULL) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label,
      caption = "Days with at least one golfer. An asterisk marks a year that does not cover January 1 through December 31. Each segment is an average recorded amount per golfer, not a posted price."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
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

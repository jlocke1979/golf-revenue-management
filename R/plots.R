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

#' Daily share of the daylight estimate
#'
#' Each played day is golfers divided by the city's daylight estimate for
#' that day. The box is the middle half of those days. Whiskers follow the
#' usual rule, one and a half times the height of the box. A day is marked
#' when it sits at least 5 percentage points beyond a whisker. The date is
#' the furthest such day in that month or weekday. `time_of_day` draws an
#' empty panel: the Dobson extract has no tee time.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param by `month_of_year`, `weekday` (or `day_of_week`), or `time_of_day`.
#' @return A ggplot object.
#' @export
#' @examples
#' daily <- dobson_operating_daily()
#' plot_daylight_share(daily, "month_of_year")
#' plot_daylight_share(daily, "time_of_day")
plot_daylight_share <- function(
    data,
    by = c("month_of_year", "weekday", "day_of_week", "time_of_day")) {
  by <- match.arg(by)
  if (by == "time_of_day") {
    return(.daylight_share_placeholder())
  }
  if (by == "day_of_week") {
    by <- "weekday"
  }
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort(
      "Daylight-share charts use daily rows.",
      class = "golfops_input_error"
    )
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before plotting the daylight share.",
      class = "golfops_duplicate_error"
    )
  }
  grouping <- if (by == "month_of_year") "month" else "weekday"
  if (!grouping %in% names(data)) {
    abort(
      paste0("Daylight-share charts need a ", grouping, " column."),
      class = "golfops_input_error"
    )
  }
  played <- data[
    !is.na(data$rounds_played) & data$rounds_played > 0 &
      !is.na(data$rounds_possible) & data$rounds_possible > 0,
    ,
    drop = FALSE
  ]
  if (nrow(played) == 0) {
    abort(
      "Daylight-share charts need at least one played day.",
      class = "golfops_input_error"
    )
  }
  levels <- if (by == "month_of_year") month_names() else weekday_names()
  played$period <- factor(played[[grouping]], levels = levels)
  played <- played[!is.na(played$period), , drop = FALSE]
  stats <- .daylight_share_stats(
    period = played$period,
    share = played$rounds_played / played$rounds_possible,
    date = played$date
  )
  subtitle <- if (by == "month_of_year") "month_of_year" else "day_of_week"
  chart <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = stats$boxes,
      ggplot2::aes(
        x = .data$period,
        ymin = .data$ymin,
        lower = .data$lower,
        middle = .data$middle,
        upper = .data$upper,
        ymax = .data$ymax
      ),
      stat = "identity",
      width = 0.62,
      fill = "#D0E4F5",
      color = "#0072B2",
      linewidth = 0.4
    )
  if (nrow(stats$outliers) > 0) {
    chart <- chart +
      ggplot2::geom_point(
        data = stats$outliers,
        ggplot2::aes(x = .data$period, y = .data$share),
        color = "#222222",
        size = 1.3
      )
  }
  if (nrow(stats$labels) > 0) {
    chart <- chart +
      ggplot2::geom_label(
        data = .daylight_share_label_positions(stats$labels, nlevels(played$period)),
        ggplot2::aes(
          x = .data$period,
          y = .data$label_y,
          label = .data$label,
          hjust = .data$hjust
        ),
        vjust = 0.5,
        size = 2.5,
        color = "grey20",
        fill = "white",
        label.size = 0.15,
        label.padding = grid::unit(0.12, "lines"),
        show.legend = FALSE
      )
  }
  chart +
    ggplot2::scale_y_continuous(
      labels = label_percent(accuracy = 1),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0.03, 0.16))
    ) +
    ggplot2::labs(
      title = "Share of daylight-estimated rounds played",
      subtitle = subtitle,
      x = NULL,
      y = "Share of daylight-estimated rounds",
      caption = "The date is the furthest day at least 5 percentage points past the whisker."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (by == "month_of_year") 40 else 0,
        hjust = if (by == "month_of_year") 1 else 0.5,
        vjust = if (by == "month_of_year") 1 else 0.5
      ),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30", hjust = 0)
    )
}

.daylight_share_placeholder <- function() {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1,
      fill = "grey85",
      color = NA
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.55,
      label = "Time of day",
      color = "grey35",
      size = 4.2,
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.42,
      label = "Empty until the extract has tee times.",
      color = "grey40",
      size = 3.4
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = NULL, expand = c(0, 0)) +
    ggplot2::labs(
      title = "Share of daylight-estimated rounds played",
      subtitle = "time_of_day",
      x = NULL,
      y = NULL,
      caption = "Dobson Ranch daily rows have a date and a golfer count. They have no start time, so morning and afternoon stay in one day."
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "grey85", color = NA),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30", hjust = 0)
    )
}

# High dates sit above the point. Low dates sit below it, unless that
# would crowd the axis, in which case they sit just above the point.
# Edge months grow inward so the date stays inside the panel.
.daylight_share_label_positions <- function(labels, n_periods) {
  labels <- labels[order(as.integer(labels$period)), , drop = FALSE]
  pos <- as.integer(labels$period)
  above <- labels$side == "high" | labels$share < 0.22
  labels$label_y <- labels$share + ifelse(above, 0.055, -0.055)
  labels$hjust <- ifelse(pos == 1, 0, ifelse(pos == n_periods, 1, 0.5))
  if (nrow(labels) > 1) {
    for (i in seq_len(nrow(labels) - 1L)) {
      adjacent <- pos[i + 1L] - pos[i] == 1
      close <- abs(labels$label_y[i + 1L] - labels$label_y[i]) < 0.07
      if (adjacent && close) {
        labels$label_y[i + 1L] <- labels$label_y[i + 1L] + ifelse(above[i + 1L], 0.09, -0.09)
      }
    }
  }
  labels
}

# Box is the interquartile range. Whiskers use the usual 1.5 rule.
# A day is an outlier only when it clears that whisker by `gap`
# (5 percentage points). The label is the furthest outlier in the group.
.daylight_share_stats <- function(period, share, date, gap = 0.05, coef = 1.5) {
  groups <- split(seq_along(period), period, drop = TRUE)
  boxes <- lapply(names(groups), function(name) {
    idx <- groups[[name]]
    x <- share[idx]
    q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
    iqr <- q[3] - q[1]
    fence_lo <- q[1] - coef * iqr
    fence_hi <- q[3] + coef * iqr
    past_hi <- x - fence_hi
    past_lo <- fence_lo - x
    past <- pmax(past_hi, past_lo, 0)
    outlier <- past >= gap
    inliers <- x[!outlier]
    if (length(inliers) == 0) {
      inliers <- x
    }
    side <- rep("low", sum(outlier))
    if (any(outlier)) {
      side[past_hi[outlier] >= past_lo[outlier]] <- "high"
      outliers <- data.frame(
        period = name,
        date = as.Date(date[idx][outlier]),
        share = x[outlier],
        past = past[outlier],
        side = side,
        stringsAsFactors = FALSE
      )
    } else {
      outliers <- data.frame(
        period = character(),
        date = as.Date(character()),
        share = numeric(),
        past = numeric(),
        side = character(),
        stringsAsFactors = FALSE
      )
    }
    list(
      box = data.frame(
        period = name,
        ymin = min(inliers),
        lower = q[1],
        middle = q[2],
        upper = q[3],
        ymax = max(inliers),
        stringsAsFactors = FALSE
      ),
      outliers = outliers
    )
  })
  box_rows <- do.call(rbind, lapply(boxes, `[[`, "box"))
  outlier_rows <- do.call(rbind, lapply(boxes, `[[`, "outliers"))
  if (is.null(outlier_rows) || nrow(outlier_rows) == 0) {
    outlier_rows <- data.frame(
      period = character(),
      date = as.Date(character()),
      share = numeric(),
      past = numeric(),
      side = character(),
      stringsAsFactors = FALSE
    )
  }
  box_rows$period <- factor(box_rows$period, levels = levels(period))
  outlier_rows$period <- factor(outlier_rows$period, levels = levels(period))
  label_rows <- outlier_rows[0, , drop = FALSE]
  if (nrow(outlier_rows) > 0) {
    label_rows <- do.call(rbind, lapply(split(outlier_rows, outlier_rows$period, drop = TRUE), function(block) {
      block[which.max(block$past), , drop = FALSE]
    }))
    label_rows$label <- sprintf(
      "%s %d, %d",
      format(label_rows$date, "%b"),
      as.integer(format(label_rows$date, "%d")),
      as.integer(format(label_rows$date, "%Y"))
    )
    label_rows$period <- factor(label_rows$period, levels = levels(period))
  }
  list(boxes = box_rows, outliers = outlier_rows, labels = label_rows)
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

# Revenue chart colors. Edit a value here to change it on the annual bars,
# the stacked mixes, the dollars-per-golfer charts, and a one-year scatter.
# A half year is a lighter tint of the same color.
.revenue_colors <- function() {
  c(
    "Green fees" = "#1B9E77",
    "Cart" = "#E7298A",
    "Food and beverage" = "#D95F02",
    "Range" = "#E6AB02",
    "Merchandise" = "#7570B3",
    "Club rental" = "#A6761D",
    "Total revenue" = "#4D4D4D",
    "Golfers" = "#333333"
  )
}

.tint_hex <- function(hex, amount = 0.55) {
  amount <- rep(amount, length.out = length(hex))
  rgb <- grDevices::col2rgb(hex) / 255
  mixed <- sweep(rgb, 2, 1 - amount, `*`)
  mixed <- sweep(mixed, 2, amount, `+`)
  grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ])
}

.span_colors <- function(name) {
  base <- .revenue_colors()[[name]]
  c("Full year" = base, "Half year" = .tint_hex(base))
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
#' correlation of golfers with that amount, rounded to two decimals. A chart
#' of one year draws each panel in that revenue type's color.
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
  year_palette <- c(
    "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
    "#66A61E", "#E6AB02", "#A6761D", "#666666"
  )
  year_colors <- stats::setNames(year_palette[seq_along(years)], years)
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
  color_scale <- if (one_year) {
    ggplot2::scale_color_manual(values = .revenue_colors(), guide = "none")
  } else {
    ggplot2::scale_color_manual(values = year_colors, name = "Year")
  }
  chart <- ggplot2::ggplot(
    long,
    if (one_year) {
      ggplot2::aes(x = .data$golfers, y = .data$revenue, color = .data$category)
    } else {
      ggplot2::aes(x = .data$golfers, y = .data$revenue, color = .data$year)
    }
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
    color_scale +
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

#' Golfers by year
#'
#' One point per calendar year, joined by a line. The value is the sum of
#' daily rounds. In the city extract a round is one golfer who played the
#' course. A range-only visit is not included. A year that does not run from
#' January 1 through December 31 is marked with an asterisk.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @return A ggplot object.
#' @export
plot_annual_golfers <- function(data) {
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort("Annual golfers use daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before totaling annual golfers.",
      class = "golfops_duplicate_error"
    )
  }
  years <- sort(unique(data$calendar_year))
  rows <- lapply(years, function(year) {
    block <- data[data$calendar_year == year, , drop = FALSE]
    year_start <- as.Date(sprintf("%s-01-01", year))
    year_end <- as.Date(sprintf("%s-12-31", year))
    partial <- min(block$date) > year_start || max(block$date) < year_end
    data.frame(
      year = if (partial) paste0(year, "*") else as.character(year),
      golfers = sum(block$rounds_played, na.rm = TRUE),
      partial = partial,
      stringsAsFactors = FALSE
    )
  })
  annual <- do.call(rbind, rows)
  annual$year <- factor(annual$year, levels = annual$year)
  annual$span <- ifelse(annual$partial, "Half year", "Full year")
  ggplot2::ggplot(annual, ggplot2::aes(x = .data$year, y = .data$golfers, group = 1)) +
    ggplot2::geom_line(color = "#333333", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = .data$span), size = 2.6) +
    ggplot2::scale_y_continuous(labels = label_comma()) +
    ggplot2::scale_color_manual(values = .span_colors("Golfers"), name = NULL) +
    ggplot2::labs(
      title = "Golfers by year",
      x = NULL,
      y = "Golfers",
      caption = "Each point is the sum of daily rounds. A round is one golfer who played the course. An asterisk marks a year that does not cover January 1 through December 31. Range-only visits are not in this count."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' Cart rentals by year
#'
#' One point per calendar year, joined by a line. The value is the sum of
#' daily cart rentals. A year that does not run from January 1 through
#' December 31 is marked with an asterisk. Blank cart counts are omitted
#' from the total.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @return A ggplot object.
#' @export
plot_annual_carts <- function(data) {
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort("Annual cart rentals use daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before totaling annual cart rentals.",
      class = "golfops_duplicate_error"
    )
  }
  years <- sort(unique(data$calendar_year))
  rows <- lapply(years, function(year) {
    block <- data[data$calendar_year == year, , drop = FALSE]
    year_start <- as.Date(sprintf("%s-01-01", year))
    year_end <- as.Date(sprintf("%s-12-31", year))
    partial <- min(block$date) > year_start || max(block$date) < year_end
    data.frame(
      year = if (partial) paste0(year, "*") else as.character(year),
      carts = sum(block$cart_rentals, na.rm = TRUE),
      partial = partial,
      stringsAsFactors = FALSE
    )
  })
  annual <- do.call(rbind, rows)
  annual$year <- factor(annual$year, levels = annual$year)
  annual$span <- ifelse(annual$partial, "Half year", "Full year")
  ggplot2::ggplot(annual, ggplot2::aes(x = .data$year, y = .data$carts, group = 1)) +
    ggplot2::geom_line(color = "#333333", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = .data$span), size = 2.6) +
    ggplot2::scale_y_continuous(labels = label_comma()) +
    ggplot2::scale_color_manual(values = .span_colors("Cart"), name = NULL) +
    ggplot2::labs(
      title = "Cart rentals by year",
      x = NULL,
      y = "Cart rentals",
      caption = "Each point is the sum of daily cart rentals. An asterisk marks a year that does not cover January 1 through December 31. Cart counts are blank through September 30, 2021, so that point is October through December only."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' Annual revenue by type
#'
#' One bar per calendar year. Each panel is one revenue amount from the city
#' extract. Total revenue is the last panel. Each panel uses that revenue
#' type's color. A year that does not run from January 1 through December 31
#' is marked with an asterisk and drawn in a lighter tint of the same color.
#' The bar is the sum of the daily amounts, including days with no golfers.
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
  annual$bar_color <- .tint_hex(
    unname(.revenue_colors()[as.character(annual$category)]),
    amount = ifelse(annual$partial, 0.55, 0)
  )
  ggplot2::ggplot(
    annual,
    ggplot2::aes(x = .data$year, y = .data$revenue, fill = .data$bar_color)
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::facet_wrap(~category, ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = label_dollar()) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      title = "Annual revenue",
      x = NULL,
      y = "Revenue",
      caption = "Each bar is the sum of daily amounts, including days with no golfers. Each panel uses that revenue type's color. An asterisk marks a year that does not cover January 1 through December 31, drawn in a lighter tint. Total revenue is the city's daily total, drawn last."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' Annual revenue mix
#'
#' One bar per calendar year. Each segment is one revenue type from the city
#' extract. `dollars` stacks the sum of daily amounts, including days with no
#' golfers. `share` scales each year to 100 percent. Total revenue is the
#' height of the dollar bar and is not drawn as its own segment. A year that
#' does not run from January 1 through December 31 is marked with an asterisk.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param scale `"dollars"` or `"share"`.
#' @return A ggplot object.
#' @export
plot_annual_revenue_mix <- function(data, scale = c("dollars", "share")) {
  scale <- match.arg(scale)
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort("Annual revenue mix uses daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before totaling the annual revenue mix.",
      class = "golfops_duplicate_error"
    )
  }
  categories <- c(
    "Green fees" = "green_fee_revenue",
    "Cart" = "cart_revenue",
    "Food and beverage" = "food_beverage_revenue",
    "Range" = "range_revenue",
    "Merchandise" = "merchandise_revenue",
    "Club rental" = "club_revenue"
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
  y_label <- if (scale == "share") "Share of annual revenue" else "Revenue"
  title <- if (scale == "share") "Share of annual revenue" else "Annual revenue by type"
  ggplot2::ggplot(
    annual,
    ggplot2::aes(x = .data$year, y = .data$revenue, fill = .data$category)
  ) +
    ggplot2::geom_col(
      width = 0.72,
      position = if (scale == "share") {
        ggplot2::position_fill(reverse = TRUE)
      } else {
        ggplot2::position_stack(reverse = TRUE)
      }
    ) +
    ggplot2::scale_y_continuous(
      labels = if (scale == "share") label_percent() else label_dollar(),
      expand = if (scale == "share") c(0, 0) else ggplot2::waiver()
    ) +
    ggplot2::scale_fill_manual(
      values = .revenue_mix_fills(),
      breaks = names(categories),
      name = NULL
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label,
      caption = "Each bar is the sum of daily amounts, including days with no golfers. An asterisk marks a year that does not cover January 1 through December 31. Total revenue is the height of the dollar bar and is not its own segment. Cart amounts are blank through September 30, 2021."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

.revenue_mix_fills <- function() {
  .revenue_colors()[c(
    "Green fees",
    "Cart",
    "Food and beverage",
    "Range",
    "Merchandise",
    "Club rental"
  )]
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

#' Revenue per available round by year
#'
#' Each value is the sum of a revenue amount divided by the sum of
#' daylight-estimated rounds, including days with no golfers. This is the
#' annual form of revenue per available round. The papers call the tee-time
#' version RevPATR. The denominator here is the city's daylight estimate.
#' It is not a count of tee times. A year that does not run from January 1
#' through December 31 is marked with an asterisk.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @return A data frame with one row per revenue type and year.
#' @export
revenue_per_available_round <- function(data) {
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort(
      "Revenue per available round uses daily rows.",
      class = "golfops_input_error"
    )
  }
  if (any(duplicated(data$date))) {
    abort(
      "Drop duplicate dates before dividing revenue by available rounds.",
      class = "golfops_duplicate_error"
    )
  }
  available <- data[!is.na(data$rounds_possible) & data$rounds_possible > 0, , drop = FALSE]
  if (nrow(available) == 0) {
    abort(
      "Revenue per available round needs at least one day with a daylight estimate.",
      class = "golfops_input_error"
    )
  }
  categories <- c(
    "Total revenue" = "total_revenue",
    "Green fees" = "green_fee_revenue"
  )
  years <- sort(unique(available$calendar_year))
  pieces <- lapply(names(categories), function(label) {
    rows <- lapply(years, function(year) {
      block <- available[available$calendar_year == year, , drop = FALSE]
      span <- data[data$calendar_year == year, , drop = FALSE]
      year_start <- as.Date(sprintf("%s-01-01", year))
      year_end <- as.Date(sprintf("%s-12-31", year))
      partial <- min(span$date) > year_start || max(span$date) < year_end
      data.frame(
        category = label,
        calendar_year = year,
        year_label = if (partial) paste0(year, "*") else as.character(year),
        dollars_per_available_round = sum(block[[categories[[label]]]], na.rm = TRUE) /
          sum(block$rounds_possible),
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

#' RevPATR by year
#'
#' One line for total revenue and one line for green fees. Each point is
#' that year's revenue divided by the daylight estimate of rounds. See
#' [revenue_per_available_round()].
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @return A ggplot object.
#' @export
plot_revpatr <- function(data) {
  annual <- revenue_per_available_round(data)
  colors <- .revenue_colors()[c("Total revenue", "Green fees")]
  ggplot2::ggplot(
    annual,
    ggplot2::aes(
      x = .data$year_label,
      y = .data$dollars_per_available_round,
      color = .data$category,
      group = .data$category
    )
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_y_continuous(labels = label_dollar()) +
    ggplot2::scale_color_manual(values = colors, name = NULL) +
    ggplot2::labs(
      title = "RevPATR",
      x = NULL,
      y = "Dollars per available round",
      caption = "Each point divides the year's revenue by the sum of the daylight estimate of rounds, including days with no golfers. That estimate stands in for rounds the course could have sold. It is not a count of tee times. An asterisk marks a year that does not cover January 1 through December 31."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' Dollars per golfer, stacked
#'
#' One bar per calendar year. Each segment is one revenue type. `dollars`
#' stacks the average recorded amount per golfer. `share` scales each year
#' to 100 percent of those dollars per golfer. This is not the mix of total
#' revenue; see [plot_annual_revenue_mix()]. Total revenue is the height of
#' the dollar bar and is not drawn as its own segment.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param scale `"dollars"` or `"share"`.
#' @return A ggplot object.
#' @export
plot_revenue_mix <- function(data, scale = c("dollars", "share")) {
  scale <- match.arg(scale)
  prices <- revenue_per_golfer(data)
  prices <- prices[prices$category != "Total revenue", , drop = FALSE]
  stack_order <- c(
    "Green fees",
    "Cart",
    "Food and beverage",
    "Range",
    "Merchandise",
    "Club rental"
  )
  prices$category <- factor(prices$category, levels = stack_order)
  y_label <- if (scale == "share") "Share of dollars per golfer" else "Dollars per golfer"
  title <- if (scale == "share") "Share of dollars per golfer" else "Dollars per golfer by type"
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
      position = if (scale == "share") {
        ggplot2::position_fill(reverse = TRUE)
      } else {
        ggplot2::position_stack(reverse = TRUE)
      }
    ) +
    ggplot2::scale_y_continuous(
      labels = if (scale == "share") label_percent() else label_dollar(),
      expand = if (scale == "share") c(0, 0) else ggplot2::waiver()
    ) +
    ggplot2::scale_fill_manual(
      values = .revenue_mix_fills(),
      breaks = stack_order,
      name = NULL
    ) +
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

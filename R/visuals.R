#' January–December values by year for the operating sample
#'
#' Averages use observed rows only. Missing dates are not filled with zero.
#' Rounds and daylight utilization use days with played rounds, so an unknown
#' zero-round stretch does not look like a month of empty sellable tee times.
#' Revenue averages keep those days, because the facility still recorded sales.
#' A month is partial when the sample span includes a date in that month that
#' has no row, or when the month contains a zero-round day of unknown status.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param metric `rounds`, `green_fee_revenue`, `total_revenue`, or
#'   `utilization`.
#' @return A tibble with one row per year and month present in the sample.
#' @export
seasonal_overlay_data <- function(
    data,
    metric = c("rounds", "green_fee_revenue", "total_revenue", "utilization")) {
  metric <- match.arg(metric)
  data <- .require_rounds(data)
  if (any(data$grain == "month", na.rm = TRUE)) {
    abort("Seasonal overlays use daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(data$date))) {
    abort("Drop duplicate dates before building a seasonal overlay.", class = "golfops_duplicate_error")
  }
  if (!"operating_status" %in% names(data)) {
    data <- apply_operating_capacity(data)
  }
  calendar <- utilization_calendar(data)
  missing <- calendar$date[calendar$operating_status == "missing"]
  span_start <- min(data$date)
  span_end <- max(data$date)
  data$month_start <- floor_date(data$date, "month")
  pieces <- split(data, data$month_start)
  rows <- lapply(pieces, function(block) {
    played <- block$operating_status %in% c("played", "above_capacity", "above_daylight_estimate")
    month_start <- block$month_start[1]
    month_end <- as.Date(format(month_start + 32, "%Y-%m-01")) - 1
    in_span <- seq(max(month_start, span_start), min(month_end, span_end), by = "day")
    unknown_zero <- any(block$operating_status == "zero_rounds_unknown")
    partial <- unknown_zero || any(in_span %in% missing) || nrow(block) < length(in_span)
    value <- switch(
      metric,
      rounds = if (!any(played)) NA_real_ else mean(block$rounds_played[played]),
      green_fee_revenue = mean(block$green_fee_revenue),
      total_revenue = mean(block$total_revenue),
      utilization = {
        denom <- sum(block$rounds_possible[played], na.rm = TRUE)
        if (!any(played) || denom <= 0) NA_real_ else sum(block$rounds_played[played]) / denom
      }
    )
    data.frame(
      month_start = month_start,
      calendar_year = block$calendar_year[1],
      month = month_names()[month(month_start)],
      value = value,
      partial = partial,
      n_rows = nrow(block),
      n_played_days = sum(played),
      stringsAsFactors = FALSE
    )
  })
  out <- as_tibble(do.call(rbind, rows))
  out$month <- factor(out$month, levels = month_names())
  out$year_label <- vapply(out$calendar_year, function(year) {
    .operating_year_label(year, span_start, span_end, missing)
  }, character(1))
  out$metric <- metric
  out[order(out$month_start), , drop = FALSE]
}

#' Plot one seasonal series with a line for each year
#'
#' Open points are partial months. The year label names a partial year. The
#' color scale is the year, not a second measure.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param metric Passed to [seasonal_overlay_data()].
#' @return A ggplot object.
#' @export
plot_seasonal_overlay <- function(
    data,
    metric = c("rounds", "green_fee_revenue", "total_revenue", "utilization")) {
  metric <- match.arg(metric)
  overlay <- seasonal_overlay_data(data, metric)
  y_label <- switch(
    metric,
    rounds = "Average rounds on days with play",
    green_fee_revenue = "Average daily green-fee revenue",
    total_revenue = "Average daily facility revenue",
    utilization = "Daylight utilization on days with play"
  )
  title <- switch(
    metric,
    rounds = "Daily rounds by month, each year overlaid",
    green_fee_revenue = "Green-fee revenue by month, each year overlaid",
    total_revenue = "Facility revenue by month, each year overlaid",
    utilization = "Daylight utilization by month, each year overlaid"
  )
  caption <- if (metric == "rounds") {
    "Average of days with play. Open points mark a missing date or a zero-round day of unknown status. Those days are left out of the average rather than treated as unused sellable capacity."
  } else if (metric == "utilization") {
    "Ratio of sums on days with play, not capped at 100 percent. Open points mark a missing date or a zero-round day of unknown status."
  } else {
    "Average of observed days, including revenue on zero-round days. Open points mark a missing date or a zero-round day of unknown status. This is not revenue per golfer."
  }
  y_scale <- if (metric == "utilization") {
    ggplot2::scale_y_continuous(labels = label_percent(accuracy = 1))
  } else if (metric == "rounds") {
    ggplot2::scale_y_continuous(labels = label_comma())
  } else {
    ggplot2::scale_y_continuous(labels = label_dollar())
  }
  year_colors <- c(
    "#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#000000"
  )
  overlay$month_status <- factor(
    ifelse(overlay$partial, "Partial or unknown-status days", "Complete observed month"),
    levels = c("Complete observed month", "Partial or unknown-status days")
  )
  labels <- unique(overlay$year_label[order(overlay$calendar_year)])
  palette <- stats::setNames(year_colors[seq_along(labels)], labels)
  ggplot2::ggplot(
    overlay,
    ggplot2::aes(
      x = .data$month,
      y = .data$value,
      color = .data$year_label,
      group = .data$year_label
    )
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$month_status), size = 2) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_shape_manual(values = c(16, 1), name = NULL) +
    y_scale +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label,
      color = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' Capacity or utilization for each calendar day
#'
#' One panel per year. Capacity uses a January–December axis so each year
#' lines up. Utilization is rounds played divided by that capacity, drawn
#' only on days that belong on the utilization scale. A zero-round day of
#' unknown status and a missing date leave a gap. The axis runs from 0 to
#' 120 percent. A cross marks a day above 120 percent.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param metric `capacity` or `utilization`.
#' @param calendar Optional [operating_calendar()].
#' @return A ggplot object.
#' @export
plot_calendar_day <- function(
    data,
    metric = c("capacity", "utilization"),
    calendar = NULL) {
  metric <- match.arg(metric)
  cal <- utilization_calendar(data, calendar)
  span_start <- min(cal$date)
  span_end <- max(cal$date)
  missing <- cal$date[cal$operating_status == "missing"]
  cal$year_label <- vapply(cal$calendar_year, function(year) {
    .operating_year_label(year, span_start, span_end, missing)
  }, character(1))
  if (metric == "capacity") {
    cal$plot_x <- as.Date(paste0("2000-", format(cal$date, "%m-%d")))
    cal$plot_y <- cal$capacity_rounds
    title <- "Capacity by calendar day"
    y_label <- "Rounds of capacity"
    y_scale <- ggplot2::scale_y_continuous(labels = label_comma())
    x_scale <- ggplot2::scale_x_date(
      limits = as.Date(c("2000-01-01", "2000-12-31")),
      breaks = seq(as.Date("2000-01-01"), as.Date("2000-12-01"), by = "month"),
      date_labels = "%b",
      expand = ggplot2::expansion(mult = 0.02)
    )
    facet_scales <- "fixed"
    caption <- "Resolved capacity for each day in the sample. With no operating calendar, this is the daylight estimate."
  } else {
    cal$plot_x <- cal$date
    cal$plot_y <- ifelse(cal$on_utilization_scale, cal$scale_utilization, NA_real_)
    title <- "Utilization by calendar day"
    y_label <- "Utilization"
    y_scale <- ggplot2::scale_y_continuous(
      limits = c(0, 1.2),
      labels = label_percent(accuracy = 1),
      oob = scales::oob_keep
    )
    x_scale <- ggplot2::scale_x_date(date_labels = "%b", date_breaks = "2 months")
    facet_scales <- "free_x"
    caption <- "Rounds played divided by that day's capacity. The axis stops at 120 percent. Crosses are days above that. Gaps are missing dates or zero-round days of unknown status."
  }
  flagged <- cal[metric == "utilization" & !is.na(cal$plot_y) & cal$plot_y > 1.2, , drop = FALSE]
  if (metric == "utilization") {
    cal$plot_y[!is.na(cal$plot_y) & cal$plot_y > 1.2] <- NA_real_
  }
  gap <- is.na(cal$plot_y)
  cal$run <- cumsum(c(TRUE, diff(as.numeric(gap)) != 0))
  observed <- cal[!gap, , drop = FALSE]
  chart <- ggplot2::ggplot(
    observed,
    ggplot2::aes(
      x = .data$plot_x,
      y = .data$plot_y,
      group = interaction(.data$year_label, .data$run)
    )
  ) +
    ggplot2::geom_line(color = "#0072B2", linewidth = 0.3)
  if (nrow(flagged) > 0) {
    chart <- chart + ggplot2::geom_point(
      data = flagged,
      ggplot2::aes(x = .data$plot_x, y = 1.2),
      shape = 4,
      size = 1.6,
      stroke = 0.6,
      color = "#000000",
      inherit.aes = FALSE
    )
  }
  chart +
    x_scale +
    y_scale +
    ggplot2::facet_wrap(~year_label, ncol = 2, scales = facet_scales) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (metric == "capacity") 45 else 0,
        hjust = if (metric == "capacity") 1 else 0.5
      ),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
}

#' Capacity, demand, and utilization for each year
#'
#' One row per year and three columns. `unit = "golfers"` counts people. In
#' the Dobson extract a round is one golfer, and capacity is the city's
#' daylight estimate of golfers. `unit = "slots"` counts tee times. Slot
#' capacity is that golfer estimate divided by `players_per_slot`. Slot demand
#' is the fewest slots that could hold the golfers, rounding up. A group of
#' 1, 2, or 3 golfers still uses a slot, and this file does not record group
#' size, so slots actually used are not shown.
#'
#' The left column is capacity and demand, in ColorBrewer Dark2 gray and
#' green, on a January–December axis and a shared scale. The middle column is
#' utilization in ColorBrewer Dark2 purple, from 0 to 120 percent. A cross
#' marks a day above 120 percent. The right column is that year's utilization
#' heatmap. Its fill is ColorBrewer PRGn: green below the median played day,
#' purple above it, and red above 100 percent. A missing date is black, and a
#' zero-round day of unknown status is medium-dark gray. A cross marks a
#' heatmap day above 120 percent. A zero-round day of
#' unknown status and a missing date leave a gap in demand and in the
#' utilization line.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param calendar Optional [operating_calendar()].
#' @param unit `golfers` or `slots`.
#' @param players_per_slot Golfers a full slot can hold. Used only for
#'   `slots`. This is not a group size from the extract.
#' @return A `golfops_capacity_demand_plot`. Printing it draws one row per year.
#' @export
plot_capacity_demand <- function(
    data,
    calendar = NULL,
    unit = c("golfers", "slots"),
    players_per_slot = 4) {
  unit <- match.arg(unit)
  if (!is.numeric(players_per_slot) || length(players_per_slot) != 1 ||
      is.na(players_per_slot) || players_per_slot < 2) {
    abort(
      "`players_per_slot` must be a single number of at least 2.",
      class = "golfops_input_error"
    )
  }
  cal <- .calendar_day_frame(data, calendar)
  if (unit == "slots") {
    played <- cal$on_utilization_scale & !is.na(cal$rounds_played)
    cal$capacity_rounds <- cal$capacity_rounds / players_per_slot
    cal$rounds_played[played] <- ceiling(cal$rounds_played[played] / players_per_slot)
    cal$scale_utilization <- ifelse(
      played & !is.na(cal$capacity_rounds) & cal$capacity_rounds > 0,
      cal$rounds_played / cal$capacity_rounds,
      NA_real_
    )
  }
  years <- levels(cal$year_label)
  on_scale <- cal$scale_utilization[cal$on_utilization_scale & !is.na(cal$scale_utilization)]
  rounds_max <- max(
    c(cal$capacity_rounds, cal$rounds_played[cal$on_utilization_scale]),
    na.rm = TRUE
  )
  y_label <- if (unit == "golfers") "Golfers" else "Golf slots"
  headers <- if (unit == "golfers") {
    c("Golfer capacity and demand", "Golfer utilization", "Golfer utilization by day")
  } else {
    c("Slot capacity and fewest slots", "Slot utilization", "Slot utilization by day")
  }
  structure(
    list(
      years = years,
      capacity = lapply(years, function(year) {
        .year_capacity_demand(
          cal[cal$year_label == year, , drop = FALSE],
          rounds_max,
          y_label
        )
      }),
      utilization = lapply(years, function(year) {
        .year_utilization_line(cal[cal$year_label == year, , drop = FALSE])
      }),
      heatmap = lapply(years, function(year) {
        .year_utilization_heatmap(
          cal[cal$year_label == year, , drop = FALSE],
          on_scale,
          legend = FALSE
        )
      }),
      legend = .heatmap_legend(cal, on_scale),
      median_utilization = stats::median(on_scale, na.rm = TRUE),
      unit = unit,
      headers = headers,
      scale_max = rounds_max,
      players_per_slot = players_per_slot
    ),
    class = "golfops_capacity_demand_plot"
  )
}

#' @export
print.golfops_capacity_demand_plot <- function(x, ...) {
  n <- length(x$years)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(
      nrow = n + 2,
      ncol = 3,
      heights = grid::unit(c(0.28, rep(1, n), 0.85), "null")
    )
  ))
  headers <- x$headers
  for (col in seq_along(headers)) {
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = col))
    grid::grid.text(headers[col], gp = grid::gpar(fontsize = 11, fontface = "bold"))
    grid::upViewport()
  }
  panels <- list(x$capacity, x$utilization, x$heatmap)
  for (i in seq_len(n)) {
    for (col in seq_along(panels)) {
      print(
        panels[[col]][[i]],
        vp = grid::viewport(layout.pos.row = i + 1, layout.pos.col = col),
        newpage = FALSE
      )
    }
  }
  grid::pushViewport(grid::viewport(layout.pos.row = n + 2, layout.pos.col = 1:3))
  grid::grid.draw(x$legend)
  grid::upViewport()
  invisible(x)
}

.calendar_day_frame <- function(data, calendar = NULL) {
  cal <- utilization_calendar(data, calendar)
  span_start <- min(cal$date)
  span_end <- max(cal$date)
  missing <- cal$date[cal$operating_status == "missing"]
  cal$year_label <- vapply(cal$calendar_year, function(year) {
    .operating_year_label(year, span_start, span_end, missing)
  }, character(1))
  cal$year_label <- factor(cal$year_label, levels = unique(cal$year_label))
  cal$plot_x <- as.Date(paste0("2000-", format(cal$date, "%m-%d")))
  cal
}

.year_capacity_demand <- function(cal, rounds_max, y_label) {
  capacity <- cal
  capacity$series <- "Capacity"
  capacity$plot_y <- capacity$capacity_rounds
  demand <- cal
  demand$series <- "Demand"
  demand$plot_y <- ifelse(demand$on_utilization_scale, demand$rounds_played, NA_real_)
  observed <- .drop_calendar_gaps(rbind(capacity, demand))
  .year_line_panel(
    observed,
    title = as.character(cal$year_label[1]),
    y_label = y_label,
    y_scale = ggplot2::scale_y_continuous(
      limits = c(0, rounds_max),
      labels = label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ),
    color_values = c(Capacity = "#666666", Demand = "#1B9E77")
  )
}

.year_utilization_line <- function(cal) {
  util <- cal
  util$series <- "Utilization"
  util$plot_y <- ifelse(util$on_utilization_scale, util$scale_utilization * 100, NA_real_)
  flagged <- util[!is.na(util$plot_y) & util$plot_y > 120, , drop = FALSE]
  util$plot_y[!is.na(util$plot_y) & util$plot_y > 120] <- NA_real_
  observed <- .drop_calendar_gaps(util)
  chart <- .year_line_panel(
    observed,
    title = as.character(cal$year_label[1]),
    y_label = "Utilization",
    y_scale = ggplot2::scale_y_continuous(
      limits = c(0, 120),
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ),
    color_values = c(Utilization = "#7570B3")
  )
  if (nrow(flagged) > 0) {
    chart <- chart + ggplot2::geom_point(
      data = flagged,
      ggplot2::aes(x = .data$plot_x, y = 120),
      shape = 4,
      size = 1.3,
      stroke = 0.45,
      color = "#000000",
      inherit.aes = FALSE
    )
  }
  chart
}

.year_line_panel <- function(observed, title, y_label, y_scale, color_values) {
  ggplot2::ggplot(
    observed,
    ggplot2::aes(
      x = .data$plot_x,
      y = .data$plot_y,
      color = .data$series,
      group = interaction(.data$series, .data$run)
    )
  ) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::scale_x_date(
      limits = as.Date(c("2000-01-01", "2000-12-31")),
      breaks = seq(as.Date("2000-01-01"), as.Date("2000-12-01"), by = "month"),
      date_labels = "%b",
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    y_scale +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::labs(title = title, x = NULL, y = y_label) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6),
      plot.title = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(2, 6, 2, 2)
    )
}

.drop_calendar_gaps <- function(plot_df) {
  plot_df <- plot_df[order(plot_df$series, plot_df$date), , drop = FALSE]
  gap <- is.na(plot_df$plot_y)
  keys <- interaction(plot_df$year_label, plot_df$series, drop = TRUE)
  plot_df$run <- stats::ave(as.integer(gap), keys, FUN = function(group) {
    cumsum(c(TRUE, diff(group) != 0))
  })
  plot_df[!gap, , drop = FALSE]
}

.year_utilization_heatmap <- function(cal, utilization, legend = FALSE) {
  on_scale <- cal[cal$on_utilization_scale & !is.na(cal$scale_utilization), , drop = FALSE]
  on_scale$fill_utilization <- pmin(on_scale$scale_utilization, 1.2)
  special <- .offscale_marks(cal[!cal$on_utilization_scale, , drop = FALSE])
  above <- on_scale[on_scale$scale_utilization > 1.2, , drop = FALSE]
  ggplot2::ggplot(cal, ggplot2::aes(x = .data$day, y = .data$month)) +
    .utilization_heatmap_layers(on_scale, special, above, utilization, legend = legend) +
    ggplot2::scale_x_continuous(
      limits = c(0.5, 31.5),
      breaks = c(1, 8, 15, 22, 29),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(title = as.character(cal$year_label[1]), x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 6),
      axis.text.x = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(2, 4, 2, 2)
    )
}

.heatmap_legend <- function(cal, utilization) {
  on_scale <- cal[cal$on_utilization_scale & !is.na(cal$scale_utilization), , drop = FALSE]
  on_scale$fill_utilization <- pmin(on_scale$scale_utilization, 1.2)
  special <- .offscale_marks(cal[!cal$on_utilization_scale, , drop = FALSE])
  above <- on_scale[on_scale$scale_utilization > 1.2, , drop = FALSE]
  chart <- ggplot2::ggplot(cal, ggplot2::aes(x = .data$day, y = .data$month)) +
    .utilization_heatmap_layers(
      on_scale,
      special,
      above,
      utilization,
      legend = TRUE,
      barwidth = grid::unit(14, "cm")
    ) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8)
    )
  grob <- ggplot2::ggplotGrob(chart)
  guide <- which(grob$layout$name == "guide-box-bottom")
  grob$grobs[[guide[1]]]
}

.offscale_marks <- function(special) {
  if (nrow(special) == 0) {
    special$mark <- factor(character(), levels = c(
      "Missing date",
      "Confirmed closure",
      "Zero rounds, status unknown",
      "Play on a day marked closed"
    ))
    return(special)
  }
  special$mark <- factor(
    ifelse(
      special$operating_status == "missing", "Missing date",
      ifelse(
        special$operating_status == "confirmed_closure", "Confirmed closure",
        ifelse(
          special$operating_status == "zero_rounds_unknown", "Zero rounds, status unknown",
          ifelse(
            special$operating_status == "conflict_play_on_closed_day", "Play on a day marked closed",
            special$operating_status
          )
        )
      )
    ),
    levels = c(
      "Missing date",
      "Confirmed closure",
      "Zero rounds, status unknown",
      "Play on a day marked closed"
    )
  )
  special
}

.utilization_heatmap_layers <- function(
    on_scale,
    special,
    above,
    utilization,
    legend = FALSE,
    barwidth = grid::unit(11, "cm")) {
  layers <- list(
    ggplot2::geom_tile(
      data = on_scale,
      ggplot2::aes(fill = .data$fill_utilization),
      color = "white",
      linewidth = 0.15
    ),
    ggplot2::geom_tile(
      data = special[special$operating_status == "missing", , drop = FALSE],
      fill = "#000000",
      color = "white",
      linewidth = 0.15
    ),
    ggplot2::geom_tile(
      data = special[special$operating_status == "confirmed_closure", , drop = FALSE],
      fill = "#252525",
      color = "white",
      linewidth = 0.15
    ),
    ggplot2::geom_tile(
      data = special[special$operating_status == "zero_rounds_unknown", , drop = FALSE],
      fill = "#737373",
      color = "white",
      linewidth = 0.15
    ),
    ggplot2::geom_tile(
      data = special[special$operating_status == "conflict_play_on_closed_day", , drop = FALSE],
      fill = "#CC79A7",
      color = "white",
      linewidth = 0.15
    ),
    ggplot2::geom_point(
      data = above,
      shape = 4,
      size = 0.7,
      color = "#000000",
      stroke = 0.4
    ),
    if (nrow(special) > 0) {
      ggplot2::geom_point(
        data = special,
        ggplot2::aes(color = .data$mark),
        shape = 15,
        size = 0,
        show.legend = legend
      )
    },
    .utilization_fill_scale(utilization, barwidth = barwidth),
    if (nrow(special) > 0) {
      ggplot2::scale_color_manual(
        values = c(
          "Missing date" = "#000000",
          "Confirmed closure" = "#252525",
          "Zero rounds, status unknown" = "#737373",
          "Play on a day marked closed" = "#CC79A7"
        ),
        name = "Off the utilization scale",
        drop = TRUE
      )
    },
    ggplot2::guides(
      color = if (nrow(special) > 0) {
        ggplot2::guide_legend(
          override.aes = list(size = 4, shape = 15),
          nrow = 2
        )
      } else {
        "none"
      },
      fill = if (legend) {
        ggplot2::guide_colorbar(
          barwidth = barwidth,
          barheight = grid::unit(0.35, "cm"),
          title.position = "top"
        )
      } else {
        "none"
      }
    )
  )
  layers[!vapply(layers, is.null, logical(1))]
}

#' Calendar heatmap of utilization against resolved capacity
#'
#' Each panel is a year. Fill is ColorBrewer's colorblind-safe PRGn palette,
#' reversed so green is below the median played day and purple is above it.
#' The light color is that median. Red is above 100 percent. Missing dates,
#' confirmed closures, and zero-round days of unknown status are drawn off
#' that scale. A cross marks a day above 120 percent.
#'
#' @param data Daily rows from [dobson_operating_daily()].
#' @param calendar Optional [operating_calendar()].
#' @return A ggplot object.
#' @export
plot_utilization_heatmap <- function(data, calendar = NULL) {
  cal <- utilization_calendar(data, calendar)
  span_start <- min(cal$date)
  span_end <- max(cal$date)
  missing <- cal$date[cal$operating_status == "missing"]
  cal$year_label <- vapply(cal$calendar_year, function(year) {
    .operating_year_label(year, span_start, span_end, missing)
  }, character(1))
  on_scale <- cal[cal$on_utilization_scale & !is.na(cal$scale_utilization), , drop = FALSE]
  on_scale$fill_utilization <- pmin(on_scale$scale_utilization, 1.2)
  fill_scale <- .utilization_fill_scale(on_scale$scale_utilization)
  median_util <- attr(fill_scale, "median_utilization")
  special <- cal[!cal$on_utilization_scale, , drop = FALSE]
  special$mark <- factor(
    ifelse(
      special$operating_status == "missing", "Missing date",
      ifelse(
        special$operating_status == "confirmed_closure", "Confirmed closure",
        ifelse(
          special$operating_status == "zero_rounds_unknown", "Zero rounds, status unknown",
          ifelse(
            special$operating_status == "conflict_play_on_closed_day", "Play on a day marked closed",
            special$operating_status
          )
        )
      )
    ),
    levels = c(
      "Missing date",
      "Confirmed closure",
      "Zero rounds, status unknown",
      "Play on a day marked closed"
    )
  )
  above <- on_scale[on_scale$scale_utilization > 1.2, , drop = FALSE]
  ggplot2::ggplot(cal, ggplot2::aes(x = .data$day, y = .data$month)) +
    ggplot2::geom_tile(
      data = on_scale,
      ggplot2::aes(fill = .data$fill_utilization),
      color = "white",
      linewidth = 0.15
    ) +
    ggplot2::geom_tile(
      data = special[special$operating_status == "missing", , drop = FALSE],
      fill = "#000000",
      color = "white",
      linewidth = 0.15
    ) +
    ggplot2::geom_tile(
      data = special[special$operating_status == "confirmed_closure", , drop = FALSE],
      fill = "#252525",
      color = "white",
      linewidth = 0.15
    ) +
    ggplot2::geom_tile(
      data = special[special$operating_status == "zero_rounds_unknown", , drop = FALSE],
      fill = "#737373",
      color = "white",
      linewidth = 0.15
    ) +
    ggplot2::geom_tile(
      data = special[special$operating_status == "conflict_play_on_closed_day", , drop = FALSE],
      fill = "#CC79A7",
      color = "white",
      linewidth = 0.15
    ) +
    ggplot2::geom_point(
      data = above,
      shape = 4,
      size = 0.7,
      color = "#000000",
      stroke = 0.4
    ) +
    ggplot2::geom_point(
      data = special,
      ggplot2::aes(color = .data$mark),
      shape = 15,
      size = 0,
      show.legend = TRUE
    ) +
    fill_scale +
    ggplot2::scale_color_manual(
      values = c(
        "Missing date" = "#000000",
        "Confirmed closure" = "#252525",
        "Zero rounds, status unknown" = "#737373",
        "Play on a day marked closed" = "#CC79A7"
      ),
      name = "Off the utilization scale"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(size = 4, shape = 15),
        nrow = 2
      )
    ) +
    ggplot2::scale_x_continuous(breaks = c(1, 8, 15, 22, 29)) +
    ggplot2::facet_wrap(~year_label, ncol = 2) +
    ggplot2::labs(
      title = "Daily utilization of published capacity",
      x = "Day of month",
      y = NULL,
      caption = sprintf(
        "Green is below the median played day and purple is above it. The light color is that median, %s. Red is above 100 percent. Crosses are days above 120 percent. Black is a missing date, and medium-dark gray is a zero-round day whose status is unknown. Blank areas are months outside the sample, not zeros. With no operating calendar, capacity is the city's daylight estimate.",
        label_percent(accuracy = 1)(median_util)
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30", hjust = 0),
      plot.margin = ggplot2::margin(6, 6, 14, 6)
    )
}

.utilization_fill_scale <- function(utilization, barwidth = grid::unit(11, "cm")) {
  median_util <- stats::median(utilization, na.rm = TRUE)
  if (!is.finite(median_util)) {
    median_util <- 0.5
  }
  has_over <- any(utilization > 1, na.rm = TRUE)
  high <- if (has_over) 1.2 else 1
  split_at <- median_util
  purple_end <- if (has_over) 1 else high
  if (split_at <= 0 || split_at >= purple_end) {
    split_at <- purple_end / 2
  }
  # ColorBrewer PRGn, 11 classes. Native order runs from purple to green.
  # Reversed here so the low side is green and the high side is purple.
  prgn <- c(
    "#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#F7F7F7",
    "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"
  )
  green_to_purple <- rev(prgn)
  green <- green_to_purple[1:6]
  purple <- green_to_purple[7:11]
  positions <- c(
    seq(0, split_at, length.out = length(green)),
    seq(split_at, purple_end, length.out = length(purple) + 1L)[-1]
  )
  colors <- c(green, purple)
  if (has_over) {
    positions <- c(positions, purple_end + 1e-3, high)
    colors <- c(colors, "#B2182B", "#67001F")
  }
  breaks <- c(0, split_at, if (has_over) 1 else NULL, high)
  scale <- ggplot2::scale_fill_gradientn(
    colors = colors,
    values = scales::rescale(positions),
    limits = c(0, high),
    breaks = breaks,
    labels = function(x) {
      pct <- label_percent(accuracy = 1)(x)
      ifelse(abs(x - split_at) < 1e-8, paste(pct, "median"), pct)
    },
    oob = scales::squish,
    name = "Utilization",
    guide = ggplot2::guide_colorbar(
      barwidth = barwidth,
      barheight = grid::unit(0.35, "cm"),
      title.position = "top"
    )
  )
  attr(scale, "median_utilization") <- median_util
  scale
}

.operating_year_label <- function(year, span_start, span_end, missing) {
  year_start <- as.Date(sprintf("%s-01-01", year))
  year_end <- as.Date(sprintf("%s-12-31", year))
  notes <- character()
  if (span_start > year_start && year(span_start) == year) {
    notes <- c(notes, sprintf("from %s", format(span_start, "%b %d")))
  }
  if (span_end < year_end && year(span_end) == year) {
    notes <- c(notes, sprintf("through %s", format(span_end, "%b %d")))
  }
  if (any(year(missing) == year)) {
    notes <- c(notes, "missing day")
  }
  if (length(notes) == 0) {
    as.character(year)
  } else {
    sprintf("%s (%s)", year, paste(notes, collapse = ", "))
  }
}

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

#' Calendar heatmap of utilization against resolved capacity
#'
#' Each panel is a year. The fill scale is shared and is not capped at 100
#' percent. Missing dates, confirmed closures, and zero-round days of unknown
#' status are drawn off that scale. A cross marks a day above the capacity
#' used for that cell.
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
  max_util <- max(on_scale$scale_utilization)
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
  above <- on_scale[on_scale$operating_status %in% c("above_capacity", "above_daylight_estimate"), , drop = FALSE]
  fill_scale <- if (max_util <= 1) {
    ggplot2::scale_fill_gradientn(
      colors = c("#deebf7", "#2171b5"),
      limits = c(0, max(1, max_util)),
      labels = label_percent(accuracy = 1),
      name = "Utilization"
    )
  } else {
    ggplot2::scale_fill_gradientn(
      colors = c("#deebf7", "#2171b5", "#67000d"),
      values = scales::rescale(c(0, 1, max_util)),
      limits = c(0, max_util),
      labels = label_percent(accuracy = 1),
      name = "Utilization"
    )
  }
  ggplot2::ggplot(cal, ggplot2::aes(x = .data$day, y = .data$month)) +
    ggplot2::geom_tile(
      data = on_scale,
      ggplot2::aes(fill = .data$scale_utilization),
      color = "white",
      linewidth = 0.15
    ) +
    ggplot2::geom_tile(
      data = special[special$operating_status == "missing", , drop = FALSE],
      fill = "#BDBDBD",
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
      fill = "#E69F00",
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
        "Missing date" = "#BDBDBD",
        "Confirmed closure" = "#252525",
        "Zero rounds, status unknown" = "#E69F00",
        "Play on a day marked closed" = "#CC79A7"
      ),
      name = "Off the utilization scale"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 4, shape = 15))
    ) +
    ggplot2::scale_x_continuous(breaks = c(1, 8, 15, 22, 29)) +
    ggplot2::facet_wrap(~year_label, ncol = 2) +
    ggplot2::labs(
      title = "Daily utilization of published capacity",
      x = "Day of month",
      y = NULL,
      caption = "The fill is not capped at 100 percent. Crosses are days above the capacity used for that cell. Grey is a missing date, black is a confirmed closure, and orange is a zero-round day whose status is unknown. Blank areas are months outside the sample, not zeros. With no operating calendar, capacity is the city's daylight estimate."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 8, color = "grey30")
    )
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

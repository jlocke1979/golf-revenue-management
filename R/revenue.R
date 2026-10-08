#' Historical revenue per dollar of green-fee revenue
#'
#' Each ratio is a sum of one category divided by the sum of green fees on the
#' rows where both amounts are present. It is not the average of daily ratios.
#' Rows with a missing category drop out of that category only. Revenue on
#' zero-round days stays in the sums.
#'
#' A green-fee denominator of zero makes the ratio unavailable. The result is
#' missing, not zero and not infinity. The ratio is a historical relationship
#' between recorded revenue categories. It is not a causal multiplier and not
#' spending per golfer.
#'
#' The input may be prepared Dobson rows or a [course_revenue()] table. A
#' revenue table does not need a participation file.
#'
#' @param data A `golfops_rounds` table or a `golfops_course_revenue` table.
#' @param by `all`, `year`, or `month_of_year`.
#' @return A tibble with one row per category and period.
#' @export
#' @examples
#' revenue_relationships(dobson_operating_daily(), by = "year")
revenue_relationships <- function(data, by = c("all", "year", "month_of_year")) {
  by <- match.arg(by)
  frame <- .as_revenue_frame(data)
  if (length(unique(frame$frequency)) != 1) {
    abort(
      "Revenue ratios cannot mix daily and monthly rows.",
      class = "golfops_input_error"
    )
  }
  categories <- c("food_beverage", "range", "merchandise", "cart", "club")
  frame$.group <- switch(
    by,
    all = "all",
    year = as.character(year(frame$date)),
    month_of_year = month_names()[month(frame$date)]
  )
  groups <- unique(frame$.group)
  rows <- lapply(groups, function(group) {
    block <- frame[frame$.group == group, , drop = FALSE]
    lapply(categories, function(category) {
      .one_revenue_ratio(block, category)
    })
  })
  out <- as_tibble(do.call(rbind, unlist(rows, recursive = FALSE)))
  if (by == "month_of_year") {
    out$period <- factor(out$period, levels = month_names())
    out <- out[order(out$category, out$period), , drop = FALSE]
  }
  out
}

.as_revenue_frame <- function(data) {
  if (inherits(data, "golfops_course_revenue")) {
    frame <- data
    frame$rounds <- NA_real_
    return(frame)
  }
  if (inherits(data, "golfops_rounds") || "green_fee_revenue" %in% names(data)) {
    data <- .require_rounds(data)
    return(tibble(
      course_id = "dobson_ranch",
      date = data$date,
      frequency = data$grain,
      green_fee = data$green_fee_revenue,
      food_beverage = data$food_beverage_revenue,
      range = data$range_revenue,
      merchandise = data$merchandise_revenue,
      cart = data$cart_revenue,
      club = data$club_revenue,
      rounds = data$rounds_played,
      cart_green_fee_treatment = "published_as_separate_categories"
    ))
  }
  abort(
    "Expected prepared rounds or a course_revenue() table.",
    class = "golfops_input_error"
  )
}

.one_revenue_ratio <- function(block, category) {
  value <- block[[category]]
  green <- block$green_fee
  matched <- !is.na(value) & !is.na(green)
  green_sum <- sum(green[matched])
  category_sum <- sum(value[matched])
  if (!any(matched)) {
    ratio <- NA_real_
    ratio_status <- "unavailable_no_matched_coverage"
  } else if (green_sum == 0) {
    ratio <- NA_real_
    ratio_status <- "unavailable_zero_green_fee"
  } else {
    ratio <- category_sum / green_sum
    ratio_status <- "ratio_of_sums"
  }
  treatment <- unique(block$cart_green_fee_treatment)
  treatment <- treatment[!is.na(treatment)]
  treatment_note <- if (length(treatment) == 0) {
    "Cart and green-fee separation is unknown."
  } else if (all(treatment == "published_as_separate_categories")) {
    "The source publishes these as separate categories. That does not show whether a posted fee bundled a cart."
  } else if (any(treatment == "bundled")) {
    "Cart revenue may also sit inside green fees. This ratio is not an add-on to the green fee."
  } else if (any(treatment == "unknown")) {
    "Cart and green-fee separation is unknown, so the cart ratio is not evidence of an extra charge."
  } else {
    "The source describes cart revenue as separate from green fees."
  }
  data.frame(
    period = block$.group[1],
    category = category,
    category_revenue = if (any(matched)) category_sum else NA_real_,
    green_fee_revenue_matched = if (any(matched)) green_sum else NA_real_,
    ratio_per_green_fee_dollar = ratio,
    ratio_status = ratio_status,
    n_rows_matched = sum(matched),
    n_zero_round_rows_in_match = if ("rounds" %in% names(block) && any(!is.na(block$rounds))) {
      sum(matched & !is.na(block$rounds) & block$rounds == 0)
    } else {
      NA_integer_
    },
    facility_revenue_recorded = sum_defined(c(
      sum_defined(block$green_fee),
      sum_defined(block$food_beverage),
      sum_defined(block$range),
      sum_defined(block$merchandise),
      sum_defined(block$cart),
      sum_defined(block$club)
    )),
    interpretation = "Historical revenue relationship. Not a causal multiplier and not spending per golfer.",
    cart_note = if (category == "cart") treatment_note else "",
    stringsAsFactors = FALSE
  )
}

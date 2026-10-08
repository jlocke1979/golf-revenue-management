#' Break-even baseline for hosting a course tournament
#'
#' Compares event revenue with regular-play green-fee revenue on the supplied
#' dates. A full-day block uses the whole observed day. A partial-day block
#' uses `displacement_share` as the caller's assumption. The share is not
#' computed from hours, and daily demand is not spread evenly across the day.
#'
#' Observed ancillary revenue on those dates is reported and is left out of
#' the contribution unless `ancillary_margin` is supplied. Variable cost is
#' left out unless `variable_cost_per_round` is supplied. Contribution is
#' calculated only when event costs, the variable cost, and the ancillary
#' margin are all supplied. Descriptive revenue ratios are not used as
#' incremental-spend multipliers.
#'
#' The result is an economic baseline for the supplied inputs. It is not a
#' recommended entry fee or a market quote.
#'
#' @param operations Daily `golfops_rounds` rows that include every event date.
#' @param event_dates Dates the tournament would occupy.
#' @param event_revenue Revenue attributed to the event.
#' @param event_costs Incremental event costs. Omit when they are unknown.
#' @param variable_cost_per_round Variable operating cost per displaced round.
#'   Omit when it is unknown.
#' @param ancillary_margin Margin on displaced ancillary revenue, between 0 and
#'   1. Omit when it is unknown.
#' @param block `full_day` or `partial_day`.
#' @param displacement_share Required for `partial_day`. A full day uses 1.
#' @return A `golfops_breakeven` list.
#' @export
tournament_breakeven <- function(
    operations,
    event_dates,
    event_revenue,
    event_costs = NULL,
    variable_cost_per_round = NULL,
    ancillary_margin = NULL,
    block = c("full_day", "partial_day"),
    displacement_share = NULL) {
  block <- match.arg(block)
  operations <- .require_rounds(operations)
  if (any(operations$grain == "month", na.rm = TRUE)) {
    abort("Tournament comparisons use daily rows.", class = "golfops_input_error")
  }
  if (any(duplicated(operations$date))) {
    abort("Drop duplicate dates before estimating displaced play.", class = "golfops_duplicate_error")
  }
  event_dates <- as.Date(event_dates)
  if (any(is.na(event_dates)) || length(event_dates) == 0) {
    abort("`event_dates` must contain at least one date.", class = "golfops_input_error")
  }
  if (!is.numeric(event_revenue) || length(event_revenue) != 1 || is.na(event_revenue)) {
    abort("`event_revenue` must be a single number.", class = "golfops_input_error")
  }
  if (block == "full_day") {
    if (!is.null(displacement_share) && displacement_share != 1) {
      abort(
        "A full-day block displaces the whole observed day. Use block = 'partial_day' to supply a share.",
        class = "golfops_input_error"
      )
    }
    displacement_share <- 1
  } else if (
    is.null(displacement_share) || length(displacement_share) != 1 ||
      is.na(displacement_share) || displacement_share <= 0 || displacement_share > 1
  ) {
    abort(
      "A partial-day block needs displacement_share greater than 0 and at most 1. Daily demand is not allocated by hours.",
      class = "golfops_input_error"
    )
  }
  missing_dates <- event_dates[!event_dates %in% operations$date]
  if (length(missing_dates) > 0) {
    abort(
      paste0(
        "These event dates are not in the operations table and were not imputed: ",
        paste(missing_dates, collapse = ", ")
      ),
      class = "golfops_input_error"
    )
  }
  block_rows <- operations[operations$date %in% event_dates, , drop = FALSE]
  displaced_rounds <- sum(block_rows$rounds_played) * displacement_share
  displaced_green_fee <- sum(block_rows$green_fee_revenue) * displacement_share
  observed_ancillary <- sum(block_rows$ancillary_revenue) * displacement_share
  ancillary_contribution <- if (is.null(ancillary_margin)) {
    NA_real_
  } else {
    if (!is.numeric(ancillary_margin) || length(ancillary_margin) != 1 ||
        is.na(ancillary_margin) || ancillary_margin < 0 || ancillary_margin > 1) {
      abort("`ancillary_margin` must be a single number from 0 to 1.", class = "golfops_input_error")
    }
    observed_ancillary * ancillary_margin
  }
  variable_cost <- if (is.null(variable_cost_per_round)) {
    NA_real_
  } else {
    if (!is.numeric(variable_cost_per_round) || length(variable_cost_per_round) != 1 ||
        is.na(variable_cost_per_round) || variable_cost_per_round < 0) {
      abort("`variable_cost_per_round` must be a single non-negative number.", class = "golfops_input_error")
    }
    displaced_rounds * variable_cost_per_round
  }
  event_cost_value <- if (is.null(event_costs)) {
    NA_real_
  } else {
    if (!is.numeric(event_costs) || length(event_costs) != 1 || is.na(event_costs)) {
      abort("`event_costs` must be a single number.", class = "golfops_input_error")
    }
    event_costs
  }
  costs_known <- !is.na(event_cost_value) && !is.na(variable_cost) && !is.na(ancillary_contribution)
  contribution <- if (costs_known) {
    event_revenue - event_cost_value - variable_cost - ancillary_contribution - displaced_green_fee
  } else {
    NA_real_
  }
  known_cost <- sum(c(event_cost_value, variable_cost, ancillary_contribution), na.rm = TRUE)
  break_even <- displaced_green_fee + known_cost
  assumptions <- c(
    "This is an economic break-even baseline for the supplied dates. It is not a recommended price or a market quote.",
    "Displaced regular-play green-fee revenue is the observed green-fee revenue on those dates, scaled by the displacement share.",
    "A partial-day share is the caller's assumption. The package does not spread daily demand evenly across hours.",
    "Observed ancillary revenue is reported and is not assumed to disappear. It enters the contribution only when ancillary_margin is supplied.",
    "Revenue ratios are not used as multipliers for incremental spending.",
    "Variable operating cost enters only when variable_cost_per_round is supplied. Contribution is omitted until event costs, that variable cost, and the ancillary margin are all supplied.",
    "Zero-round days in the block contribute no displaced rounds. Their facility revenue stays in the observed ancillary amount."
  )
  structure(
    list(
      totals = tibble(
        block = block,
        displacement_share = displacement_share,
        n_dates = length(event_dates),
        event_revenue = event_revenue,
        displaced_rounds = displaced_rounds,
        displaced_green_fee_revenue = displaced_green_fee,
        observed_ancillary_revenue = observed_ancillary,
        displaced_ancillary_contribution = ancillary_contribution,
        event_costs = event_cost_value,
        variable_operating_cost = variable_cost,
        contribution = contribution,
        break_even_event_revenue = break_even,
        ancillary_included_in_break_even = !is.na(ancillary_contribution)
      ),
      assumptions = assumptions
    ),
    class = "golfops_breakeven"
  )
}

#' @export
print.golfops_breakeven <- function(x, ...) {
  cat("Tournament break-even baseline\n\n")
  print(x$totals)
  cat("\n")
  cat(paste0("- ", x$assumptions), sep = "\n")
  invisible(x)
}

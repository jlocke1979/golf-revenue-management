# Break-even baseline for hosting a course tournament

Compares event revenue with regular-play green-fee revenue on the
supplied dates. A full-day block uses the whole observed day. A
partial-day block uses `displacement_share` as the caller's assumption.
The share is not computed from hours, and daily demand is not spread
evenly across the day.

## Usage

``` r
tournament_breakeven(
  operations,
  event_dates,
  event_revenue,
  event_costs = NULL,
  variable_cost_per_round = NULL,
  ancillary_margin = NULL,
  block = c("full_day", "partial_day"),
  displacement_share = NULL
)
```

## Arguments

- operations:

  Daily `golfops_rounds` rows that include every event date.

- event_dates:

  Dates the tournament would occupy.

- event_revenue:

  Revenue attributed to the event.

- event_costs:

  Incremental event costs. Omit when they are unknown.

- variable_cost_per_round:

  Variable operating cost per displaced round. Omit when it is unknown.

- ancillary_margin:

  Margin on displaced ancillary revenue, between 0 and

  1.  Omit when it is unknown.

- block:

  `full_day` or `partial_day`.

- displacement_share:

  Required for `partial_day`. A full day uses 1.

## Value

A `golfops_breakeven` list.

## Details

Observed ancillary revenue on those dates is reported and is left out of
the contribution unless `ancillary_margin` is supplied. Variable cost is
left out unless `variable_cost_per_round` is supplied. Contribution is
calculated only when event costs, the variable cost, and the ancillary
margin are all supplied. Descriptive revenue ratios are not used as
incremental-spend multipliers.

The result is an economic baseline for the supplied inputs. It is not a
recommended entry fee or a market quote.

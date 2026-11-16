# Resolve capacity and operating status for prepared daily rows

Precedence is published available rounds, then a schedule estimate from
a complete operating window, then the city's daylight estimate. The
schedule estimate is
`floor(window minutes / tee interval) * players per start - blocked rounds`.
Any missing piece, including a missing blocked-round count, skips that
estimate. The daylight field is then the fallback and is labeled
`daylight_estimate`.

## Usage

``` r
apply_operating_capacity(data, calendar = NULL)
```

## Arguments

- data:

  A `golfops_rounds` table.

- calendar:

  Optional result of
  [`operating_calendar()`](https://jlocke1979.github.io/golf-revenue-management/reference/operating_calendar.md).

## Value

The same rows with `operating_status`, `capacity_rounds`,
`capacity_basis`, `unused_sellable_rounds`, and `capacity_utilization`.

## Details

Zero-round days stay `zero_rounds_unknown` unless the calendar says
`open` or `closed`. Unknown zero-round days do not contribute unused
sellable rounds. Confirmed closures do not either. Utilization is rounds
divided by the resolved capacity and is not capped at 1.

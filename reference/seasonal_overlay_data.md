# January–December values by year for the operating sample

Averages use observed rows only. Missing dates are not filled with zero.
Rounds and daylight utilization use days with played rounds, so an
unknown zero-round stretch does not look like a month of empty sellable
tee times. Revenue averages keep those days, because the facility still
recorded sales. A month is partial when the sample span includes a date
in that month that has no row, or when the month contains a zero-round
day of unknown status.

## Usage

``` r
seasonal_overlay_data(
  data,
  metric = c("rounds", "green_fee_revenue", "total_revenue", "utilization")
)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- metric:

  `rounds`, `green_fee_revenue`, `total_revenue`, or `utilization`.

## Value

A tibble with one row per year and month present in the sample.

# Plot one seasonal series with a line for each year

Open points are partial months. The year label names a partial year. The
color scale is the year, not a second measure.

## Usage

``` r
plot_seasonal_overlay(
  data,
  metric = c("rounds", "green_fee_revenue", "total_revenue", "utilization")
)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- metric:

  Passed to
  [`seasonal_overlay_data()`](https://jlocke1979.github.io/golf-revenue-management/reference/seasonal_overlay_data.md).

## Value

A ggplot object.

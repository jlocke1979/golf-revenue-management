# Plot holdout actuals against both forecasts

`interval = "week"` sums the holdout to weeks beginning Monday. Weekly
totals are easier to read than a daily line. They still include
zero-round days in the actual total.

## Usage

``` r
plot_forecast(forecast, interval = c("day", "week"))
```

## Arguments

- forecast:

  A result from
  [`compare_demand_forecasts()`](https://jlocke1979.github.io/golf-revenue-management/reference/compare_demand_forecasts.md).

- interval:

  `day` keeps one point per holdout date. `week` sums to the Monday of
  each week.

## Value

A ggplot object.

# Compare a seasonal baseline with one demand model

The split is chronological. Both forecasts are fit on the training
period only. Scores are reported for every test row and, when some test
rows were kept out of the fit, for the test rows that were eligible for
fitting.

## Usage

``` r
compare_demand_forecasts(
  data,
  holdout_start,
  formula = outcome ~ weekday + month,
  by = c("weekday", "month")
)
```

## Arguments

- data:

  A `golfops_demand` table.

- holdout_start:

  First test date.

- formula:

  Formula for
  [`forecast_demand_model()`](https://jlocke1979.github.io/golf-revenue-management/reference/forecast_demand_model.md).

- by:

  Keys for
  [`forecast_seasonal_baseline()`](https://jlocke1979.github.io/golf-revenue-management/reference/forecast_seasonal_baseline.md).

## Value

A `golfops_forecast` list.

# Compare Dobson Ranch round forecasts on a chronological holdout

The seasonal baseline is the training mean for the same weekday and
month. The demand model is a linear regression of observed rounds on
weekday and month. The default holdout is calendar year 2025.

## Usage

``` r
forecast_dobson_rounds(
  demand = NULL,
  holdout_start = as.Date("2025-01-01"),
  holdout_end = as.Date("2025-12-31")
)
```

## Arguments

- demand:

  Optional series from
  [`dobson_rounds_demand()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_rounds_demand.md).

- holdout_start:

  First date of the test period.

- holdout_end:

  Last date included in the scored series. The default keeps calendar
  2025 and leaves the partial 2026 year out of the score.

## Value

A `golfops_forecast` list.

## Examples

``` r
forecast_dobson_rounds()
#> Demand forecast for dobson_ranch_daily_rounds
#> Holdout starts 2025-01-01. Training rows: 1279. Rows used in the fit: 1234.
#> 
#> # A tibble: 4 × 6
#>   model             slice                     n   mae  rmse mean_error
#>   <chr>             <chr>                 <int> <dbl> <dbl>      <dbl>
#> 1 seasonal_baseline all_rows                365  45.4  71.7      -1.50
#> 2 demand_model      all_rows                365  45.3  71.4      -1.98
#> 3 seasonal_baseline rows_eligible_for_fit   354  38.7  56.3      -9.65
#> 4 demand_model      rows_eligible_for_fit   354  38.7  56.5     -10.0 
#> 
#> Limits
#> - The forecasts predict the observed outcome. They do not estimate latent demand on sold-out or above-capacity days.
#> - Predictions are not capped at capacity, and days above capacity are not revised upward.
#> - The volume scenario, if used, is an assumption supplied by the user. It is not a price elasticity.
#> - Rows flagged for exclusion stay in the all-rows score and are omitted from the eligible-rows score. For Dobson Ranch the October rows are course closures. The extract does not label them closed. A prediction on one of those dates is expected play if the day had been open.
```

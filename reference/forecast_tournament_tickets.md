# Forecast the synthetic tournament with its own model

Uses the same split, baseline, and score as Dobson Ranch. The demand
model is separate: tickets on weekday and days remaining until the
event. The default holdout is the 2025 edition.

## Usage

``` r
forecast_tournament_tickets(
  tickets = NULL,
  holdout_start = as.Date("2025-01-01")
)
```

## Arguments

- tickets:

  Optional series from
  [`synthetic_tournament_tickets()`](https://jlocke1979.github.io/golf-revenue-management/reference/synthetic_tournament_tickets.md).

- holdout_start:

  First date of the test edition.

## Value

A `golfops_forecast` list with `synthetic` TRUE.

## Examples

``` r
forecast_tournament_tickets()
#> Demand forecast for synthetic_cedar_municipal_classic_tickets [synthetic]
#> Holdout starts 2025-01-01. Training rows: 180. Rows used in the fit: 180.
#> 
#> # A tibble: 2 × 6
#>   model             slice        n   mae  rmse mean_error
#>   <chr>             <chr>    <int> <dbl> <dbl>      <dbl>
#> 1 seasonal_baseline all_rows    90  7.89  9.99     -0.678
#> 2 demand_model      all_rows    90  6.78  8.52      0.756
#> 
#> Limits
#> - The forecasts predict the observed outcome. They do not estimate latent demand on sold-out or above-capacity days.
#> - Predictions are not capped at capacity, and days above capacity are not revised upward.
#> - The volume scenario, if used, is an assumption supplied by the user. It is not a price elasticity.
#> - Rows flagged for exclusion stay in the all-rows score and are omitted from the eligible-rows score. The flag is the caller's exclusion. It does not, by itself, establish that a day was closed. A prediction on an excluded date is the model's usual count for that calendar day. It is not evidence that the day was open.
```

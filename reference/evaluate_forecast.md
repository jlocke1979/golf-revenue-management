# Score forecasts against observed outcomes

Score forecasts against observed outcomes

## Usage

``` r
evaluate_forecast(actual, predicted, slice = "all_rows")
```

## Arguments

- actual, predicted:

  Numeric vectors of the same length.

- slice:

  Label for this score.

## Value

A one-row tibble with MAE, RMSE, and mean error (prediction minus
actual).

# Forecast with one interpretable linear model

The default formula is observed outcome on weekday and month. Fit it on
a different formula for a different application. The model sees training
rows with `include_in_fit` TRUE and does not see test outcomes.

## Usage

``` r
forecast_demand_model(train, test, formula = outcome ~ weekday + month)
```

## Arguments

- train, test:

  Demand tables from
  [`split_chronological()`](https://jlocke1979.github.io/golf-revenue-management/reference/split_chronological.md).

- formula:

  A model formula. The outcome column must be named `outcome`.

## Value

A list with `predicted` and `model`.

## Details

Predictions are not clipped at capacity. A prediction above capacity is
still a prediction of observed play, not an estimate of unmet demand.

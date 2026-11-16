# Forecast with training means for the same calendar keys

The default keys are weekday and month. A test row whose key never
appears in the fitting rows uses the weekday mean, then the overall
fitting mean. Means are computed only from rows with `include_in_fit`
TRUE.

## Usage

``` r
forecast_seasonal_baseline(train, test, by = c("weekday", "month"))
```

## Arguments

- train, test:

  Demand tables from
  [`split_chronological()`](https://jlocke1979.github.io/golf-revenue-management/reference/split_chronological.md).

- by:

  Column names used as the seasonal key.

## Value

A numeric vector with one prediction per test row.

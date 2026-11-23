# Dobson Ranch daily rounds as a demand series

Uses
[`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).
Zero-round days stay in the series and out of the fit. The extract does
not label them closed, and this series does not mark them closed.
`capacity` is the city's daylight estimate. Days above that estimate
remain observed play.

## Usage

``` r
dobson_rounds_demand(rounds = NULL)
```

## Arguments

- rounds:

  Optional result of
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  The bundled extract is read when this is omitted.

## Value

A `golfops_demand` table.

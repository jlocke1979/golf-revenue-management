# Revenue per available round by year

Each value is the sum of a revenue amount divided by the sum of
daylight-estimated rounds, including days with no golfers. This is the
annual form of revenue per available round. The papers call the tee-time
version RevPATR. The denominator here is the city's daylight estimate.
It is not a count of tee times. A year that does not run from January 1
through December 31 is marked with an asterisk.

## Usage

``` r
revenue_per_available_round(data)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

## Value

A data frame with one row per revenue type and year.

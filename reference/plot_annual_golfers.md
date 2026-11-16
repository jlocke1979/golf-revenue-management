# Golfers by year

One point per calendar year, joined by a line. The value is the sum of
daily rounds. In the city extract a round is one golfer who played the
course. A range-only visit is not included. A year that does not run
from January 1 through December 31 is marked with an asterisk.

## Usage

``` r
plot_annual_golfers(data)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

## Value

A ggplot object.

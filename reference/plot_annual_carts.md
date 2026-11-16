# Cart rentals by year

One point per calendar year, joined by a line. The value is the sum of
daily cart rentals. A year that does not run from January 1 through
December 31 is marked with an asterisk. Blank cart counts are omitted
from the total.

## Usage

``` r
plot_annual_carts(data)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

## Value

A ggplot object.

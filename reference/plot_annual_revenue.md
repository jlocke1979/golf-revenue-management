# Annual revenue by type

One bar per calendar year. Each panel is one revenue amount from the
city extract. Total revenue is the last panel. Each panel uses that
revenue type's color. A year that does not run from January 1 through
December 31 is marked with an asterisk and drawn in a lighter tint of
the same color. The bar is the sum of the daily amounts, including days
with no golfers.

## Usage

``` r
plot_annual_revenue(data)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

## Value

A ggplot object.

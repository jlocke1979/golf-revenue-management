# Revenue per golfer by year

Each value is the sum of a revenue amount divided by the sum of golfers,
on days with at least one golfer. It is an average recorded amount per
golfer, not a posted price. A year that does not run from January 1
through December 31 is marked with an asterisk. A blank category amount
is left out of the numerator. Golfers on those days stay in the
denominator.

## Usage

``` r
revenue_per_golfer(data)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

## Value

A data frame with one row per revenue type and year.

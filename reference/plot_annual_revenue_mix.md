# Annual revenue mix

One bar per calendar year. Each segment is one revenue type from the
city extract. `dollars` stacks the sum of daily amounts, including days
with no golfers. `share` scales each year to 100 percent. Total revenue
is the height of the dollar bar and is not drawn as its own segment. A
year that does not run from January 1 through December 31 is marked with
an asterisk.

## Usage

``` r
plot_annual_revenue_mix(data, scale = c("dollars", "share"))
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- scale:

  `"dollars"` or `"share"`.

## Value

A ggplot object.

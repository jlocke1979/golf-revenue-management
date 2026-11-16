# Dollars per golfer, stacked

One bar per calendar year. Each segment is one revenue type. `dollars`
stacks the average recorded amount per golfer. `share` scales each year
to 100 percent of those dollars per golfer. This is not the mix of total
revenue; see
[`plot_annual_revenue_mix()`](https://jlocke1979.github.io/golf-revenue-management/reference/plot_annual_revenue_mix.md).
Total revenue is the height of the dollar bar and is not drawn as its
own segment.

## Usage

``` r
plot_revenue_mix(data, scale = c("dollars", "share"))
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- scale:

  `"dollars"` or `"share"`.

## Value

A ggplot object.

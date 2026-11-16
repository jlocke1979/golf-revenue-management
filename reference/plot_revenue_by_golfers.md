# Daily revenue against golfers

Each point is one day with at least one golfer. The horizontal axis is
golfers. Each panel is one revenue amount from the city extract: green
fees, food and beverage, merchandise, range, cart, club rental, and
total revenue. Food and beverage is a single amount. Alcohol is not
separated from dining. A line on each panel is a straight fit. It
describes the cloud of days. It is not a causal effect of an extra
golfer. Each panel also prints the correlation of golfers with that
amount, rounded to two decimals. A chart of one year draws each panel in
that revenue type's color.

## Usage

``` r
plot_revenue_by_golfers(data, year = NULL)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- year:

  Optional calendar year. One year draws that year alone.

## Value

A ggplot object.

# Calendar heatmap of utilization against resolved capacity

Each panel is a year. Fill is ColorBrewer's colorblind-safe PRGn
palette, reversed so green is below the median played day and purple is
above it. The light color is that median. Red is above 100 percent.
Missing dates, confirmed closures, and zero-round days of unknown status
are drawn off that scale. A cross marks a day above 120 percent.

## Usage

``` r
plot_utilization_heatmap(data, calendar = NULL, layout = c("month", "weekday"))
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- calendar:

  Optional
  [`operating_calendar()`](https://jlocke1979.github.io/golf-revenue-management/reference/operating_calendar.md).

- layout:

  `month` puts the day of the month on the horizontal axis. `weekday`
  uses a Monday-through-Sunday calendar, one week per row.

## Value

A ggplot object.

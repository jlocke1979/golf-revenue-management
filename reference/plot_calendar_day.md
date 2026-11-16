# Capacity or utilization for each calendar day

One panel per year. Capacity uses a January–December axis so each year
lines up. Utilization is rounds played divided by that capacity, drawn
only on days that belong on the utilization scale. A zero-round day of
unknown status and a missing date leave a gap. The axis runs from 0 to
120 percent. A cross marks a day above 120 percent.

## Usage

``` r
plot_calendar_day(data, metric = c("capacity", "utilization"), calendar = NULL)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- metric:

  `capacity` or `utilization`.

- calendar:

  Optional
  [`operating_calendar()`](https://jlocke1979.github.io/golf-revenue-management/reference/operating_calendar.md).

## Value

A ggplot object.

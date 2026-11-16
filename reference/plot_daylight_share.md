# Daily share of the daylight estimate

Each played day is golfers divided by the city's daylight estimate for
that day. The box is the middle half of those days. Whiskers follow the
usual rule, one and a half times the height of the box. A day is marked
when it sits at least 5 percentage points beyond a whisker. The date is
the furthest such day in that month or weekday. `time_of_day` draws an
empty panel: the Dobson extract has no tee time.

## Usage

``` r
plot_daylight_share(
  data,
  by = c("month_of_year", "weekday", "day_of_week", "time_of_day")
)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- by:

  `month_of_year`, `weekday` (or `day_of_week`), or `time_of_day`.

## Value

A ggplot object.

## Examples

``` r
daily <- dobson_operating_daily()
plot_daylight_share(daily, "month_of_year")

plot_daylight_share(daily, "time_of_day")
```

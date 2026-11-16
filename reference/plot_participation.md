# Plot rounds or utilization from a summary

For weekday, season, and month-of-year summaries, rounds are plotted as
the average per source row. For year-month, year, and fiscal-year
summaries, rounds are plotted as totals. Utilization is always played
rounds divided by the sum of daylight-estimated rounds.

## Usage

``` r
plot_participation(summary, metric = c("rounds_played", "utilization"))
```

## Arguments

- summary:

  A `golfops_summary` from
  [`summarize_golf_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/summarize_golf_rounds.md).

- metric:

  `rounds_played` or `utilization`.

## Value

A ggplot object.

## Examples

``` r
rounds <- prepare_dobson_rounds()
summary <- summarize_golf_rounds(rounds, by = "weekday", duplicates = "drop_extra")
plot_participation(summary)
```

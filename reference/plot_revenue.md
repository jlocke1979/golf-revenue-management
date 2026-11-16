# Plot realized revenue per round from a summary

Plot realized revenue per round from a summary

## Usage

``` r
plot_revenue(summary, metric = c("per_played_round", "per_daylight_round"))
```

## Arguments

- summary:

  A `golfops_summary` from
  [`summarize_golf_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/summarize_golf_rounds.md).

- metric:

  `per_played_round` uses rows with played rounds in both the numerator
  and the denominator. `per_daylight_round` divides revenue by the
  city's daylight-estimated rounds, including rows with no play.

## Value

A ggplot object.

## Examples

``` r
rounds <- prepare_dobson_rounds()
summary <- summarize_golf_rounds(
  rounds[rounds$grain == "day", ],
  by = "year_month",
  duplicates = "drop_extra"
)
plot_revenue(summary)
```

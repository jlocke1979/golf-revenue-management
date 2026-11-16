# Summarize participation and revenue for one grain

Period yields are ratios of sums. They are not averages of daily yields.
Green-fee and total revenue per played round exclude rows with zero
played rounds from both the numerator and the denominator. Revenue per
daylight round keeps those rows, because the city's rounds-possible
value stays positive when nobody played.

## Usage

``` r
summarize_golf_rounds(
  data,
  by = c("year_month", "week", "weekday", "season", "year", "fiscal_year",
    "month_of_year"),
  grain = c("day", "month"),
  duplicates = c("abort", "drop_extra", "keep")
)
```

## Arguments

- data:

  A `golfops_rounds` table.

- by:

  Grouping. `year_month` is a time series. `week` is the Monday of each
  week and is meant for charts that are hard to read one day at a time.
  `month_of_year`, `weekday`, and `season` pool every matching row.
  `year` and `fiscal_year` are annual totals. Fiscal year follows the
  City of Mesa calendar, July through June.

- grain:

  Which rows to keep. Monthly and daily rows are never added together.

- duplicates:

  What to do when a date appears more than once. `abort` stops.
  `drop_extra` keeps one copy when the copies are identical aside from
  `row_id`. `keep` retains every row and will double count.

## Value

A tibble of class `golfops_summary`.

## Examples

``` r
rounds <- prepare_dobson_rounds()
summarize_golf_rounds(rounds, by = "weekday", duplicates = "drop_extra")
#> Golf operations summary by weekday (day grain, duplicates: drop_extra)
#> # A tibble: 7 × 20
#>   n_rows rounds_played rounds_possible open_daylight_rounds
#>    <int>         <dbl>           <dbl>                <dbl>
#> 1    252         61796           84560                23415
#> 2    251         60545           84272                23936
#> 3    252         60215           84560                24515
#> 4    253         65319           84964                19940
#> 5    253         67950           84596                17280
#> 6    252         67696           84580                17388
#> 7    252         69313           84572                15759
#> # ℹ 16 more variables: rounds_above_daylight <dbl>, green_fee_revenue <dbl>,
#> #   ancillary_revenue <dbl>, total_revenue <dbl>,
#> #   green_fee_revenue_on_played_rows <dbl>, total_revenue_on_played_rows <dbl>,
#> #   rounds_on_played_rows <dbl>, n_zero_round_rows <int>,
#> #   n_above_capacity_rows <int>, period <fct>, rounds_per_row <dbl>,
#> #   utilization <dbl>, green_fee_per_played_round <dbl>,
#> #   total_revenue_per_played_round <dbl>, green_fee_per_daylight_round <dbl>, …
```

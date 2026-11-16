# Audit a prepared Dobson Ranch table

Reports grain, coverage gaps, duplicate dates, and whether the published
revenue and booking-rate fields match the definitions in the city
catalog. The audit reads the prepared table only. It does not contact
the open-data portal.

## Usage

``` r
audit_golf_rounds(data)
```

## Arguments

- data:

  A `golfops_rounds` table from
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).

## Value

A `golfops_audit` list with a `checks` tibble. Each check has `status`
`pass`, `watch`, or `info`.

## Examples

``` r
audit_golf_rounds(prepare_dobson_rounds())
#> Dobson Ranch data audit
#> 
#> [info] rows_and_dates
#>     1,838 rows from 2004-12-10 to 2026-04-30. Rounds played sum to 868,646.
#> [pass] grain_separation
#>     1,766 daily rows and 72 monthly rows using a cutoff of 1,000 played rounds. Largest daily total is 675 and smallest monthly total is 2,963.
#> [pass] monthly_rows_dated_the_first
#>     0 monthly rows are not dated on the 1st. Monthly totals in this extract are dated the first of the month.
#> [watch] daily_coverage
#>     2004-12-10 to 2004-12-10; 2021-07-01 to 2026-04-30. Missing inside a segment: 2024-12-10.
#> [watch] duplicate_dates
#>     Duplicate dates: 2022-04-24. summarize_golf_rounds() will stop until duplicates are dropped or kept on purpose.
#> [pass] calendar_labels
#>     0 rows disagree with the calendar implied by Date for month, weekday, day, season, or fiscal year. Fiscal year is July 1 through June 30. Season is meteorological.
#> [pass] component_sum
#>     On 1674 rows with all six revenue categories, the largest absolute gap versus total revenue is $0.50. A gap of at most $0.50 is consistent with dollar rounding in the CSV.
#> [pass] revenue_per_round_source
#>     On 1734 rows, the city's Revenue/Round matches total revenue divided by rounds played within $0.02. Largest absolute gap is $0.0100. The field is blank when rounds played are zero.
#> [pass] booking_rate
#>     On 1771 rows, Round Booking Rate matches rounds played divided by rounds possible within 0.001 after converting the CSV percent to a proportion. Largest absolute gap is 0.00050.
#> [info] cart_revenue_coverage
#>     Cart revenue is populated on 1674 rows. The earliest populated date is 2004-12-10. On 116 rows cart revenue is blank while the other five categories are present, and those five already equal total revenue.
#> [watch] zero_round_rows
#>     56 rows have zero rounds played. Zero-round rows by month: October 55; November 1. Revenue on those rows is left out of per-played-round yields because the denominator is zero.
#> [watch] above_daylight_estimate
#>     109 rows have more rounds played than rounds possible. Utilization can exceed 1. The daylight estimate is not a hard cap in the source data.
#> [info] daylight_and_rounds_possible
#>     Correlation between daylight hours and rounds possible is 1.000 on 1771 rows. This is consistent with the catalog note that rounds possible comes from daylight, and it is not a recovered tee-sheet formula.
```

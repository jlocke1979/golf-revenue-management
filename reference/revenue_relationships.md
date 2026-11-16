# Historical revenue per dollar of green-fee revenue

Each ratio is a sum of one category divided by the sum of green fees on
the rows where both amounts are present. It is not the average of daily
ratios. Rows with a missing category drop out of that category only.
Revenue on zero-round days stays in the sums.

## Usage

``` r
revenue_relationships(data, by = c("all", "year", "month_of_year"))
```

## Arguments

- data:

  A `golfops_rounds` table or a `golfops_course_revenue` table.

- by:

  `all`, `year`, or `month_of_year`.

## Value

A tibble with one row per category and period.

## Details

A green-fee denominator of zero makes the ratio unavailable. The result
is missing, not zero and not infinity. The ratio is a historical
relationship between recorded revenue categories. It is not a causal
multiplier and not spending per golfer.

The input may be prepared Dobson rows or a
[`course_revenue()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_revenue.md)
table. A revenue table does not need a participation file.

## Examples

``` r
revenue_relationships(dobson_operating_daily(), by = "year")
#> # A tibble: 30 × 11
#>    period category      category_revenue green_fee_revenue_matched
#>    <chr>  <chr>                    <dbl>                     <dbl>
#>  1 2021   food_beverage          639701.                  1232760.
#>  2 2021   range                  356390.                  1232760.
#>  3 2021   merchandise            179652.                  1232760.
#>  4 2021   cart                   238336                    644525.
#>  5 2021   club                    14341                   1232760.
#>  6 2022   food_beverage         1671896.                  2794374.
#>  7 2022   range                 1145323.                  2794374.
#>  8 2022   merchandise            445108.                  2794374.
#>  9 2022   cart                  1205291                   2794374.
#> 10 2022   club                    39076                   2794374.
#> # ℹ 20 more rows
#> # ℹ 7 more variables: ratio_per_green_fee_dollar <dbl>, ratio_status <chr>,
#> #   n_rows_matched <int>, n_zero_round_rows_in_match <int>,
#> #   facility_revenue_recorded <dbl>, interpretation <chr>, cart_note <chr>
```

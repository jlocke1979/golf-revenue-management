# Round-weighted daily price from segment rows

Sums rounds by date and sets price to the round-weighted average of the
segment prices. That average is an average of prices paid. It is not a
posted rack rate. Other columns are kept when they do not vary within
the date. `segment` is dropped.

## Usage

``` r
aggregate_segment_rounds(data)
```

## Arguments

- data:

  A table with `date`, `segment`, `rounds`, and `price`, such as a
  segment sample from
  [`simulate_elasticity_sample()`](https://jlocke1979.github.io/golf-revenue-management/reference/simulate_elasticity_sample.md).

## Value

A tibble with one row per date.

## Examples

``` r
days <- data.frame(
  date = as.Date(c("2024-01-01", "2024-01-01")),
  segment = c("resident", "visitor"),
  rounds = c(10, 30),
  price = c(20, 40)
)
aggregate_segment_rounds(days)
#> # A tibble: 1 × 3
#>   date       rounds price
#>   <date>      <dbl> <dbl>
#> 1 2024-01-01     40    35
```

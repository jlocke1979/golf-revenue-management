# Daily tee times and player slots from an operating window

Counts starts inside the open window. The end clock closes the window.
It is not an extra start, and a window that ends at or before it starts
does not roll into the next day. That day has no starts. There is no
default interval and no daylight, weather, or closure adjustment.

## Usage

``` r
course_capacity(
  date,
  window_start,
  window_end,
  tee_interval_minutes,
  players_per_start
)
```

## Arguments

- date:

  Dates to schedule.

- window_start, window_end:

  Clock times as `"HH:MM"` or `"HH:MM:SS"`.

- tee_interval_minutes:

  Minutes between starts. Required. There is no default.

- players_per_start:

  Players on one start. Required. There is no default.

## Value

A tibble with one row per date, including `tee_times` and
`player_slots`.

## Details

Player slots are tee times multiplied by players per start. With blocked
rounds of zero, that product is the schedule estimate used by
[`apply_operating_capacity()`](https://jlocke1979.github.io/golf-revenue-management/reference/apply_operating_capacity.md).

## Examples

``` r
course_capacity(
  date = as.Date("2024-01-05"),
  window_start = "07:00",
  window_end = "15:00",
  tee_interval_minutes = 12,
  players_per_start = 4
)
#> # A tibble: 1 × 7
#>   date       window_start window_end tee_interval_minutes players_per_start
#>   <date>     <chr>        <chr>                     <dbl>             <dbl>
#> 1 2024-01-05 07:00        15:00                        12                 4
#> # ℹ 2 more variables: tee_times <dbl>, player_slots <dbl>
```

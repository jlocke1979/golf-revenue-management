# Daily tee-sheet use against a course capacity schedule

Compares groups on a tee sheet with the daily supply from
[`course_capacity()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_capacity.md).
One row is one group at one start. Groups that share a start count as
one occupied tee time, and their players add together. A single player
on a foursome start occupies the tee time and one player slot.

## Usage

``` r
daily_utilization(bookings, capacity)
```

## Arguments

- bookings:

  Tee-sheet rows with `date`, `tee_time` (`"HH:MM"` or `"HH:MM:SS"`),
  and `players`.

- capacity:

  A result from
  [`course_capacity()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_capacity.md),
  one row per date.

## Value

A tibble with one row per scheduled date, including `tee_time_occupancy`
and `slot_utilization`.

## Details

`tee_time_occupancy` is occupied tee times divided by scheduled tee
times. `slot_utilization` is players divided by player slots. Both are
shares. Neither is capped at 1. A day with no scheduled starts has no
share, because the denominator is zero. A day in the schedule with no
groups is zero use. A group on a date that is not in the schedule is an
error.

This is not the Dobson daylight comparison. That file has no tee time
and no players per group. Daily rounds stay on
[`utilization_calendar()`](https://jlocke1979.github.io/golf-revenue-management/reference/utilization_calendar.md).

## Examples

``` r
capacity <- course_capacity(
  date = as.Date("2024-06-01"),
  window_start = "07:00",
  window_end = "07:40",
  tee_interval_minutes = 10,
  players_per_start = 4
)
bookings <- data.frame(
  date = as.Date(c("2024-06-01", "2024-06-01", "2024-06-01")),
  tee_time = c("07:00", "07:00", "07:10"),
  players = c(4, 0, 1)
)
daily_utilization(bookings, capacity)
#> # A tibble: 1 × 7
#>   date       tee_times player_slots occupied_tee_times players
#>   <date>         <dbl>        <dbl>              <dbl>   <dbl>
#> 1 2024-06-01         4           16                  2       5
#> # ℹ 2 more variables: tee_time_occupancy <dbl>, slot_utilization <dbl>
```

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

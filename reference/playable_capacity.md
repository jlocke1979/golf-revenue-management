# Playable capacity from stated time, closures, or an optional weather scenario

`theoretical_capacity` is the capacity column you pass in. This function
does not change
[`course_capacity()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_capacity.md)
or `rounds_possible`.

## Usage

``` r
playable_capacity(
  data,
  capacity = "rounds_possible",
  calendar = NULL,
  unavailable_hours = NULL,
  daylight_hours = NULL,
  unavailable_fraction = NULL,
  weather = NULL,
  precip_unplayable_mm = NULL,
  precip_column = "precip_mm",
  wind_unplayable_ms = NULL,
  wind_column = "wind_speed_ms"
)
```

## Arguments

- data:

  A table with `date` and the capacity column.

- capacity:

  Column name of theoretical capacity, in rounds.

- calendar:

  Optional
  [`operating_calendar()`](https://jlocke1979.github.io/golf-revenue-management/reference/operating_calendar.md).

- unavailable_hours, daylight_hours:

  Column names. Both are required together. Hours cannot exceed daylight
  hours.

- unavailable_fraction:

  Column name of a stated fraction from 0 to 1.

- weather:

  Optional
  [`course_weather()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_weather.md)
  or
  [`prepare_dobson_weather()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_weather.md)
  table. Used only when a precipitation or wind threshold is set.

- precip_unplayable_mm:

  Positive precipitation threshold, in millimeters. NULL skips the
  scenario.

- precip_column:

  Precipitation column on `weather`. The name must end in `_mm`.

- wind_unplayable_ms:

  Positive wind threshold, in meters per second. NULL skips the
  scenario.

- wind_column:

  Wind column on `weather`. The name must end in `_ms`.

## Value

A `golfops_playable_capacity` tibble. The attribute `assumptions` states
which rules were applied.

## Details

The primary adjustments are a confirmed closure, a stated unavailable
fraction, or stated unavailable hours. Hours reduce theoretical capacity
in proportion to daylight hours. Those hours come from the table. They
are not inferred from a daily weather total.

A precipitation or wind threshold is a separate scenario. On a day that
meets it, playable capacity is set to zero for the whole day and the
source is `weather_assumption`. Lost hours stay missing. Pass that
scenario in its own call, not together with stated hours or a stated
fraction. High temperature is not an input and does not reduce capacity.

A calendar status of `closed` sets playable capacity to zero and is
recorded as `observed_closure`. That status wins over a weather scenario
on the same date.

## Examples

``` r
day <- data.frame(
  date = as.Date("2024-07-01"),
  rounds_possible = 240,
  daylight_hours = 14,
  unavailable_hours = 3
)
playable_capacity(
  day,
  unavailable_hours = "unavailable_hours",
  daylight_hours = "daylight_hours"
)
#> Playable capacity
#> - theoretical_capacity is the capacity column passed into this function. course_capacity() and rounds_possible are not modified.
#> - High temperature does not reduce playable capacity. Temperature can be compared with rounds when the question is demand.
#> - Unavailable hours reduce theoretical capacity in proportion to daylight hours. The hours are a statement in the table. They are not calculated from precipitation, wind, or temperature.
#> - A stated fraction is applied as given. It is not inferred from a daily weather total.
#> 
#> # A tibble: 1 × 8
#>   date       theoretical_capacity playable_capacity unavailable_fraction_applied
#>   <date>                    <dbl>             <dbl>                        <dbl>
#> 1 2024-07-01                  240              189.                        0.214
#> # ℹ 4 more variables: unavailable_hours_stated <dbl>, restriction <chr>,
#> #   restriction_source <chr>, weather_assumption_met <lgl>
```

# Optional operating calendar for one course

Supplies confirmed open or closed status and, when the course actually
has them, the operating window, tee interval, players per start, blocked
rounds, or a count of available rounds. No interval, opening hour, or
blocked-round value is filled in. A 15-minute tee interval is not
assumed.

## Usage

``` r
operating_calendar(
  date,
  course_id = NA_character_,
  status = "unknown",
  window_start = NA_character_,
  window_end = NA_character_,
  tee_interval_minutes = NA_real_,
  players_per_start = NA_real_,
  blocked_rounds = NA_real_,
  available_rounds = NA_real_
)
```

## Arguments

- date:

  Dates the calendar covers.

- course_id:

  Course identifier. Recycled when length 1.

- status:

  `open`, `closed`, or `unknown`.

- window_start, window_end:

  Clock times as `"HH:MM"` or `"HH:MM:SS"`. Both are required before a
  schedule estimate is calculated. An end at or before the start does
  not roll into the next day.

- tee_interval_minutes:

  Minutes between starts. Required for a schedule estimate. There is no
  default.

- players_per_start:

  Players on one start. Required for a schedule estimate. There is no
  default.

- blocked_rounds:

  Player-rounds held off the sellable inventory. A schedule estimate is
  not produced while this is missing, including when the caller might
  have meant zero.

- available_rounds:

  Observed or published sellable rounds. These take precedence over a
  schedule estimate and over the daylight proxy.

## Value

A `golfops_operating_calendar` tibble.

## Details

`available_rounds` is sellable player-rounds already counted by the
source. It is not a tee-time count unless the source has already
multiplied by players per start.

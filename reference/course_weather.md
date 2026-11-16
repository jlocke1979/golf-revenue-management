# Optional weather observations or forecast-time weather

A retrospective description may use weather that was observed on the
day. A forecast may use only weather that was available when the
prediction was made. This table records that timing. It does not
download a weather feed, and the Dobson model does not gain a weather
term until a real series is validated.

## Usage

``` r
course_weather(course_id, date, timing, source = "unknown", ...)
```

## Arguments

- course_id, date:

  Course and date.

- timing:

  `observed` or `available_at_forecast`.

- source:

  Where the values came from.

- ...:

  Optional weather columns, such as precipitation.

## Value

A `golfops_course_weather` tibble.

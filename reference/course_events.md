# Optional event list documented independently of the rounds file

An event name is attached only when the caller supplies it. The package
does not infer a charity tournament, a holiday, or a broadcast weekend
from a spike in rounds.

## Usage

``` r
course_events(course_id, date, event_name, source = "unknown")
```

## Arguments

- course_id, date:

  Course and date.

- event_name:

  Name from the external source.

- source:

  Where the event list came from.

## Value

A `golfops_course_events` tibble.

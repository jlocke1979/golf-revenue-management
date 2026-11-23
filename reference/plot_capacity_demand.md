# Capacity, demand, and utilization for each year

One row per year. The default draws three columns. `unit = "golfers"`
counts people. In the Dobson extract a round is one golfer, and capacity
is the city's daylight estimate of golfers. `unit = "slots"` counts tee
times. Slot capacity is that golfer estimate divided by
`players_per_slot`. Slot demand is the fewest slots that could hold the
golfers, rounding up. A group of 1, 2, or 3 golfers still uses a slot,
and this file does not record group size, so slots actually used are not
shown. `panels` chooses which columns to print.

## Usage

``` r
plot_capacity_demand(
  data,
  calendar = NULL,
  unit = c("golfers", "slots"),
  players_per_slot = 4,
  panels = c("capacity", "utilization", "heatmap")
)
```

## Arguments

- data:

  Daily rows from
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- calendar:

  Optional
  [`operating_calendar()`](https://jlocke1979.github.io/golf-revenue-management/reference/operating_calendar.md).

- unit:

  `golfers` or `slots`.

- players_per_slot:

  Golfers a full slot can hold. Used only for `slots`. This is not a
  group size from the extract.

- panels:

  Columns to print: `capacity`, `utilization`, and `heatmap`. The object
  still carries every column.

## Value

A `golfops_capacity_demand_plot`. Printing it draws one row per year.

## Details

The left column is capacity and demand, in ColorBrewer Dark2 gray and
green, on a January–December axis and a shared scale. The middle column
is utilization in ColorBrewer Dark2 purple, from 0 to 120 percent. A
cross marks a day above 120 percent. The right column is that year's
utilization heatmap. Its fill is ColorBrewer PRGn: green below the
median played day, purple above it, and red above 100 percent. A missing
date is black, and a zero-round day of unknown status is medium-dark
gray. A cross marks a heatmap day above 120 percent. A zero-round day of
unknown status and a missing date leave a gap in demand and in the
utilization line.

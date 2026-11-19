# Historical weather for the Dobson Ranch operating window

Reads the bundled NOAA GHCND extract. Temperature and wind are Phoenix
Sky Harbor (`USW00023183`), about 13.91 km from the course centroid.
Precipitation is the Mesa 4.3 SW CoCoRaHS gauge (`US1AZMR0260`), about
0.81 km away. That gauge does not report temperature or wind. Sky Harbor
precipitation is kept in `precip_mm_airport` and is not copied into the
local precipitation column.

## Usage

``` r
prepare_dobson_weather(path = NULL)
```

## Arguments

- path:

  Optional path to a weather CSV built by `data-raw/dobson_weather.R`.
  The station CSV must sit beside it.

## Value

A `golfops_course_weather` tibble. `timing` is `observed`. Attributes
`stations`, `coverage`, `units`, and `decisions` record provenance and
missingness.

## Details

GHCND stores temperature and precipitation and wind in tenths. This file
stores degrees Celsius, millimeters, and meters per second. `temp_avg_c`
is the average of the daily minimum and maximum. GHCND `TAVG` is empty
for Sky Harbor on every day in this window, so the average is not an
hourly mean.

A non-blank GHCND quality flag withdraws that value. The flag is kept. A
measurement flag of `T` is a trace: precipitation is 0 and
`precip_trace` is TRUE. Multiday accumulations are not assigned to one
date. Missing gauge days stay missing.

The extract was retrieved from NCEI on 2026-11-19 and restricted to
2021-07-01 through 2026-04-30, including 2024-12-10. Examples, tests,
and the vignette read this file. They do not call a weather service.
GHCND is a U.S. government work.

The course coordinate is the OpenStreetMap centroid of relation 326342,
retrieved the same day. It is not a surveyed tee.

## Examples

``` r
weather <- prepare_dobson_weather()
range(weather$date)
#> [1] "2021-07-01" "2026-04-30"
attr(weather, "coverage")
#> # A tibble: 6 × 4
#>   variable          n_days n_missing pct_missing
#>   <chr>              <int>     <int>       <dbl>
#> 1 temp_min_c          1765         0        0   
#> 2 temp_max_c          1765         0        0   
#> 3 temp_avg_c          1765         0        0   
#> 4 precip_mm           1765       139        7.88
#> 5 precip_mm_airport   1765         0        0   
#> 6 wind_speed_ms       1765         2        0.11
```

# sources/weather

These are unmodified date subsets of NOAA GHCND access files, retrieved on
2026-11-19. Rows are limited to 2021-07-01 through 2026-04-30. Values,
attribute fields, and column names are the NCEI fields. The package does not
read this folder. `data-raw/dobson_weather.R` converts units and writes
`inst/extdata/dobson_weather.csv`.

- `USW00023183.csv`: PHOENIX AIRPORT, AZ US. Temperature, wind, and airport precipitation.
- `US1AZMR0260.csv`: MESA 4.3 SW, AZ US. Local precipitation. This CoCoRaHS station does not report temperature or wind.

Full station files:

- <https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/USW00023183.csv>
- <https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/US1AZMR0260.csv>

GHCND is a U.S. government work and is in the public domain.

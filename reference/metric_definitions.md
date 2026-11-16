# Definitions for the participation and revenue metrics

These definitions are accounting identities on the city extract. They
are not posted prices, tee-sheet utilization, or causal effects of a
price change.

## Usage

``` r
metric_definitions()
```

## Value

A tibble with one row per metric.

## Examples

``` r
metric_definitions()
#> # A tibble: 9 × 4
#>   metric                           formula                   use   do_not_use_as
#>   <chr>                            <chr>                     <chr> <chr>        
#> 1 rounds_played                    Rounds Played, as publis… Coun… A booking, a…
#> 2 rounds_possible                  Rounds Possible, as publ… Dayl… Tee-sheet ca…
#> 3 utilization                      rounds_played / rounds_p… Shar… A booking ra…
#> 4 open_daylight_rounds             max(rounds_possible - ro… Unus… Sellable emp…
#> 5 green_fee_per_played_round       green-fee revenue on row… Real… A posted gre…
#> 6 total_revenue_per_played_round   total revenue on rows wi… Faci… A posted pri…
#> 7 green_fee_per_daylight_round     green-fee revenue, inclu… Gree… Revenue per …
#> 8 total_revenue_per_daylight_round total revenue, including… Faci… A minimum ac…
#> 9 booking_rate                     City field Round Booking… The … Evidence tha…
```

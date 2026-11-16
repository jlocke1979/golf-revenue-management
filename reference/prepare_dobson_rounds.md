# Prepare the City of Mesa Dobson Ranch rounds export

Reads the 23-column city extract, parses dates and currency, and
separates monthly totals from daily rows. In this extract the two grains
do not overlap: daily rows stay below 1,000 rounds and monthly rows stay
above 2,900. The default threshold of 1,000 sits in that gap. It is a
rule for this file, not a definition supplied by the catalog.

## Usage

``` r
prepare_dobson_rounds(
  path = dobson_rounds_example(),
  monthly_rounds_threshold = 1000
)
```

## Arguments

- path:

  Path to the city CSV. Defaults to the bundled example.

- monthly_rounds_threshold:

  Rounds-played cutoff used to label a row as a monthly total. Rows at
  or above the cutoff are `grain = "month"`.

## Value

A `golfops_rounds` tibble with one row per source record, including
duplicate dates. Duplicates are flagged and are not removed.

## Details

The catalog describes `Rounds Possible` as a daylight-based estimate,
not as tee-sheet capacity. `Round Booking Rate` is the percent of those
possible rounds that were played. Cart and club rental counts are
described as derived from revenue. Revenue per played round is
recomputed here. The city's `Revenue/Round` column is retained for
comparison.

## Examples

``` r
rounds <- prepare_dobson_rounds()
table(rounds$grain)
#> 
#>   day month 
#>  1766    72 
```

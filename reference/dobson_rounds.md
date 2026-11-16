# Dobson Ranch golf rounds and realized revenue

Activity and revenue for Dobson Ranch Golf Course, published by the City
of Mesa Parks, Recreation and Community Facilities. The city describes
the course as operated by Paradigm Golf Group. This object is the
open-data extract saved on 2026-10-03, dataset `24g5-hb8k`, after
[`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md)
parses dates, currency, and the city's percent field. Blank source cells
stay missing. No row is imputed, and the absent day 2024-12-10 is not
added.

## Usage

``` r
dobson_rounds
```

## Format

A `golfops_rounds` data frame with 1838 rows and 38 variables:

- row_id:

  City field RowID. The catalog calls it a unique identifier. The CSV
  prints thousands separators; those are removed and the value is stored
  as a number.

- date:

  City field Date, parsed to a date. The catalog says this is when the
  golf round was played. Monthly total rows are dated the first of the
  month.

- calendar_year:

  Calendar year of `date`. It equals the source Calendar Year on every
  row. The catalog description is "Calendar year".

- fiscal_year:

  City fiscal-year label, such as `FY15-16`. The catalog says the City
  of Mesa fiscal year is July 1 through June 30 and that the field uses
  the two-year designation. The label matches the source Fiscal Year on
  every row.

- month:

  Month of `date`, as a factor with levels January through December. It
  matches the source Month on every row.

- day:

  Day of the month from `date`. It matches the source Day on every row.

- weekday:

  Weekday of `date`, as a factor with levels Monday through Sunday. It
  matches the source Day of Week on every row.

- season:

  Source Season value, as a factor with levels Winter, Spring, Summer,
  and Fall. On every row it matches December–February as Winter,
  March–May as Spring, June–August as Summer, and September–November as
  Fall. The catalog description for Season is blank.

- sunrise:

  City field Sunrise, kept as the published clock string `HH:MM:SS`. The
  catalog says "Time of sunrise" and does not give a time zone. Missing
  on 67 rows, the same rows where `sunset` and `daylight_hours` are
  missing.

- sunset:

  City field Sunset, kept as the published clock string `HH:MM:SS`. The
  catalog says "Time of sunset" and does not give a time zone. Missing
  on the same 67 rows as `sunrise`.

- daylight_hours:

  Sunset minus sunrise, in decimal hours, reading each clock string as a
  time on a 24-hour clock. Computed by
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Missing on the same 67 rows as the clock strings. This column is not
  in the city catalog.

- grain:

  `day` or `month`. `month` means `rounds_played` is at least 1000. This
  file has 1766 `day` rows and 72 `month` rows. The cutoff is not a
  catalog field.

- rounds_played:

  City field Rounds Played. The catalog says "Number of rounds played".
  These are played rounds, not reserved tee times.

- rounds_possible:

  City field Rounds Possible. The catalog says this is the number of
  rounds that could be played, found from the hours of daylight
  available. It is the city's daylight estimate, not a count of tee
  times on a tee sheet. Missing on 67 rows.

- booking_rate:

  City field Round Booking Rate, divided by 100. The catalog defines it
  as the percent of possible rounds that were played, so the stored
  number is a proportion. On the 1771 rows where this value and
  `rounds_played / rounds_possible` are both defined, the largest
  absolute difference is 0.0005. The rate is above 1 on 109 rows. It is
  the city's played-over-possible percent, not a count or share of
  reserved tee times. Missing on 67 rows.

- utilization:

  `rounds_played / rounds_possible` where `rounds_possible` is positive,
  from
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  It is the share of the daylight estimate that was played. Missing on
  67 rows. This column is not in the city catalog.

- open_daylight_rounds:

  `max(rounds_possible - rounds_played, 0)` where `rounds_possible` is
  present. Days with more played rounds than the daylight estimate are
  stored as 0. This is the unused portion of the daylight estimate, not
  a count of open tee times. Missing on 67 rows. This column is not in
  the city catalog.

- cart_rentals:

  City field Cart Rentals. The catalog says "Number of carts used in
  golf rounds. Derived from Cart Revenue." Missing values stay missing.

- cart_revenue:

  City field Cart Revenue. The catalog says "Revenue earned from cart
  rentals." The CSV prints a dollar sign; the column stores the parsed
  number. This is realized cart revenue, not a posted cart price. A
  missing value is a blank source cell.

- club_rentals:

  City field Club Rentals. The catalog says "Number of club rentals.
  Derived from club rental revenue."

- club_revenue:

  City field Club Revenue. The catalog says "Revenue earned from golf
  club rentals." Parsed from the dollar-formatted CSV value. Realized
  club-rental revenue, not a posted rental price.

- food_beverage_revenue:

  City field Food and Beverage. The catalog says "Revenue earned from
  food and beverage sales." Parsed from the dollar-formatted CSV value.

- green_fee_revenue:

  City field Green Fees. The catalog says "Revenue earned from green
  fees." Parsed from the dollar-formatted CSV value. This is realized
  green-fee revenue, not a published green fee and not a price for an
  available tee time.

- merchandise_revenue:

  City field Merchandise Revenue. The catalog says "Revenue earned from
  merchandise revenue sales." Parsed from the dollar-formatted CSV
  value.

- range_revenue:

  City field Range Sales. The catalog says "Revenue earned from driving
  range use." Parsed from the dollar-formatted CSV value.

- ancillary_revenue:

  `total_revenue - green_fee_revenue` when both source amounts are
  present. Computed by
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Missing on the 48 rows where total revenue is missing. This column is
  not in the city catalog.

- total_revenue:

  City field Total Revenue. The catalog says it is the sum of cart
  revenue, club revenue, merchandise revenue, green fees, range sales,
  and food and beverage. Parsed from the dollar-formatted CSV value.
  Missing on 48 rows.

- component_revenue:

  Sum of cart, club, food and beverage, green fee, merchandise, and
  range revenue on rows where all six amounts are present. Computed by
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Missing on 164 rows, including rows where one or more category amounts
  are blank. This column is not in the city catalog.

- revenue_per_round_source:

  City field Revenue/Round. The catalog says "Calculation: Total
  Revenue/Rounds." On the 1734 rows where this value and
  `total_revenue / rounds_played` are both defined, the largest absolute
  difference is 0.01. It is facility revenue per played round. It is
  missing for all 56 zero-round rows and for the 48 rows with no total
  revenue. It is not a published price.

- green_fee_per_played_round:

  `green_fee_revenue / rounds_played` where `rounds_played` is positive.
  Computed by
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Realized green-fee revenue per played round. Missing on 104 rows. This
  column is not in the city catalog and is not a posted green fee.

- total_revenue_per_played_round:

  `total_revenue / rounds_played` where `rounds_played` is positive.
  Computed by
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Missing on 104 rows. This column is not in the city catalog.

- green_fee_per_daylight_round:

  `green_fee_revenue / rounds_possible` where `rounds_possible` is
  positive. Computed by
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Green-fee revenue per daylight-estimated round. Missing on 67 rows.
  This column is not in the city catalog and is not revenue per open tee
  time.

- total_revenue_per_daylight_round:

  `total_revenue / rounds_possible` where `rounds_possible` is positive.
  Computed by
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Facility revenue per daylight-estimated round. Missing on 67 rows.
  This column is not in the city catalog.

- flag_duplicate_date:

  `TRUE` when `date` appears on more than one row. Two rows are `TRUE`,
  both 2022-04-24, and they match on every other column.

- flag_zero_rounds:

  `TRUE` when `rounds_played` is 0. Fifty-six rows are `TRUE`. The
  extract does not give a reason.

- flag_above_daylight_capacity:

  `TRUE` when `rounds_played` is greater than `rounds_possible`. One
  hundred nine rows are `TRUE`: played rounds exceeded the city's
  daylight estimate.

- flag_calendar_mismatch:

  `TRUE` when the source Month, Day of Week, Day, Season, or Fiscal Year
  disagrees with the value computed from `date`. Every row is `FALSE` in
  this extract.

- flag_missing_total_revenue:

  `TRUE` when `total_revenue` is missing. Forty-eight rows are `TRUE`.

## Source

City of Mesa. "Dobson Ranch Golf Rounds." Mesa Open Data, dataset
24g5-hb8k, Parks, Recreation and Community Facilities. Catalog page:
<https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Dobson-Ranch-Golf-Rounds/24g5-hb8k>.
Column descriptions are the catalog field descriptions. The catalog
description of the dataset is: "Provides information on activity levels
and revenues at Dobson Ranch Golf Course, which is operated by Paradigm
Golf Group." The packaged file is the CSV saved 2026-10-03,
`Dobson_Ranch_Golf_Rounds_20261003.csv`, copied without changes to
`inst/extdata/dobson_ranch_golf_rounds.csv`. Both copies have SHA-256
`d6b53a8dc37e80c9ec10ec3bc08cd834f3d6155cdeb9809ce0870cd7c1be0079`. The
catalog lists the data as public and links terms of use at
<https://data.mesaaz.gov/stories/s/2dcd-j2nx/>.

## Details

The extract records rounds the city says were played and revenue the
city says was earned. It has no published price list, no inventory of
available tee times, and no reservation records. `rounds_played` is
play. `green_fee_revenue` and the other revenue columns are realized
revenue. `rounds_possible` is the city's daylight estimate.
`booking_rate` keeps the city's field name, Round Booking Rate, and
stores the city's definition of that field: the percent of possible
rounds that were played.

Every source row is kept. There are 1766 daily rows and 72 monthly
totals. The monthly rows run from 2015-07-01 through 2021-06-01 and are
dated the first of the month. In this file, daily rows have at most 675
rounds played and monthly rows have at least 2963, so `grain` labels a
row `month` when rounds played are at least 1000 and `day` otherwise.
That cutoff is the package rule stored in `monthly_rounds_threshold`. It
is not a field in the city catalog. Add daily rows to daily rows, and
monthly rows to monthly rows.

One daily row is dated 2004-12-10 and sits apart from the later series.
From 2021-07-01 through 2026-04-30 the only missing date is 2024-12-10.
The date 2022-04-24 occurs twice, and those two rows match on every
column except `row_id`.

`sunrise` and `sunset` keep the source clock strings. The catalog does
not give a time zone. Calendar labels are taken from `date` by
[`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md)
and match the source Calendar Year, Fiscal Year, Month, Day, Day of
Week, and Season values on every row.

Attributes `source_path`, `source_url`, and `source_accessed` record the
bundled CSV, the catalog page, and the 2026-10-03 save date. A catalog
metadata request on 2026-10-17 reported a later rows-updated time,
2026-10-17 02:17:12 UTC. This object was not replaced with that later
update.

## Examples

``` r
data(dobson_rounds)
str(dobson_rounds)
#> glfps_rn [1,838 × 38] (S3: golfops_rounds/tbl_df/tbl/data.frame)
#>  $ row_id                          : num [1:1838] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ date                            : Date[1:1838], format: "2004-12-10" "2015-07-01" ...
#>  $ calendar_year                   : num [1:1838] 2004 2015 2015 2015 2015 ...
#>  $ fiscal_year                     : chr [1:1838] "FY04-5" "FY15-16" "FY15-16" "FY15-16" ...
#>  $ month                           : Factor w/ 12 levels "January","February",..: 12 7 8 9 10 11 12 1 2 3 ...
#>  $ day                             : int [1:1838] 10 1 1 1 1 1 1 1 1 1 ...
#>  $ weekday                         : Factor w/ 7 levels "Monday","Tuesday",..: 5 3 6 2 4 7 2 5 1 2 ...
#>  $ season                          : Factor w/ 4 levels "Winter","Spring",..: 1 3 3 4 4 4 1 1 1 2 ...
#>  $ sunrise                         : chr [1:1838] NA NA NA NA ...
#>  $ sunset                          : chr [1:1838] NA NA NA NA ...
#>  $ daylight_hours                  : num [1:1838] NA NA NA NA NA NA NA NA NA NA ...
#>  $ grain                           : chr [1:1838] "day" "month" "month" "month" ...
#>  $ rounds_played                   : num [1:1838] 219 4492 3589 3867 5396 ...
#>  $ rounds_possible                 : num [1:1838] NA NA NA NA NA NA NA NA NA NA ...
#>  $ booking_rate                    : num [1:1838] NA NA NA NA NA NA NA NA NA NA ...
#>  $ utilization                     : num [1:1838] NA NA NA NA NA NA NA NA NA NA ...
#>  $ open_daylight_rounds            : num [1:1838] NA NA NA NA NA NA NA NA NA NA ...
#>  $ cart_rentals                    : num [1:1838] 166 NA NA NA NA NA NA NA NA NA ...
#>  $ cart_revenue                    : num [1:1838] 2656 NA NA NA NA ...
#>  $ club_rentals                    : num [1:1838] 4 NA NA NA NA NA NA NA NA NA ...
#>  $ club_revenue                    : num [1:1838] 60 NA NA NA NA NA NA NA NA NA ...
#>  $ food_beverage_revenue           : num [1:1838] 3878 NA NA NA NA ...
#>  $ green_fee_revenue               : num [1:1838] 12313 NA NA NA NA ...
#>  $ merchandise_revenue             : num [1:1838] 1812 NA NA NA NA ...
#>  $ range_revenue                   : num [1:1838] 5350 NA NA NA NA ...
#>  $ ancillary_revenue               : num [1:1838] 13756 NA NA NA NA ...
#>  $ total_revenue                   : num [1:1838] 26069 NA NA NA NA ...
#>  $ component_revenue               : num [1:1838] 26069 NA NA NA NA ...
#>  $ revenue_per_round_source        : num [1:1838] 119 NA NA NA NA ...
#>  $ green_fee_per_played_round      : num [1:1838] 56.2 NA NA NA NA ...
#>  $ total_revenue_per_played_round  : num [1:1838] 119 NA NA NA NA ...
#>  $ green_fee_per_daylight_round    : num [1:1838] NA NA NA NA NA NA NA NA NA NA ...
#>  $ total_revenue_per_daylight_round: num [1:1838] NA NA NA NA NA NA NA NA NA NA ...
#>  $ flag_duplicate_date             : logi [1:1838] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ flag_zero_rounds                : logi [1:1838] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ flag_above_daylight_capacity    : logi [1:1838] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ flag_calendar_mismatch          : logi [1:1838] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ flag_missing_total_revenue      : logi [1:1838] FALSE TRUE TRUE TRUE TRUE TRUE ...
#>  - attr(*, "monthly_rounds_threshold")= num 1000
#>  - attr(*, "source_path")= chr "inst/extdata/dobson_ranch_golf_rounds.csv"
#>  - attr(*, "source_url")= chr "https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Dobson-Ranch-Golf-Rounds/24g5-hb8k"
#>  - attr(*, "source_accessed")= chr "2026-10-03"
```

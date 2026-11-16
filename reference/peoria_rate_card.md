# Posted prices from the Peoria rate sheet

Ten products from the Peoria weekday and weekend price sheet, version 2:
Kellogg, Madison, and Newman, each with an 18-hole course and a front
nine and a back nine, plus Kellogg Executive. The sheet key for Newman
is spelled Neuman. Dollars are the posted cells. Empty cells are
omitted. The sheet has no rounds and no revenue.

## Usage

``` r
peoria_rate_card()
```

## Value

A data frame with one posted price per product, segment, day, and
daypart. `sheet_name` is the course key on the sheet. The attribute
`source` names the sheet.

## Details

Twilight is an adult daypart. It is posted on three products. On Kellogg
Executive and Madison Back 9 the twilight green fee is above that
product's day rate.

## Examples

``` r
card <- peoria_rate_card()
card[card$facility == "Kellogg" & card$product == "18 holes" &
  card$segment == "Adult", c("day", "daypart", "price")]
#>        day daypart price
#> 2  Weekday     Day    42
#> 39 Weekend     Day    42
```

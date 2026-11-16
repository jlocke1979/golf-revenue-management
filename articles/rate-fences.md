# Rate fences

``` r

library(golfops)
fences <- peoria_rate_fences()
card <- peoria_rate_card()
products <- unique(card[, c("facility", "product", "holes")])

lookup <- function(segment, day, daypart) {
  rows <- card[card$segment == segment & card$day == day & card$daypart == daypart, ]
  key <- paste(products$facility, products$product)
  rows$price[match(key, paste(rows$facility, rows$product))]
}

show_prices <- function(frame, price_cols) {
  for (col in price_cols) {
    frame[[col]] <- ifelse(is.na(frame[[col]]), "N/A", sprintf("%.2f", frame[[col]]))
  }
  frame
}
```

These are the posted prices on the Peoria weekday and weekend sheet,
version 2. Ten products are on the sheet. Kellogg, Madison, and Newman
each post an 18-hole course, a front nine, and a back nine. Kellogg also
posts Executive, a nine-hole course. The course key on the sheet spells
Newman as Neuman. The tables below use Newman.

A blank cell is a fence the sheet leaves empty. The sheet has no count
of rounds, so it has no revenue. Quantity from the earlier fictional
card is gone.

## Segments

| Segment | What is priced | Rule | On the sheet |
|:---|:---|:---|:---|
| Adult | Green fee for one round | Weekday and weekend each have an adult green fee on every product. | Posted |
| Adult twilight | Green fee for one round | Posted on Kellogg Executive, Kellogg Back 9, and Madison Back 9, for weekday and weekend. The other products leave the cell blank. | Posted |
| Senior | Green fee for one round | Weekday and weekend each have a senior green fee on every product. | Posted |
| Junior | Green fee for one round | Posted on Kellogg Executive and on each 18-hole course. The weekday fee and the weekend fee are the same. Front 9 and Back 9 leave the cell blank. | Posted |
| Junior twilight | Green fee for one round | The sheet has the column. Every cell is blank. | Blank |
| Military | Green fee for one round | The sheet has a weekday column and a weekend column. Every cell is blank. | Blank |
| Adult with cart | Green fee and cart for one adult round | A posted adult price that includes a cart. On a weekday it equals that product’s adult green fee plus its cart fee. On a weekend it is its own posted price. | Posted |
| Holiday 18 with cart | Green fee and cart for one 18-hole round | The column header names a Saturday and Sunday holiday rate for 18 holes with a cart, from May 11 through September 29. Every cell is blank. | Blank |
| Cart | Cart for one round | Eighteen-hole courses are \$17. Nine-hole products are \$12. The sheet does not split the cart fee by weekday or weekend. | Posted |
| Pull cart | Pull cart for one round | Five dollars on every product. | Posted |
| Club per club | One club | One dollar per club on every product. | Posted |
| Club rental | Club rental | Kellogg 18 and Newman 18 are \$20. Every other product, including Madison 18, is \$10. | Posted |

Fences on the Peoria sheet. Twilight is a daypart on the adult green
fee. Blank means every cell in that column is empty. {.table
style="width:100%;"}

## Weekday

Adult, senior, and junior are walking green fees. Twilight is the adult
twilight green fee. Adult with cart is the posted price for an adult
round that includes a cart. On a weekday that price equals the adult
green fee plus the cart fee, on every product.

| Facility | Product     | Holes | Adult | Twilight | Senior | Junior | Adult with cart |
|:---------|:------------|------:|:------|:---------|:-------|:-------|:----------------|
| Kellogg  | Executive 9 |     9 | 24.00 | 30.00    | 22.00  | 10.00  | 36.00           |
| Kellogg  | 18 holes    |    18 | 42.00 | N/A      | 36.00  | 15.00  | 59.00           |
| Kellogg  | Front 9     |     9 | 30.00 | N/A      | 28.00  | N/A    | 42.00           |
| Kellogg  | Back 9      |     9 | 30.00 | 20.00    | 28.00  | N/A    | 42.00           |
| Madison  | 18 holes    |    18 | 26.00 | N/A      | 24.00  | 15.00  | 43.00           |
| Madison  | Front 9     |     9 | 20.00 | N/A      | 20.00  | N/A    | 32.00           |
| Madison  | Back 9      |     9 | 20.00 | 27.00    | 20.00  | N/A    | 32.00           |
| Newman   | 18 holes    |    18 | 35.00 | N/A      | 32.00  | 15.00  | 52.00           |
| Newman   | Front 9     |     9 | 27.00 | N/A      | 26.00  | N/A    | 39.00           |
| Newman   | Back 9      |     9 | 27.00 | N/A      | 26.00  | N/A    | 39.00           |

Weekday posted prices, in dollars. N/A is an empty cell on the sheet.
{.table style="width:100%;"}

Twilight is posted on Kellogg Executive, Kellogg Back 9, and Madison
Back 9. On Kellogg Executive the twilight green fee is \$30.00 and the
day rate is \$24.00. On Madison Back 9 the twilight green fee is \$27.00
and the day rate is \$20.00. On Kellogg Back 9 twilight is \$20.00 and
the day rate is \$30.00.

## Weekend

The walking green fees match the weekday green fees on this sheet. The
adult-with-cart column does not. It is a separate posted price.

| Facility | Product     | Holes | Adult | Twilight | Senior | Junior | Adult with cart |
|:---------|:------------|------:|:------|:---------|:-------|:-------|:----------------|
| Kellogg  | Executive 9 |     9 | 24.00 | 30.00    | 22.00  | 10.00  | 48.00           |
| Kellogg  | 18 holes    |    18 | 42.00 | N/A      | 36.00  | 15.00  | 76.00           |
| Kellogg  | Front 9     |     9 | 30.00 | N/A      | 28.00  | N/A    | 54.00           |
| Kellogg  | Back 9      |     9 | 30.00 | 20.00    | 28.00  | N/A    | 54.00           |
| Madison  | 18 holes    |    18 | 26.00 | N/A      | 24.00  | 15.00  | 60.00           |
| Madison  | Front 9     |     9 | 20.00 | N/A      | 20.00  | N/A    | 44.00           |
| Madison  | Back 9      |     9 | 20.00 | 27.00    | 20.00  | N/A    | 44.00           |
| Newman   | 18 holes    |    18 | 35.00 | N/A      | 32.00  | 15.00  | 68.00           |
| Newman   | Front 9     |     9 | 27.00 | N/A      | 26.00  | N/A    | 51.00           |
| Newman   | Back 9      |     9 | 27.00 | N/A      | 26.00  | N/A    | 51.00           |

Weekend posted prices, in dollars. N/A is an empty cell on the sheet.
{.table style="width:100%;"}

## Carts and clubs

The cart fee follows the number of holes: \$17.00 on an 18-hole course
and \$12.00 on a nine-hole product. The pull cart is \$5.00 and a single
club is \$1.00, on every product. The club-rental fee is \$20.00 at
Kellogg 18 and Newman 18, and \$10.00 on the other products, including
Madison 18.

| Facility | Product     | Holes | Cart  | Pull cart | Club per club | Club rental |
|:---------|:------------|------:|:------|:----------|:--------------|:------------|
| Kellogg  | Executive 9 |     9 | 12.00 | 5.00      | 1.00          | 10.00       |
| Kellogg  | 18 holes    |    18 | 17.00 | 5.00      | 1.00          | 20.00       |
| Kellogg  | Front 9     |     9 | 12.00 | 5.00      | 1.00          | 10.00       |
| Kellogg  | Back 9      |     9 | 12.00 | 5.00      | 1.00          | 10.00       |
| Madison  | 18 holes    |    18 | 17.00 | 5.00      | 1.00          | 10.00       |
| Madison  | Front 9     |     9 | 12.00 | 5.00      | 1.00          | 10.00       |
| Madison  | Back 9      |     9 | 12.00 | 5.00      | 1.00          | 10.00       |
| Newman   | 18 holes    |    18 | 17.00 | 5.00      | 1.00          | 20.00       |
| Newman   | Front 9     |     9 | 12.00 | 5.00      | 1.00          | 10.00       |
| Newman   | Back 9      |     9 | 12.00 | 5.00      | 1.00          | 10.00       |

Cart and club fees, in dollars. The sheet does not split these by
weekday or weekend. {.table}

The card has 114 posted prices. Junior twilight, military, and the
holiday 18-hole rate with a cart are columns on the sheet with no prices
in them. The holiday header names May 11 through September 29.

A played round can still add fences. An adult weekday round with a cart
is already one posted price. A senior round with a cart is the senior
green fee plus the cart fee, and that sum is not its own row. The mix of
rounds across these fences needs a season of rounds labeled with the
product, the day, and the daypart. This sheet does not have that season.

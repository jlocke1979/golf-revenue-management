# 8. Rate fences

``` r

library(golfops)
card <- peoria_rate_card()
open <- dobson_operating_daily()
open <- open[open$rounds_played > 0, ]
weekday <- summarize_golf_rounds(open, by = "weekday", duplicates = "drop_extra")
yield_of <- function(day) {
  weekday$green_fee_per_played_round[as.character(weekday$period) == day]
}
dollars <- function(x) paste0("$", formatC(x, format = "f", digits = 2))
money <- function(x) {
  ifelse(is.na(x), "—", paste0("$", formatC(x, format = "f", digits = 0)))
}
one_price <- function(facility, product, segment, day, daypart) {
  hit <- card$price[
    card$facility == facility &
      card$product == product &
      card$segment == segment &
      card$day == day &
      card$daypart == daypart
  ]
  if (length(hit) == 0) NA_real_ else hit[[1]]
}
```

Dobson Ranch published a pricing guide on September 16, 2026, [Dobson
Ranch Green Fees
Explained](https://www.dobsonranchgolfclub.com/blog/dobson-ranch-green-fees-explained-mesa-az-2026-guide).
It is a course editorial page. It points to the City of Mesa fee
schedule and to the live checkout at `dobson-ranch.book.teeitup.golf`.
The operating extract ends April 30, 2026, so these dollars are later
than the rounds in the file.

The guide’s adult 18-hole range is \$17 to \$150, and it says most tee
times fall between \$39 and \$69. Season and time of day are the rows it
actually prices. Day of week is named as a driver, with no weekday
column. The guide calls the schedule dynamic and also says demand moves
the quote. It does not publish the rule that separates those two.

| Item                       | What the guide prints                    |
|:---------------------------|:-----------------------------------------|
| Snowbird morning, Nov–Apr  | Adult example \$69, range up to \$150    |
| Summer or shoulder mid-day | Adult example \$39, range down to \$17   |
| Twilight, about 2 PM       | Adult \$11–\$100, any season             |
| Sunset                     | Adult \$9–\$39, any season               |
| Junior, 18 holes           | \$12–\$60                                |
| Ranch Card                 | \$109 for 2026, \$99 for 2027            |
| Smash Pass                 | \$24.99 a month, includes the Ranch Card |

Dobson Ranch Golf Club, September 16, 2026. Planning examples and
published ranges. The per-round card discount is not on the page.
{.table}

The green fee is course access. Cart, clubs, and range balls are
separate, and the cart fee is not itemized. Senior, resident, and
military rates are not on the page. On played days in the extract,
Friday’s collected green fee is \$40.69 a round and Sunday’s is \$31.72.
[Yield per
round](https://jlocke1979.github.io/golf-revenue-management/articles/dollars-per-golfer.md)
is that receipt. The extract has no rate code.

## Peoria

[`peoria_rate_card()`](https://jlocke1979.github.io/golf-revenue-management/reference/peoria_rate_card.md)
is a transcription of a sheet labeled “Peoria weekday and weekend
prices, version 2.” The original file is not in the package. These are
not Dobson Ranch or City of Mesa rates. The sheet key spells Newman as
Neuman.

Walking fees are the same on weekdays and weekends. The weekend
difference is the adult price that includes a cart. Front nine and back
nine carry the same walking and cart-included fees, so each course is
one nine-hole row. Twilight is the exception: it is posted on Kellogg
Executive, Kellogg Back 9, and Madison Back 9 only.

| Course | Product | Adult | Senior | Junior | Weekday with cart | Weekend with cart | Twilight |
|:---|:---|:---|:---|:---|:---|:---|:---|
| Kellogg | Executive 9 | \$24 | \$22 | \$10 | \$36 | \$48 | \$30 |
| Kellogg | 18 holes | \$42 | \$36 | \$15 | \$59 | \$76 | — |
| Kellogg | 9 holes | \$30 | \$28 | — | \$42 | \$54 | Back 9 \$20 |
| Madison | 18 holes | \$26 | \$24 | \$15 | \$43 | \$60 | — |
| Madison | 9 holes | \$20 | \$20 | — | \$32 | \$44 | Back 9 \$27 |
| Newman | 18 holes | \$35 | \$32 | \$15 | \$52 | \$68 | — |
| Newman | 9 holes | \$27 | \$26 | — | \$39 | \$51 | — |

Peoria walking fees, in dollars. Adult, senior, and junior are the same
on weekdays and weekends. Nine-hole rows use the front nine, which
matches the back nine except for twilight. {.table}

A cart is \$17 on 18 holes and \$12 on nine holes. A pull cart is \$5,
and one club is \$1. Club rental is \$20 at Kellogg 18 and Newman 18,
and \$10 on the other products. On a weekday, adult-with-cart equals the
walking fee plus the cart. On Kellogg Executive and Madison Back 9,
twilight is above the daytime walking fee. On Kellogg Back 9 it is \$20,
below the \$30 daytime fee. Junior twilight, military, and the holiday
18-hole rate with a cart are empty columns. The holiday header is May 11
through September 29.

The extract has no quoted price, so this page does not estimate how
rounds respond to a rate. The synthetic section in [Weather and playable
capacity](https://jlocke1979.github.io/golf-revenue-management/articles/weather-and-elasticity.md)
checks a known equation. It does not use this guide.

#' Rate fences on the Peoria posted card
#'
#' Each row is one column on the Peoria weekday and weekend price sheet,
#' version 2. A fence is one posted price for one kind of round. Blank
#' columns stay in this list because the sheet has the column and leaves
#' every cell empty.
#'
#' @return A data frame of fence names, what is priced, and the rule.
#'   `posted` is FALSE when every cell in that column is empty.
#' @export
#' @examples
#' peoria_rate_fences()
peoria_rate_fences <- function() {
  data.frame(
    segment = c(
      "Adult",
      "Adult twilight",
      "Senior",
      "Junior",
      "Junior twilight",
      "Military",
      "Adult with cart",
      "Holiday 18 with cart",
      "Cart",
      "Pull cart",
      "Club per club",
      "Club rental"
    ),
    prices = c(
      "Green fee for one round",
      "Green fee for one round",
      "Green fee for one round",
      "Green fee for one round",
      "Green fee for one round",
      "Green fee for one round",
      "Green fee and cart for one adult round",
      "Green fee and cart for one 18-hole round",
      "Cart for one round",
      "Pull cart for one round",
      "One club",
      "Club rental"
    ),
    rule = c(
      "Weekday and weekend each have an adult green fee on every product.",
      "Posted on Kellogg Executive, Kellogg Back 9, and Madison Back 9, for weekday and weekend. The other products leave the cell blank.",
      "Weekday and weekend each have a senior green fee on every product.",
      "Posted on Kellogg Executive and on each 18-hole course. The weekday fee and the weekend fee are the same. Front 9 and Back 9 leave the cell blank.",
      "The sheet has the column. Every cell is blank.",
      "The sheet has a weekday column and a weekend column. Every cell is blank.",
      "A posted adult price that includes a cart. On a weekday it equals that product's adult green fee plus its cart fee. On a weekend it is its own posted price.",
      "The column header names a Saturday and Sunday holiday rate for 18 holes with a cart, from May 11 through September 29. Every cell is blank.",
      "Eighteen-hole courses are $17. Nine-hole products are $12. The sheet does not split the cart fee by weekday or weekend.",
      "Five dollars on every product.",
      "One dollar per club on every product.",
      "Kellogg 18 and Newman 18 are $20. Every other product, including Madison 18, is $10."
    ),
    posted = c(
      TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE
    ),
    stringsAsFactors = FALSE
  )
}

#' Posted prices from the Peoria rate sheet
#'
#' Ten products from the Peoria weekday and weekend price sheet, version 2:
#' Kellogg, Madison, and Newman, each with an 18-hole course and a front nine
#' and a back nine, plus Kellogg Executive. The sheet key for Newman is
#' spelled Neuman. Dollars are the posted cells. Empty cells are omitted.
#' The sheet has no rounds and no revenue.
#'
#' Twilight is an adult daypart. It is posted on three products. On Kellogg
#' Executive and Madison Back 9 the twilight green fee is above that
#' product's day rate.
#'
#' @return A data frame with one posted price per product, segment, day, and
#'   daypart. `sheet_name` is the course key on the sheet. The attribute
#'   `source` names the sheet.
#' @export
#' @examples
#' card <- peoria_rate_card()
#' card[card$facility == "Kellogg" & card$product == "18 holes" &
#'   card$segment == "Adult", c("day", "daypart", "price")]
peoria_rate_card <- function() {
  # Row order is the sheet order.
  products <- data.frame(
    facility_id = c(1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
    facility = c(
      "Kellogg", "Kellogg", "Kellogg", "Kellogg",
      "Madison", "Madison", "Madison",
      "Newman", "Newman", "Newman"
    ),
    product = c(
      "Executive 9", "18 holes", "Front 9", "Back 9",
      "18 holes", "Front 9", "Back 9",
      "18 holes", "Front 9", "Back 9"
    ),
    holes = c(9, 18, 9, 9, 18, 9, 9, 18, 9, 9),
    sheet_name = c(
      "Kellogg_Executive_09",
      "Kellogg_18_Hole_Course",
      "Kellogg_Front_09",
      "Kellogg_Back_09",
      "Madison_18_Hole_Course",
      "Madison_Front_09",
      "Madison_Back_09",
      "Neuman_18_Hole_Course",
      "Neuman_Front_09",
      "Neuman_Back_09"
    ),
    adult_weekday = c(24, 42, 30, 30, 26, 20, 20, 35, 27, 27),
    twilight_weekday = c(30, NA, NA, 20, NA, NA, 27, NA, NA, NA),
    senior_weekday = c(22, 36, 28, 28, 24, 20, 20, 32, 26, 26),
    junior_weekday = c(10, 15, NA, NA, 15, NA, NA, 15, NA, NA),
    with_cart_weekday = c(36, 59, 42, 42, 43, 32, 32, 52, 39, 39),
    adult_weekend = c(24, 42, 30, 30, 26, 20, 20, 35, 27, 27),
    twilight_weekend = c(30, NA, NA, 20, NA, NA, 27, NA, NA, NA),
    senior_weekend = c(22, 36, 28, 28, 24, 20, 20, 32, 26, 26),
    junior_weekend = c(10, 15, NA, NA, 15, NA, NA, 15, NA, NA),
    with_cart_weekend = c(48, 76, 54, 54, 60, 44, 44, 68, 51, 51),
    cart = c(12, 17, 12, 12, 17, 12, 12, 17, 12, 12),
    pull_cart = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
    club_per_club = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    club_rental = c(10, 20, 10, 10, 10, 10, 10, 20, 10, 10),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    column = c(
      "adult_weekday", "twilight_weekday", "senior_weekday", "junior_weekday",
      "with_cart_weekday",
      "adult_weekend", "twilight_weekend", "senior_weekend", "junior_weekend",
      "with_cart_weekend",
      "cart", "pull_cart", "club_per_club", "club_rental"
    ),
    segment = c(
      "Adult", "Adult", "Senior", "Junior", "Adult with cart",
      "Adult", "Adult", "Senior", "Junior", "Adult with cart",
      "Cart", "Pull cart", "Club per club", "Club rental"
    ),
    day = c(
      "Weekday", "Weekday", "Weekday", "Weekday", "Weekday",
      "Weekend", "Weekend", "Weekend", "Weekend", "Weekend",
      "Any", "Any", "Any", "Any"
    ),
    daypart = c(
      "Day", "Twilight", "Day", "Day", "Day",
      "Day", "Twilight", "Day", "Day", "Day",
      "Any", "Any", "Any", "Any"
    ),
    unit = c(
      "per round", "per round", "per round", "per round", "per round",
      "per round", "per round", "per round", "per round", "per round",
      "per round", "per round", "per club", "club rental"
    ),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(spec)), function(i) {
    price <- products[[spec$column[i]]]
    keep <- !is.na(price)
    data.frame(
      facility_id = products$facility_id[keep],
      facility = products$facility[keep],
      product = products$product[keep],
      holes = products$holes[keep],
      sheet_name = products$sheet_name[keep],
      segment = spec$segment[i],
      day = spec$day[i],
      daypart = spec$daypart[i],
      price = price[keep],
      unit = spec$unit[i],
      stringsAsFactors = FALSE
    )
  })
  card <- do.call(rbind, rows)
  card$segment <- factor(
    card$segment,
    levels = c(
      "Adult", "Senior", "Junior", "Adult with cart",
      "Cart", "Pull cart", "Club per club", "Club rental"
    )
  )
  rownames(card) <- NULL
  attr(card, "source") <- "Peoria weekday and weekend prices, version 2"
  card
}

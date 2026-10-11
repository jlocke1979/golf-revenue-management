#' Rate fences on a synthetic park-district card
#'
#' Each row is one fence. A fence is one posted price for one kind of round.
#' Daily adult and senior fees differ by course, by weekday or weekend, and
#' by day or twilight. Young adult and junior fees differ by course only.
#' A pass is one season price, and a round played on that pass has no further
#' green fee. The cart is a flat fee on top of the green fee.
#'
#' @return A data frame of segment names and the rule for each.
#' @export
synthetic_rate_fences <- function() {
  data.frame(
    segment = c(
      "Adult daily",
      "Senior daily",
      "Young adult daily",
      "Junior daily",
      "Twilight",
      "Adult pass",
      "Senior pass",
      "Young adult pass",
      "Cart"
    ),
    prices = c(
      "Green fee for one round",
      "Green fee for one round",
      "Green fee for one round",
      "Green fee for one round",
      "Green fee for one round after the posted hour",
      "One payment for the season",
      "One payment for the season",
      "One payment for the season",
      "Cart for one round, added to the green fee"
    ),
    rule = c(
      "An adult without a pass. Weekday and weekend each have a day rate.",
      "The district's senior age, without a pass. Below the adult rate for the same course, day, and daypart.",
      "The district's young-adult age, without a pass. One fee per course, all week and all day.",
      "The district's junior age, without a pass. One fee per course, all week and all day.",
      "A lower adult or senior green fee after a posted hour. The same hour on weekday and weekend. Young adult and junior do not have a separate twilight fee.",
      "One season price for an adult. A round on the pass has a green fee of zero at every course, including twilight.",
      "One season price for a senior. A round on the pass has a green fee of zero at every course, including twilight.",
      "One season price for a young adult. A round on the pass has a green fee of zero at every course, including twilight.",
      "A flat cart fee for 9 holes or for 18 holes. It does not change at twilight. A riding round pays this plus the green fee."
    ),
    stringsAsFactors = FALSE
  )
}

.synthetic_twilight_price <- function(day_price) {
  as.integer(round(day_price * 0.75))
}

.synthetic_round_quantity <- function(course, segment, day, daypart) {
  base <- c(
    "Adult daily|Weekday|Day" = 4800,
    "Adult daily|Weekday|Twilight" = 1600,
    "Adult daily|Weekend|Day" = 3600,
    "Adult daily|Weekend|Twilight" = 800,
    "Senior daily|Weekday|Day" = 2000,
    "Senior daily|Weekday|Twilight" = 2400,
    "Senior daily|Weekend|Day" = 1200,
    "Senior daily|Weekend|Twilight" = 1000,
    "Young adult daily|Any|All day" = 1800,
    "Junior daily|Any|All day" = 600
  )
  scale <- c(Ridge = 1, Prairie = 0.8, Oak = 0.55, Orchard = 0.35)
  key <- paste(segment, day, daypart, sep = "|")
  as.integer(round(unname(base[key]) * unname(scale[course])))
}

.attach_synthetic_sales <- function(rows) {
  rows$quantity <- .synthetic_round_quantity(
    rows$course,
    as.character(rows$segment),
    rows$day,
    rows$daypart
  )
  rows$revenue <- rows$price * rows$quantity
  rows
}

#' Synthetic prices for the park-district fences
#'
#' Invented prices for three fictional 18-hole courses and one fictional
#' 9-hole course. Ridge is the base 18-hole card. Prairie is 85 percent of
#' Ridge's day rates and Oak is 70 percent, rounded to the dollar. Twilight
#' is 75 percent of that course's own day rate for the same golfer and the
#' same weekday or weekend. Each row also carries an invented season quantity
#' and the revenue from that quantity. The dollars are not a park district's
#' rate card and not a survey of Central Illinois.
#'
#' @return A data frame with one posted price per course, segment, day, and
#'   daypart. `quantity` is an invented season total: rounds for a green fee,
#'   passes sold for a pass, and cart rentals for a cart. `revenue` is price
#'   times quantity. The attribute `synthetic` is TRUE.
#' @export
synthetic_park_rate_card <- function() {
  fences <- synthetic_rate_fences()
  ridge <- data.frame(
    segment = c("Adult daily", "Senior daily", "Young adult daily", "Junior daily"),
    weekday = c(48, 38, 32, 16),
    weekend = c(58, 46, 32, 16),
    stringsAsFactors = FALSE
  )
  orchard <- data.frame(
    segment = c("Adult daily", "Senior daily", "Young adult daily", "Junior daily"),
    weekday = c(18, 14, 14, 8),
    weekend = c(22, 16, 14, 8),
    stringsAsFactors = FALSE
  )
  courses <- data.frame(
    course = c("Ridge", "Prairie", "Oak", "Orchard"),
    holes = c(18, 18, 18, 9),
    scale = c(1, 0.85, 0.70, NA_real_),
    stringsAsFactors = FALSE
  )
  split_daypart <- c("Adult daily", "Senior daily")
  pieces <- lapply(seq_len(nrow(courses)), function(i) {
    base <- if (courses$course[i] == "Orchard") orchard else ridge
    day_prices <- base
    if (courses$course[i] != "Orchard") {
      day_prices$weekday <- as.integer(round(base$weekday * courses$scale[i]))
      day_prices$weekend <- as.integer(round(base$weekend * courses$scale[i]))
    }
    rows <- lapply(seq_len(nrow(day_prices)), function(j) {
      segment <- day_prices$segment[j]
      if (segment %in% split_daypart) {
        day_rates <- data.frame(
          course = courses$course[i],
          holes = courses$holes[i],
          segment = segment,
          day = c("Weekday", "Weekend"),
          daypart = "Day",
          price = c(day_prices$weekday[j], day_prices$weekend[j]),
          unit = "per round",
          stringsAsFactors = FALSE
        )
        twilight <- day_rates
        twilight$daypart <- "Twilight"
        twilight$price <- .synthetic_twilight_price(day_rates$price)
        rbind(day_rates, twilight)
      } else {
        data.frame(
          course = courses$course[i],
          holes = courses$holes[i],
          segment = segment,
          day = "Any",
          daypart = "All day",
          price = day_prices$weekday[j],
          unit = "per round",
          stringsAsFactors = FALSE
        )
      }
    })
    do.call(rbind, rows)
  })
  rounds <- .attach_synthetic_sales(do.call(rbind, pieces))
  passes <- data.frame(
    course = "All courses",
    holes = NA_integer_,
    segment = c("Adult pass", "Senior pass", "Young adult pass"),
    day = "Any",
    daypart = "Any",
    price = c(600, 480, 300),
    unit = "per season",
    quantity = c(420, 380, 210),
    stringsAsFactors = FALSE
  )
  passes$revenue <- passes$price * passes$quantity
  carts <- data.frame(
    course = "All courses",
    holes = c(18, 9),
    segment = "Cart",
    day = "Any",
    daypart = "Any",
    price = c(18, 10),
    unit = "per round",
    quantity = c(
      as.integer(round(0.72 * sum(rounds$quantity[rounds$holes == 18]))),
      as.integer(round(0.45 * sum(rounds$quantity[rounds$holes == 9])))
    ),
    stringsAsFactors = FALSE
  )
  carts$revenue <- carts$price * carts$quantity
  card <- rbind(rounds, passes, carts)
  card$segment <- factor(card$segment, levels = setdiff(fences$segment, "Twilight"))
  card <- card[order(card$holes, card$course, card$segment, card$day, card$daypart), , drop = FALSE]
  rownames(card) <- NULL
  attr(card, "synthetic") <- TRUE
  card
}

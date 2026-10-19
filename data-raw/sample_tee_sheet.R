# Rebuild data/sample_tee_sheet.rda.
#
# From the package root:
#   Rscript -e 'options(golfops.build_sample_tee_sheet = TRUE); source("data-raw/sample_tee_sheet.R")'
#
# Sourcing the file only defines generate_sample_tee_sheet(). The data file is
# written when golfops.build_sample_tee_sheet is TRUE.

generate_sample_tee_sheet <- function(seed = 704L) {
  rng_previous <- RNGkind(
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  on.exit(
    RNGkind(
      kind = rng_previous[1],
      normal.kind = rng_previous[2],
      sample.kind = rng_previous[3]
    ),
    add = TRUE
  )
  set.seed(seed)

  as_of <- as.Date("2026-06-19")
  dates <- seq(as.Date("2026-06-08"), as.Date("2026-06-21"), by = "day")
  interval <- 10L
  players_per_start <- 4L
  weekday_window_start <- "07:00"
  weekday_window_end <- "18:00"
  weekend_window_start <- "06:30"
  weekend_window_end <- "18:30"

  # Synthetic municipal green fees, in dollars per player. These are not the
  # Peoria rate card and not Dobson Ranch realized yields.
  prices <- rbind(
    standard = c(36, 46, 22, 28),
    senior = c(28, 36, 18, 22),
    junior = c(12, 14, 8, 8),
    member = c(20, 26, 14, 16),
    promotional = c(26, 34, 16, 20)
  )
  colnames(prices) <- c("weekday_18", "weekend_18", "weekday_9", "weekend_9")
  rate_card <- do.call(rbind, lapply(rownames(prices), function(rate) {
    data.frame(
      rate_type = rate,
      day = c("weekday", "weekend", "weekday", "weekend"),
      holes = c(18L, 18L, 9L, 9L),
      price_per_player = as.numeric(prices[rate, ]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(rate_card) <- NULL

  price_for <- function(rate, weekend, holes) {
    column <- paste0(if (weekend) "weekend" else "weekday", "_", holes)
    unname(prices[rate, column])
  }
  draw_one <- function(labels, weights) {
    labels[[sample.int(length(labels), 1L, prob = weights / sum(weights))]]
  }
  clock_label <- function(minutes) {
    sprintf("%02d:%02d", minutes %/% 60L, minutes %% 60L)
  }
  daypart <- function(minutes) {
    if (minutes < 9L * 60L) {
      "early"
    } else if (minutes < 12L * 60L) {
      "morning"
    } else if (minutes < 15L * 60L) {
      "midday"
    } else {
      "afternoon"
    }
  }
  lead_time <- function(channel, play_date) {
    lead <- switch(
      channel,
      online = min(as.integer(stats::rgeom(1, 0.22)), 21L),
      phone = min(as.integer(stats::rgeom(1, 0.40)), 10L),
      walk_in = 0L,
      stop("Unknown booking channel.", call. = FALSE)
    )
    booked <- play_date - lead
    if (booked > as_of) {
      booked <- as_of
    }
    booked
  }

  occupancy <- list(
    early = c(weekday = 0.78, weekend = 0.94),
    morning = c(weekday = 0.62, weekend = 0.86),
    midday = c(weekday = 0.42, weekend = 0.68),
    afternoon = c(weekday = 0.28, weekend = 0.48)
  )
  player_weights <- list(
    early = c(0.05, 0.15, 0.35, 0.45),
    morning = c(0.10, 0.22, 0.38, 0.30),
    midday = c(0.18, 0.34, 0.30, 0.18),
    afternoon = c(0.28, 0.40, 0.22, 0.10)
  )
  rate_labels <- c("standard", "senior", "junior", "member", "promotional")
  channel_labels <- c("online", "phone", "walk_in")

  course_id <- character()
  play_date <- as.Date(character())
  tee_time <- character()
  players_booked <- integer()
  players_checked_in <- integer()
  holes <- integer()
  booking_status <- character()
  booking_channel <- character()
  rate_type <- character()
  price_per_player <- numeric()
  booking_date <- as.Date(character())
  revenue <- numeric()
  seq_no <- integer()
  next_seq <- 0L

  add_booking <- function(
      date, minutes, players, status, channel, rate, hole_count,
      price, booked_on, checked_in, amount) {
    next_seq <<- next_seq + 1L
    course_id <<- c(course_id, "sample_municipal")
    play_date <<- c(play_date, date)
    tee_time <<- c(tee_time, clock_label(minutes))
    players_booked <<- c(players_booked, as.integer(players))
    players_checked_in <<- c(players_checked_in, as.integer(checked_in))
    holes <<- c(holes, as.integer(hole_count))
    booking_status <<- c(booking_status, status)
    booking_channel <<- c(booking_channel, channel)
    rate_type <<- c(rate_type, rate)
    price_per_player <<- c(price_per_player, as.numeric(price))
    booking_date <<- c(booking_date, booked_on)
    revenue <<- c(revenue, as.numeric(amount))
    seq_no <<- c(seq_no, next_seq)
  }

  choose_rate <- function(part, weekend) {
    weights <- if (weekend) {
      c(0.58, 0.16, 0.06, 0.10, 0.10)
    } else {
      c(0.42, 0.28, 0.08, 0.14, 0.08)
    }
    if (part == "afternoon") {
      weights[2] <- weights[2] * 0.7
      weights[5] <- weights[5] * 3
    }
    draw_one(rate_labels, weights)
  }
  choose_channel <- function(future) {
    weights <- if (future) c(0.72, 0.28, 0) else c(0.58, 0.27, 0.15)
    draw_one(channel_labels, weights)
  }
  choose_holes <- function(part) {
    p_nine <- if (part == "afternoon") 0.45 else 0.08
    if (stats::runif(1) < p_nine) 9L else 18L
  }
  split_players <- function(players) {
    if (players == 2L) {
      return(c(1L, 1L))
    }
    if (players == 3L) {
      return(if (stats::runif(1) < 0.5) c(1L, 2L) else c(2L, 1L))
    }
    draw <- sample.int(3L, 1L)
    if (draw == 1L) c(2L, 2L) else if (draw == 2L) c(1L, 3L) else c(3L, 1L)
  }

  for (date_index in seq_along(dates)) {
    # for (date in dates) would drop the Date class.
    date <- dates[[date_index]]
    weekend <- as.POSIXlt(date)$wday %in% c(0L, 6L)
    day_key <- if (weekend) "weekend" else "weekday"
    start_min <- if (weekend) 6L * 60L + 30L else 7L * 60L
    end_min <- if (weekend) 18L * 60L + 30L else 18L * 60L
    future <- date > as_of
    for (minutes in seq(start_min, end_min - interval, by = interval)) {
      part <- daypart(minutes)
      # A cancelled reservation is drawn first so it keeps the earlier id
      # when a later reservation refills the same start.
      if (stats::runif(1) < 0.08) {
        cancelled_players <- draw_one(1:4, c(0.20, 0.35, 0.25, 0.20))
        cancelled_holes <- choose_holes(part)
        cancelled_rate <- choose_rate(part, weekend)
        cancelled_channel <- choose_channel(future)
        add_booking(
          date = date,
          minutes = minutes,
          players = cancelled_players,
          status = "cancelled",
          channel = cancelled_channel,
          rate = cancelled_rate,
          hole_count = cancelled_holes,
          price = price_for(cancelled_rate, weekend, cancelled_holes),
          booked_on = lead_time(cancelled_channel, date),
          checked_in = 0L,
          amount = 0
        )
      }
      if (stats::runif(1) >= occupancy[[part]][[day_key]]) {
        next
      }
      players <- draw_one(1:4, player_weights[[part]])
      groups <- if (players >= 2L && stats::runif(1) < 0.30) {
        split_players(players)
      } else {
        players
      }
      for (group_players in groups) {
        group_holes <- choose_holes(part)
        group_rate <- choose_rate(part, weekend)
        group_channel <- choose_channel(future)
        group_price <- price_for(group_rate, weekend, group_holes)
        if (future) {
          status <- "reserved"
          checked_in <- 0L
          amount <- NA_real_
        } else {
          noshow_p <- switch(
            group_channel,
            online = 0.09,
            phone = 0.06,
            walk_in = 0.02
          )
          if (stats::runif(1) < noshow_p) {
            status <- "no_show"
            checked_in <- 0L
            amount <- 0
          } else {
            status <- "completed"
            checked_in <- group_players
            if (group_players >= 2L && stats::runif(1) < 0.12) {
              checked_in <- group_players - 1L
            }
            amount <- checked_in * group_price
          }
        }
        add_booking(
          date = date,
          minutes = minutes,
          players = group_players,
          status = status,
          channel = group_channel,
          rate = group_rate,
          hole_count = group_holes,
          price = group_price,
          booked_on = lead_time(group_channel, date),
          checked_in = checked_in,
          amount = amount
        )
      }
    }
  }

  order_index <- order(play_date, tee_time, seq_no)
  sheet <- data.frame(
    course_id = course_id[order_index],
    play_date = play_date[order_index],
    tee_time = tee_time[order_index],
    booking_id = sprintf("B-%05d", seq_along(order_index)),
    players_booked = players_booked[order_index],
    players_checked_in = players_checked_in[order_index],
    max_players = rep(players_per_start, length(order_index)),
    holes = holes[order_index],
    booking_status = factor(
      booking_status[order_index],
      levels = c("reserved", "completed", "cancelled", "no_show")
    ),
    booking_channel = factor(
      booking_channel[order_index],
      levels = c("online", "phone", "walk_in")
    ),
    rate_type = factor(
      rate_type[order_index],
      levels = rate_labels
    ),
    price_per_player = price_per_player[order_index],
    booking_date = booking_date[order_index],
    revenue = revenue[order_index],
    stringsAsFactors = FALSE
  )
  rownames(sheet) <- NULL

  active <- sheet$booking_status != "cancelled"
  active_key <- paste(sheet$play_date, sheet$tee_time)[active]
  held_players <- tapply(sheet$players_booked[active], active_key, sum)
  stopifnot(
    as.POSIXlt(as.Date("2026-06-08"))$wday == 1L,
    as.POSIXlt(as_of)$wday == 5L,
    as.POSIXlt(as.Date("2026-06-21"))$wday == 0L,
    nrow(sheet) > 0L,
    !anyDuplicated(sheet$booking_id),
    all(sheet$players_booked >= 1L, sheet$players_booked <= sheet$max_players),
    all(sheet$players_checked_in >= 0L),
    all(sheet$players_checked_in <= sheet$players_booked),
    all(sheet$players_checked_in[sheet$booking_status != "completed"] == 0L),
    all(sheet$players_checked_in[sheet$booking_status == "completed"] >= 1L),
    all(sheet$holes %in% c(9L, 18L)),
    all(sheet$booking_date <= sheet$play_date),
    all(sheet$booking_date <= as_of),
    all(is.na(sheet$revenue[sheet$booking_status == "reserved"])),
    all(sheet$revenue[sheet$booking_status %in% c("cancelled", "no_show")] == 0),
    all(sheet$revenue[sheet$booking_status == "completed"] ==
      sheet$players_checked_in[sheet$booking_status == "completed"] *
        sheet$price_per_player[sheet$booking_status == "completed"]),
    all(held_players <= players_per_start),
    all(c("reserved", "completed", "cancelled", "no_show") %in% sheet$booking_status),
    any(duplicated(active_key)),
    any(sheet$booking_status == "completed" &
      sheet$players_checked_in < sheet$players_booked),
    any(sheet$play_date > as_of),
    any(sheet$play_date <= as_of),
    all(sheet$booking_status[sheet$play_date > as_of] %in% c("reserved", "cancelled")),
    all(sheet$booking_status[sheet$play_date <= as_of] != "reserved")
  )

  if (requireNamespace("tibble", quietly = TRUE)) {
    sheet <- tibble::as_tibble(sheet)
  }
  class(sheet) <- unique(c("golfops_tee_sheet", class(sheet)))
  attr(sheet, "synthetic") <- TRUE
  attr(sheet, "seed") <- as.integer(seed)
  attr(sheet, "rng_kind") <- "Mersenne-Twister"
  attr(sheet, "as_of") <- as_of
  attr(sheet, "row_grain") <- "reservation"
  attr(sheet, "tee_interval_minutes") <- interval
  attr(sheet, "players_per_start") <- players_per_start
  attr(sheet, "weekday_window_start") <- weekday_window_start
  attr(sheet, "weekday_window_end") <- weekday_window_end
  attr(sheet, "weekend_window_start") <- weekend_window_start
  attr(sheet, "weekend_window_end") <- weekend_window_end
  attr(sheet, "rate_card") <- rate_card
  attr(sheet, "price_unit") <- "USD per player, green fee only"
  attr(sheet, "generator") <- "data-raw/sample_tee_sheet.R"
  attr(sheet, "decisions") <- c(
    "sample_municipal is a fictional municipal 18-hole course.",
    "Reservations and revenue are simulated. They are not Dobson Ranch, Peoria Park District, or Cress Creek transactions.",
    "One row is one reservation. Reservations may share a tee time. Cancelled reservations do not hold player slots.",
    "Empty tee times are omitted. Inventory is the operating window, not the sum of max_players.",
    "The extract is as of the end of the day on 2026-06-19. Later play dates are still reserved.",
    "Realized green-fee revenue is collected at check-in. Cancelled and no-show reservations realize zero. Reserved reservations have no realized revenue yet.",
    "No no-show fee, cancellation fee, cart fee, tax, or golfer identity is included.",
    "Weekday starts run from 07:00 until 18:00 on a 10-minute interval. Weekend starts run from 06:30 until 18:30. The end clock is not a start.",
    "The generator uses RNGkind Mersenne-Twister and set.seed(704)."
  )
  sheet
}

if (isTRUE(getOption("golfops.build_sample_tee_sheet"))) {
  sample_tee_sheet <- generate_sample_tee_sheet()
  message(sprintf(
    "sample_tee_sheet: %s reservations, realized green fees $%s",
    nrow(sample_tee_sheet),
    format(sum(sample_tee_sheet$revenue, na.rm = TRUE), big.mark = ",")
  ))
  print(table(sample_tee_sheet$booking_status))
  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop(
      "Install usethis, then rerun this script so data/sample_tee_sheet.rda is saved with usethis::use_data().",
      call. = FALSE
    )
  }
  usethis::proj_set(".")
  usethis::use_data(sample_tee_sheet, overwrite = TRUE)
}

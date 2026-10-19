sheet_capacity <- function(sheet) {
  dates <- sort(unique(sheet$play_date))
  weekend <- as.POSIXlt(dates)$wday %in% c(0L, 6L)
  course_capacity(
    date = dates,
    window_start = ifelse(
      weekend,
      attr(sheet, "weekend_window_start"),
      attr(sheet, "weekday_window_start")
    ),
    window_end = ifelse(
      weekend,
      attr(sheet, "weekend_window_end"),
      attr(sheet, "weekday_window_end")
    ),
    tee_interval_minutes = attr(sheet, "tee_interval_minutes"),
    players_per_start = attr(sheet, "players_per_start")
  )
}

clock_minutes <- function(clock) {
  parts <- strsplit(clock, ":", fixed = TRUE)
  vapply(parts, function(piece) {
    piece <- as.numeric(piece)
    piece[1] * 60 + piece[2]
  }, numeric(1))
}

test_that("the synthetic tee sheet is a reservation table with no golfer identity", {
  sheet <- sample_tee_sheet
  expect_s3_class(sheet, "golfops_tee_sheet")
  expect_true(attr(sheet, "synthetic"))
  expect_equal(attr(sheet, "seed"), 704L)
  expect_equal(attr(sheet, "row_grain"), "reservation")
  expect_equal(attr(sheet, "as_of"), as.Date("2026-06-19"))
  expect_equal(unique(sheet$course_id), "sample_municipal")
  expect_false(any(c(
    "player_name", "customer_name", "email", "phone", "first_name", "last_name"
  ) %in% names(sheet)))
  expect_equal(nrow(sheet), 735L)
  expect_equal(sum(sheet$revenue, na.rm = TRUE), 30746)
  expect_equal(
    unname(as.integer(table(sheet$booking_status))),
    c(127L, 495L, 67L, 46L)
  )
})

test_that("player counts, statuses, prices, and tee times stay inside the rules", {
  sheet <- sample_tee_sheet
  expect_true(all(sheet$players_booked >= 1L))
  expect_true(all(sheet$players_booked <= sheet$max_players))
  expect_true(all(sheet$max_players == 4L))
  expect_true(all(sheet$players_checked_in >= 0L))
  expect_true(all(sheet$players_checked_in <= sheet$players_booked))
  expect_true(all(sheet$players_checked_in[sheet$booking_status != "completed"] == 0L))
  expect_true(all(sheet$players_checked_in[sheet$booking_status == "completed"] >= 1L))
  expect_true(all(sheet$holes %in% c(9L, 18L)))
  expect_setequal(
    levels(sheet$booking_status),
    c("reserved", "completed", "cancelled", "no_show")
  )
  expect_true(all(sheet$booking_date <= sheet$play_date))
  expect_true(all(sheet$booking_date <= attr(sheet, "as_of")))
  expect_true(all(is.na(sheet$revenue[sheet$booking_status == "reserved"])))
  expect_true(all(sheet$revenue[sheet$booking_status %in% c("cancelled", "no_show")] == 0))
  completed <- sheet[sheet$booking_status == "completed", ]
  expect_equal(
    completed$revenue,
    completed$players_checked_in * completed$price_per_player
  )
  expect_true(any(completed$players_checked_in < completed$players_booked))
  expect_true(any(sheet$play_date > attr(sheet, "as_of")))
  expect_true(all(sheet$booking_status[sheet$play_date > attr(sheet, "as_of")] %in%
    c("reserved", "cancelled")))
  expect_true(all(sheet$booking_status[sheet$play_date <= attr(sheet, "as_of")] != "reserved"))
  expect_false(any(sheet$booking_channel == "walk_in" & sheet$play_date > attr(sheet, "as_of")))

  card <- attr(sheet, "rate_card")
  weekend <- as.POSIXlt(sheet$play_date)$wday %in% c(0L, 6L)
  day <- ifelse(weekend, "weekend", "weekday")
  posted <- merge(
    data.frame(
      rate_type = as.character(sheet$rate_type),
      day = day,
      holes = sheet$holes,
      price_per_player = sheet$price_per_player
    ),
    card,
    by = c("rate_type", "day", "holes"),
    suffixes = c("_sheet", "_card")
  )
  expect_equal(nrow(posted), nrow(sheet))
  expect_equal(posted$price_per_player_sheet, posted$price_per_player_card)

  minutes <- clock_minutes(sheet$tee_time)
  start <- ifelse(weekend, 6 * 60 + 30, 7 * 60)
  end <- ifelse(weekend, 18 * 60 + 30, 18 * 60)
  expect_true(all((minutes - start) %% attr(sheet, "tee_interval_minutes") == 0))
  expect_true(all(minutes >= start & minutes < end))
  expect_true(any(weekend))
  expect_true(any(!weekend))
})

test_that("shared reservations do not double-count tee-time inventory", {
  sheet <- sample_tee_sheet
  active <- sheet[sheet$booking_status != "cancelled", ]
  key <- paste(active$play_date, active$tee_time)
  held <- tapply(active$players_booked, key, sum)
  expect_true(all(held <= 4))
  expect_true(any(duplicated(key)))
  expect_gt(nrow(active), length(unique(key)))

  capacity <- sheet_capacity(sheet)
  held_use <- daily_utilization(
    data.frame(
      date = active$play_date,
      tee_time = active$tee_time,
      players = active$players_booked
    ),
    capacity
  )
  expect_lte(max(held_use$tee_time_occupancy), 1)
  expect_lte(max(held_use$slot_utilization), 1)
  expect_lt(mean(held_use$tee_time_occupancy), 1)
  expect_gt(mean(held_use$slot_utilization), 0.25)
  expect_equal(sum(held_use$occupied_tee_times), length(unique(key)))
  expect_lt(sum(held_use$occupied_tee_times), nrow(active))
  expect_equal(sum(held_use$players), sum(active$players_booked))

  played <- sheet[sheet$booking_status == "completed", ]
  played_use <- daily_utilization(
    data.frame(
      date = played$play_date,
      tee_time = played$tee_time,
      players = played$players_checked_in
    ),
    capacity
  )
  expect_lte(max(played_use$slot_utilization, na.rm = TRUE), 1)
  expect_lt(sum(played_use$players), sum(held_use$players))
  expect_equal(
    sum(played_use$players),
    sum(played$players_checked_in)
  )

  # Zero checked-in rows do not create an occupied start by themselves.
  checked <- daily_utilization(
    data.frame(
      date = sheet$play_date,
      tee_time = sheet$tee_time,
      players = sheet$players_checked_in
    ),
    capacity
  )
  expect_equal(sum(checked$players), sum(sheet$players_checked_in))
  expect_lte(max(checked$slot_utilization, na.rm = TRUE), 1)
})

test_that("the documented seed regenerates the packaged tee sheet", {
  script <- testthat::test_path("..", "..", "data-raw", "sample_tee_sheet.R")
  skip_if_not(file.exists(script))
  env <- new.env(parent = globalenv())
  # Sourcing must not rebuild data/ even if a build option is still set.
  old_build <- getOption("golfops.build_sample_tee_sheet")
  on.exit(options(golfops.build_sample_tee_sheet = old_build), add = TRUE)
  options(golfops.build_sample_tee_sheet = FALSE)
  sys.source(script, envir = env)
  rng <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (is.null(rng)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", rng, envir = .GlobalEnv)
    }
  }, add = TRUE)
  regen <- env$generate_sample_tee_sheet(seed = 704L)
  expect_equal(regen, sample_tee_sheet)
})

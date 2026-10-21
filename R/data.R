#' Dobson Ranch golf rounds and realized revenue
#'
#' Activity and revenue for Dobson Ranch Golf Course, published by the City
#' of Mesa Parks, Recreation and Community Facilities. The city describes the
#' course as operated by Paradigm Golf Group. This object is the open-data
#' extract saved on 2026-10-03, dataset `24g5-hb8k`, after
#' [prepare_dobson_rounds()] parses dates, currency, and the city's percent
#' field. Blank source cells stay missing. No row is imputed, and the absent
#' day 2024-12-10 is not added.
#'
#' The extract records rounds the city says were played and revenue the city
#' says was earned. It has no published price list, no inventory of available
#' tee times, and no reservation records. `rounds_played` is play.
#' `green_fee_revenue` and the other revenue columns are realized revenue.
#' `rounds_possible` is the city's daylight estimate. `booking_rate` keeps
#' the city's field name, Round Booking Rate, and stores the city's
#' definition of that field: the percent of possible rounds that were played.
#'
#' Every source row is kept. There are 1766 daily rows and 72 monthly totals.
#' The monthly rows run from 2015-07-01 through 2021-06-01 and are dated the
#' first of the month. In this file, daily rows have at most 675 rounds
#' played and monthly rows have at least 2963, so `grain` labels a row
#' `month` when rounds played are at least 1000 and `day` otherwise. That
#' cutoff is the package rule stored in `monthly_rounds_threshold`. It is
#' not a field in the city catalog. Add daily rows to daily rows, and
#' monthly rows to monthly rows.
#'
#' One daily row is dated 2004-12-10 and sits apart from the later series.
#' From 2021-07-01 through 2026-04-30 the only missing date is 2024-12-10.
#' The date 2022-04-24 occurs twice, and those two rows match on every column
#' except `row_id`.
#'
#' `sunrise` and `sunset` keep the source clock strings. The catalog does
#' not give a time zone. Calendar labels are taken from `date` by
#' [prepare_dobson_rounds()] and match the source Calendar Year, Fiscal
#' Year, Month, Day, Day of Week, and Season values on every row.
#'
#' Attributes `source_path`, `source_url`, and `source_accessed` record the
#' bundled CSV, the catalog page, and the 2026-10-03 save date. A catalog
#' metadata request on 2026-10-17 reported a later rows-updated time,
#' 2026-10-17 02:17:12 UTC. This object was not replaced with that later
#' update.
#'
#' @format A `golfops_rounds` data frame with 1838 rows and 38 variables:
#' \describe{
#'   \item{row_id}{City field RowID. The catalog calls it a unique
#'     identifier. The CSV prints thousands separators; those are removed
#'     and the value is stored as a number.}
#'   \item{date}{City field Date, parsed to a date. The catalog says this is
#'     when the golf round was played. Monthly total rows are dated the
#'     first of the month.}
#'   \item{calendar_year}{Calendar year of `date`. It equals the source
#'     Calendar Year on every row. The catalog description is "Calendar
#'     year".}
#'   \item{fiscal_year}{City fiscal-year label, such as `FY15-16`. The
#'     catalog says the City of Mesa fiscal year is July 1 through June 30
#'     and that the field uses the two-year designation. The label matches
#'     the source Fiscal Year on every row.}
#'   \item{month}{Month of `date`, as a factor with levels January through
#'     December. It matches the source Month on every row.}
#'   \item{day}{Day of the month from `date`. It matches the source Day on
#'     every row.}
#'   \item{weekday}{Weekday of `date`, as a factor with levels Monday through
#'     Sunday. It matches the source Day of Week on every row.}
#'   \item{season}{Source Season value, as a factor with levels Winter,
#'     Spring, Summer, and Fall. On every row it matches December--February
#'     as Winter, March--May as Spring, June--August as Summer, and
#'     September--November as Fall. The catalog description for Season is
#'     blank.}
#'   \item{sunrise}{City field Sunrise, kept as the published clock string
#'     `HH:MM:SS`. The catalog says "Time of sunrise" and does not give a
#'     time zone. Missing on 67 rows, the same rows where `sunset` and
#'     `daylight_hours` are missing.}
#'   \item{sunset}{City field Sunset, kept as the published clock string
#'     `HH:MM:SS`. The catalog says "Time of sunset" and does not give a
#'     time zone. Missing on the same 67 rows as `sunrise`.}
#'   \item{daylight_hours}{Sunset minus sunrise, in decimal hours, reading
#'     each clock string as a time on a 24-hour clock. Computed by
#'     [prepare_dobson_rounds()]. Missing on the same 67 rows as the clock
#'     strings. This column is not in the city catalog.}
#'   \item{grain}{`day` or `month`. `month` means `rounds_played` is at
#'     least 1000. This file has 1766 `day` rows and 72 `month` rows. The
#'     cutoff is not a catalog field.}
#'   \item{rounds_played}{City field Rounds Played. The catalog says "Number
#'     of rounds played". These are played rounds, not reserved tee times.}
#'   \item{rounds_possible}{City field Rounds Possible. The catalog says this
#'     is the number of rounds that could be played, found from the hours of
#'     daylight available. It is the city's daylight estimate, not a count
#'     of tee times on a tee sheet. Missing on 67 rows.}
#'   \item{booking_rate}{City field Round Booking Rate, divided by 100. The
#'     catalog defines it as the percent of possible rounds that were
#'     played, so the stored number is a proportion. On the 1771 rows where
#'     this value and `rounds_played / rounds_possible` are both defined,
#'     the largest absolute difference is 0.0005. The rate is above 1 on
#'     109 rows. It is the city's played-over-possible percent, not a count
#'     or share of reserved tee times. Missing on 67 rows.}
#'   \item{utilization}{`rounds_played / rounds_possible` where
#'     `rounds_possible` is positive, from [prepare_dobson_rounds()]. It is
#'     the share of the daylight estimate that was played. Missing on 67
#'     rows. This column is not in the city catalog.}
#'   \item{open_daylight_rounds}{`max(rounds_possible - rounds_played, 0)`
#'     where `rounds_possible` is present. Days with more played rounds than
#'     the daylight estimate are stored as 0. This is the unused portion of
#'     the daylight estimate, not a count of open tee times. Missing on 67
#'     rows. This column is not in the city catalog.}
#'   \item{cart_rentals}{City field Cart Rentals. The catalog says "Number
#'     of carts used in golf rounds. Derived from Cart Revenue." Missing
#'     values stay missing.}
#'   \item{cart_revenue}{City field Cart Revenue. The catalog says "Revenue
#'     earned from cart rentals." The CSV prints a dollar sign; the column
#'     stores the parsed number. This is realized cart revenue, not a posted
#'     cart price. A missing value is a blank source cell.}
#'   \item{club_rentals}{City field Club Rentals. The catalog says "Number
#'     of club rentals. Derived from club rental revenue."}
#'   \item{club_revenue}{City field Club Revenue. The catalog says "Revenue
#'     earned from golf club rentals." Parsed from the dollar-formatted CSV
#'     value. Realized club-rental revenue, not a posted rental price.}
#'   \item{food_beverage_revenue}{City field Food and Beverage. The catalog
#'     says "Revenue earned from food and beverage sales." Parsed from the
#'     dollar-formatted CSV value.}
#'   \item{green_fee_revenue}{City field Green Fees. The catalog says
#'     "Revenue earned from green fees." Parsed from the dollar-formatted
#'     CSV value. This is realized green-fee revenue, not a published green
#'     fee and not a price for an available tee time.}
#'   \item{merchandise_revenue}{City field Merchandise Revenue. The catalog
#'     says "Revenue earned from merchandise revenue sales." Parsed from the
#'     dollar-formatted CSV value.}
#'   \item{range_revenue}{City field Range Sales. The catalog says "Revenue
#'     earned from driving range use." Parsed from the dollar-formatted CSV
#'     value.}
#'   \item{ancillary_revenue}{`total_revenue - green_fee_revenue` when both
#'     source amounts are present. Computed by [prepare_dobson_rounds()].
#'     Missing on the 48 rows where total revenue is missing. This column
#'     is not in the city catalog.}
#'   \item{total_revenue}{City field Total Revenue. The catalog says it is
#'     the sum of cart revenue, club revenue, merchandise revenue, green
#'     fees, range sales, and food and beverage. Parsed from the
#'     dollar-formatted CSV value. Missing on 48 rows.}
#'   \item{component_revenue}{Sum of cart, club, food and beverage, green
#'     fee, merchandise, and range revenue on rows where all six amounts are
#'     present. Computed by [prepare_dobson_rounds()]. Missing on 164 rows,
#'     including rows where one or more category amounts are blank. This
#'     column is not in the city catalog.}
#'   \item{revenue_per_round_source}{City field Revenue/Round. The catalog
#'     says "Calculation: Total Revenue/Rounds." On the 1734 rows where this
#'     value and `total_revenue / rounds_played` are both defined, the
#'     largest absolute difference is 0.01. It is facility revenue per
#'     played round. It is missing for all 56 zero-round rows and for the
#'     48 rows with no total revenue. It is not a published price.}
#'   \item{green_fee_per_played_round}{`green_fee_revenue / rounds_played`
#'     where `rounds_played` is positive. Computed by
#'     [prepare_dobson_rounds()]. Realized green-fee revenue per played
#'     round. Missing on 104 rows. This column is not in the city catalog
#'     and is not a posted green fee.}
#'   \item{total_revenue_per_played_round}{`total_revenue / rounds_played`
#'     where `rounds_played` is positive. Computed by
#'     [prepare_dobson_rounds()]. Missing on 104 rows. This column is not in
#'     the city catalog.}
#'   \item{green_fee_per_daylight_round}{`green_fee_revenue / rounds_possible`
#'     where `rounds_possible` is positive. Computed by
#'     [prepare_dobson_rounds()]. Green-fee revenue per daylight-estimated
#'     round. Missing on 67 rows. This column is not in the city catalog
#'     and is not revenue per open tee time.}
#'   \item{total_revenue_per_daylight_round}{`total_revenue / rounds_possible`
#'     where `rounds_possible` is positive. Computed by
#'     [prepare_dobson_rounds()]. Facility revenue per daylight-estimated
#'     round. Missing on 67 rows. This column is not in the city catalog.}
#'   \item{flag_duplicate_date}{`TRUE` when `date` appears on more than one
#'     row. Two rows are `TRUE`, both 2022-04-24, and they match on every
#'     other column.}
#'   \item{flag_zero_rounds}{`TRUE` when `rounds_played` is 0. Fifty-six
#'     rows are `TRUE`. The extract does not give a reason.}
#'   \item{flag_above_daylight_capacity}{`TRUE` when `rounds_played` is
#'     greater than `rounds_possible`. One hundred nine rows are `TRUE`:
#'     played rounds exceeded the city's daylight estimate.}
#'   \item{flag_calendar_mismatch}{`TRUE` when the source Month, Day of
#'     Week, Day, Season, or Fiscal Year disagrees with the value computed
#'     from `date`. Every row is `FALSE` in this extract.}
#'   \item{flag_missing_total_revenue}{`TRUE` when `total_revenue` is
#'     missing. Forty-eight rows are `TRUE`.}
#' }
#' @source City of Mesa. "Dobson Ranch Golf Rounds." Mesa Open Data, dataset
#'   24g5-hb8k, Parks, Recreation and Community Facilities. Catalog page:
#'   <https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Dobson-Ranch-Golf-Rounds/24g5-hb8k>.
#'   Column descriptions are the catalog field descriptions. The catalog
#'   description of the dataset is: "Provides information on activity levels
#'   and revenues at Dobson Ranch Golf Course, which is operated by Paradigm
#'   Golf Group." The packaged file is the CSV saved 2026-10-03,
#'   `Dobson_Ranch_Golf_Rounds_20261003.csv`, copied without changes to
#'   `inst/extdata/dobson_ranch_golf_rounds.csv`. Both copies have SHA-256
#'   `d6b53a8dc37e80c9ec10ec3bc08cd834f3d6155cdeb9809ce0870cd7c1be0079`.
#'   The catalog lists the data as public and links terms of use at
#'   <https://data.mesaaz.gov/stories/s/2dcd-j2nx/>.
#' @examples
#' data(dobson_rounds)
#' str(dobson_rounds)
"dobson_rounds"

#' Synthetic municipal tee sheet
#'
#' Simulated reservations for `sample_municipal`, a fictional 18-hole
#' municipal course. This is not a tee sheet from Dobson Ranch, the Peoria
#' Park District, or Cress Creek Country Club, and it is not an export from
#' foreUP, TeeItUp, GolfNow, or another vendor. The column names are
#' `golfops` names. Public vendor documents describe tee sheets, check-in,
#' cancellations, and price classes, but this package does not have a
#' verified CSV dictionary for any of those systems, so none of their field
#' names are used here.
#'
#' One row is one reservation. Two reservations may share a tee time, and
#' their player counts add. A cancelled reservation stays on the sheet and
#' does not hold a player slot, so a later reservation can use that start.
#' `max_players` is the cap for the start, copied onto every reservation at
#' that start. Summing `max_players` double-counts a shared start and still
#' misses starts with no reservation. Those empty starts are not rows.
#' Inventory is the operating window: weekday 07:00--18:00 and weekend
#' 06:30--18:30, every 10 minutes, four players per start. The end clock is
#' not itself a start. Pass that window to [course_capacity()].
#'
#' The sheet is an end-of-day extract on 2026-06-19. Play dates after that
#' day are `reserved` or `cancelled`. Earlier play dates have a terminal
#' status. `no_show` means nobody in that reservation checked in.
#' `completed` can still have `players_checked_in` below `players_booked`
#' when part of the group played. Nine-hole and 18-hole reservations use the
#' same starting inventory. A nine-hole player still takes one player slot.
#'
#' Realized revenue is the green fee collected at check-in:
#' `players_checked_in * price_per_player`. Cancelled and no-show
#' reservations realize zero. There is no no-show fee and no cancellation
#' fee. On a `reserved` row, `players_booked * price_per_player` is future
#' booking value. It is not realized revenue. This sheet does not model
#' prepayment, so `revenue` is missing for those rows. Prices are synthetic
#' dollars per player. They are not a Peoria posted card and not Dobson
#' Ranch revenue divided by rounds. The assumed card is stored in attribute
#' `rate_card`. Carts, tax, and golfer names are absent.
#'
#' [daily_utilization()] asks for `date`, `tee_time`, and `players`, and it
#' has no course id. This sample has one course. Realized player utilization
#' uses `players_checked_in` on `completed` rows, and only play dates through
#' the extract, 2026-06-19. `slot_utilization` is those players divided by
#' player slots. Reservations after the extract have not been played, so
#' checked-in counts are zero there. Evaluate those future bookings with
#' booked utilization: drop `cancelled` rows and pass `players_booked`.
#' Groups on the same start count as one occupied tee time. A start whose
#' supplied player counts sum to zero is not occupied.
#'
#' @format A `golfops_tee_sheet` data frame with 735 rows and 14 variables:
#' \describe{
#'   \item{course_id}{Course identifier. Every row is `sample_municipal`.}
#'   \item{play_date}{Date the round is scheduled to start.}
#'   \item{tee_time}{Scheduled start, `"HH:MM"`, on the 10-minute grid.
#'     Weekday starts run from 07:00 through 17:50. Weekend starts run from
#'     06:30 through 18:20.}
#'   \item{booking_id}{Reservation identifier, `B-` plus five digits, unique
#'     on this sheet and ordered by play date, tee time, and creation.}
#'   \item{players_booked}{Players on this reservation. An integer from 1 to
#'     `max_players`.}
#'   \item{players_checked_in}{Players who arrived and paid. Zero unless
#'     `booking_status` is `completed`. Never above `players_booked`.}
#'   \item{max_players}{Player cap for the start, 4 on every row. Do not sum
#'     this column.}
#'   \item{holes}{9 or 18. This does not create a second tee sheet.}
#'   \item{booking_status}{`reserved`, `completed`, `cancelled`, or
#'     `no_show`.}
#'   \item{booking_channel}{`online`, `phone`, or `walk_in`. Walk-in
#'     reservations are same-day and are not used for play dates after the
#'     extract.}
#'   \item{rate_type}{`standard`, `senior`, `junior`, `member`, or
#'     `promotional`. Promotional rates are more common after 15:00.}
#'   \item{price_per_player}{Green fee for one player, in US dollars, from
#'     attribute `rate_card`. It depends on `rate_type`, weekday or weekend,
#'     and `holes`.}
#'   \item{booking_date}{Date the reservation was made. On or before
#'     `play_date`, and on or before the extract date.}
#'   \item{revenue}{Realized green-fee dollars. Missing while `reserved`.
#'     The posted fee on those rows, `players_booked * price_per_player`,
#'     is future booking value and is not stored in this column. Zero when
#'     `cancelled` or `no_show`. Otherwise
#'     `players_checked_in * price_per_player`.}
#' }
#' Attributes `synthetic`, `seed` (704), `as_of`, the operating window,
#' `rate_card`, and `decisions` record the simulation. `row_grain` is
#' `reservation`.
#'
#' @source Synthetic data from `data-raw/sample_tee_sheet.R`,
#'   `set.seed(704)`, RNGkind `Mersenne-Twister`. No operator file was
#'   copied. Documents consulted while choosing the grain, and not used as a
#'   column dictionary, include foreUP's tee-sheet print article
#'   (<https://foreup.zendesk.com/hc/en-us/articles/360006796293-Printing-the-Tee-Sheet>),
#'   its check-in article
#'   (<https://foreup.zendesk.com/hc/en-us/articles/360006726354-Checking-in-a-Tee-Time>),
#'   and its Tee Times Sales by Price Class article
#'   (<https://foreup.zendesk.com/hc/en-us/articles/45261809335835-Tee-Times-Sales-By-Price-Class>).
#'   Those articles confirm individual booked and cancelled tee times, partial
#'   check-in, price class, and CSV export of reports. They do not publish
#'   the tee-sheet columns. Dobson Ranch's public booking page is TeeItUp
#'   (<https://dobson-ranch.book.teeitup.com/>), which shows open inventory
#'   rather than an internal sheet. Cress Creek's tee-sheet vendor was not
#'   identified. Peoria Park District minutes for November 8, 2023 record a
#'   foreUP license extension
#'   (<https://peoriaparks.org/wp-content/uploads/11.8.23-Regular-Board-Meeting-Minutes-1.pdf>)
#'   and do not include an export layout.
#' @examples
#' data(sample_tee_sheet)
#' sheet <- sample_tee_sheet
#' as_of <- attr(sheet, "as_of")
#' dates <- sort(unique(sheet$play_date[sheet$play_date <= as_of]))
#' weekend <- as.POSIXlt(dates)$wday %in% c(0L, 6L)
#' capacity <- course_capacity(
#'   date = dates,
#'   window_start = ifelse(
#'     weekend,
#'     attr(sheet, "weekend_window_start"),
#'     attr(sheet, "weekday_window_start")
#'   ),
#'   window_end = ifelse(
#'     weekend,
#'     attr(sheet, "weekend_window_end"),
#'     attr(sheet, "weekday_window_end")
#'   ),
#'   tee_interval_minutes = attr(sheet, "tee_interval_minutes"),
#'   players_per_start = attr(sheet, "players_per_start")
#' )
#' played <- sheet[
#'   sheet$booking_status == "completed" & sheet$play_date <= as_of,
#' ]
#' use <- daily_utilization(
#'   data.frame(
#'     date = played$play_date,
#'     tee_time = played$tee_time,
#'     players = played$players_checked_in
#'   ),
#'   capacity
#' )
#' use[, c("date", "players", "player_slots", "slot_utilization")]
"sample_tee_sheet"

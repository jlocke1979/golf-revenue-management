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

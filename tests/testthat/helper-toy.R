toy_dobson_csv <- function(path) {
  rows <- data.frame(
    RowID = c("1", "2", "3", "4", "5", "6", "7"),
    Date = c(
      "2024 Jan 02 12:00:00 AM",
      "2024 Jan 03 12:00:00 AM",
      "2024 Jan 04 12:00:00 AM",
      "2024 Jan 05 12:00:00 AM",
      "2024 Jun 01 12:00:00 AM",
      "2024 Feb 01 12:00:00 AM",
      "2024 Feb 01 12:00:00 AM"
    ),
    `Calendar Year` = c("2024", "2024", "2024", "2024", "2024", "2024", "2024"),
    `Fiscal Year` = c("FY23-24", "FY23-24", "FY23-24", "FY23-24", "FY23-24", "FY23-24", "FY23-24"),
    Month = c("January", "January", "January", "January", "June", "February", "February"),
    Day = c("2", "3", "4", "5", "1", "1", "1"),
    `Day of Week` = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Thursday", "Thursday"),
    Sunrise = c("", "", "", "", "", "", ""),
    Sunset = c("", "", "", "", "", "", ""),
    `Cart Rentals` = c("10", "4", "1", "0", "", "2", "2"),
    `Cart Revenue` = c("1000", "200", "100", "0", "", "50", "50"),
    `Club Rentals` = c("1", "1", "1", "0", "", "1", "1"),
    `Club Revenue` = c("1000", "200", "100", "0", "", "50", "50"),
    `Food and Beverage` = c("1000", "200", "100", "0", "", "50", "50"),
    `Green Fees` = c("5000", "9000", "100", "12500", "", "200", "200"),
    `Merchandise Revenue` = c("1000", "200", "100", "0", "", "50", "50"),
    `Range Sales` = c("1000", "200", "100", "0", "", "50", "50"),
    `Rounds Possible` = c("200", "200", "200", "200", "", "40", "40"),
    `Round Booking Rate` = c("50%", "90%", "0%", "125%", "", "50%", "50%"),
    `Rounds Played` = c("100", "180", "0", "250", "4000", "20", "20"),
    `Total Revenue` = c("10000", "10000", "600", "12500", "", "450", "450"),
    `Revenue/Round` = c("100", "55.56", "", "50", "", "22.5", "22.5"),
    Season = c("Winter", "Winter", "Winter", "Winter", "Summer", "Winter", "Winter"),
    check.names = FALSE
  )
  write.csv(rows, path, row.names = FALSE, na = "", quote = TRUE)
  path
}

# Get data

rm(list = ls())

library(data.table)
library(lubridate)
library(httr2)

source("functions.R")

# It looks the API returns data about 100 days max
# Get the prices for the last 12 months

# Vector of the first days
start_dates <- seq(
  from = Sys.Date() - months(1) + days(1),
  by = "-1 month",
  length.out = 12
) |> sort()

# Vector of the last days
end_dates <- start_dates + months(1) - days(1)

data.table(start_dates, end_dates)

xml_data <- purrr::map2(
  .x = start_dates,
  .y = end_dates,
  .f = \(x, y) get_prices_xml(dateStart = x, dateEnd = y)
)

dat <- purrr::map(.x = xml_data, .f = extract_prices) |>
  rbindlist() |> setkey(datetime)

# Original price is € / MWh
# Convert price to cents / kWh
dat[, price_c_kWh := price / 10]

dat[, date := as.IDate(format(datetime, format = "%Y-%m-%d"))]
dat[, time := as.ITime(format(datetime, format = "%H:%M"))]

dat[, weekday := lubridate::wday(date, week_start = 1)]

dat[, workdays := weekday %in% 1:5]


# Save
fwrite(x = dat, file = "data.csvy", yaml = TRUE)

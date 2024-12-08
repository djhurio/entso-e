# Get data

rm(list = ls())

library(data.table)
library(gt)
library(lubridate)

options("openxlsx2.numFmt" = "0.0")

source("functions.R")

# Parameters
hours_on <- 24 * 2/3

# It looks the API returns data about 100 days max
# Get the prices for the last 12 months

# Vector of the first days
start_dates <- seq(
  # from = floor_date(Sys.Date() - months(1), "month"),
  from = Sys.Date() - months(1) + days(1),
  by = "-1 month",
  length.out = 12
) |> sort()

# Vector of the last days
end_dates <- start_dates + months(1) - days(1)

data.table(start_dates, end_dates)

# xml_data <- get_prices_xml(
#   dateStart = Sys.Date(),
#   dateEnd = Sys.Date()
# )

xml_data <- purrr::map2(
  .x = start_dates,
  .y = end_dates,
  .f = \(x, y) get_prices_xml(dateStart = x, dateEnd = y)
)

# dat <- convert_prices_dt(xml_data) |> setDT(key = "datetime")
# dat <- extract_prices(xml_data) |> setkey(datetime)
dat <- purrr::map(.x = xml_data, .f = extract_prices) |>
  rbindlist() |> setkey(datetime)

# Original price is € / MWh
# Convert price to cents / kWh
dat[, price_c_kWh := price / 10]

dat[, date := as.Date(format(datetime, format = "%Y-%m-%d"))]
dat[, time := format(datetime, format = "%H:%M")]

dat[, weekday := lubridate::wday(date, week_start = 1)]

dat[, workdays := weekday %in% 1:5]


# By weekdays
dat_agg_wdays <- dat[, .(price = mean(price_c_kWh)),
                     keyby = .(weekday, time)]

setorder(dat_agg_wdays, weekday, price)

dat_agg_wdays[, hi := factor(1:.N <= hours_on, c(TRUE, FALSE), c("on", "off")),
              by = .(weekday)]
dat_agg_wdays[, .N, keyby = .(hi)]


# By workdays / weekends
dat_agg_workdays <- dat[, .(price = mean(price_c_kWh)),
                        keyby = .(workdays, time)]

setorder(dat_agg_workdays, workdays, price)

dat_agg_workdays[
  ,
  hi := factor(1:.N <= hours_on, c(TRUE, FALSE), c("on", "off")),
  by = .(workdays)
]
dat_agg_workdays[, .N, keyby = .(hi)]

dat_agg_workdays[, weekday := ifelse(workdays, 15L, 67L)]
dat_agg_workdays[, workdays := NULL]

setorder(dat_agg_workdays, weekday, time)
dat_agg_workdays



# All days
dat_agg_all <- dat[, .(price = mean(price_c_kWh)),
                   keyby = .(time)]
dat_agg_all[, weekday := 0L]

setorder(dat_agg_all, weekday, price)

dat_agg_all[, hi := factor(1:.N <= hours_on, c(TRUE, FALSE), c("on", "off")),
            by = .(weekday)]
dat_agg_all[, .N, keyby = .(hi)]

setorder(dat_agg_all, weekday, time)
dat_agg_all


# Add
dat_agg <- rbindlist(
  l = list(dat_agg_wdays, dat_agg_all, dat_agg_workdays),
  use.names = TRUE
)

schedule <- dcast.data.table(
  data = dat_agg,
  formula = time ~ weekday,
  value.var = "hi"
)

schedule.gt <- gt(schedule) |>
  sub_missing(missing_text = "") |>
  data_color(
    columns = matches("[0-9]"),
    target_columns = matches("[0-9]"),
    palette = "Accent"
  )

schedule
schedule.gt

# Optimal time slot
dat_agg[order(price)]

cat("\n# Optimal time for water desinfection\n")
dat_agg[time <= "05:00"][order(price)][1] |> print()

dat[weekday == 7L & time == "13:00", summary(price_c_kWh)]

dat_summary <- dat[, as.list(summary(price_c_kWh)), keyby = .(weekday, time)]
dat_summary[order(Mean)]   # Sunday 13:00 3.4
dat_summary[order(Median)] # Sunday 14:00 3.7
dat_summary[order(Max.)]   # Monday 04:00 5.4


# Schedule for water heating
setorder(dat_agg, weekday, time)
dat_agg
# dat_agg[, .(1:.N == 1L, hi != shift(hi)), by = .(weekday)]
dat_agg[, time_slot := cumsum(1:.N == 1L | hi != shift(hi)), by = .(weekday)]

dat_agg[, time_next := shift(time, type = "lead")]
dat_agg[.I == .N, time_next := "00:00"]

cat("\n# Schedule for water heating\n")
dat_agg[
  hi == "on" & weekday > 7,
  .(
    period = paste(first(time), last(time_next), sep = " - "),
    hours = .N
  ),
  keyby = .(weekday, hi, time_slot)
] |> print()



# Average prices

schedule.price <- dcast.data.table(
  data = dat_agg,
  formula = time ~ weekday,
  value.var = "price"
)

schedule.price.gt <- gt(schedule.price) |>
  sub_missing(missing_text = "") |>
  fmt_number(decimals = 1) |>
  cols_width(
    time ~ px(60),
    everything() ~ px(40)
  ) |>
  data_color(
    columns = matches("[0-9]"),
    target_columns = matches("[0-9]"),
    palette = "Reds"
  )

schedule.price
schedule.price.gt


dat_agg_all[, mean(price)]
dat_agg_all[, mean(price), keyby = .(hi)]

dat_agg_all[, 1 - mean(price[hi == "on"]) / mean(price)]
dat_agg_wdays[, 1 - mean(price[hi == "on"]) / mean(price)]



# Save

schedule.gt |> gtsave(filename = "schedule.html")
schedule.price.gt |> gtsave(filename = "schedule.price.html")

openxlsx2::write_xlsx(
  x = list(
    Schedule = schedule,
    Price = schedule.price
  ),
  file = "schedule.xlsx"
)

# Get data

rm(list = ls())

library(data.table)
library(gt)
library(lubridate)

source("functions.R")

xml_data <- get_prices_xml(
  dateStart = Sys.Date() %m-% years(1),
  dateEnd = Sys.Date()
)

dat <- convert_prices_dt(xml_data) |> setDT(key = "datetime")

dat[, date := as.Date(format(datetime, format = "%Y-%m-%d"))]
dat[, time := format(datetime, format = "%H:%M")]

dat[, weekday := lubridate::wday(date, week_start = 1)]

dat_agg <- dat[, .(price = mean(price * 1.21 / 10)), keyby = .(weekday, time)]

setorder(dat_agg, weekday, price)

dat_agg[, hi := factor(1:.N <= 12, c(TRUE, FALSE), c("on", "off")),
        by = .(weekday)]
dat_agg[, .N, keyby = .(hi)]


schedule <- dcast.data.table(
  data = dat_agg,
  formula = time ~ weekday,
  value.var = "hi"
) |>
  gt() |>
  sub_missing(missing_text = "") |>
  data_color(
    columns = matches("[0-9]"),
    target_columns = matches("[0-9]")
  )

schedule |> gtsave(filename = "schedule.html")

# Functions to get Day-ahead Prices [12.1.D]
# https://documenter.getpostman.com/view/7009892/2s93JtP3F6#3b383df0-ada2-49fe-9a50-98b1bb201c6b

# Function to create API URL
make_api_url <- function(base_url, params) {
  if (length(params) > 0) {
    query_string <- paste(names(params), params, sep = "=", collapse = "&")
    query_string <- URLencode(query_string)
    full_url <- paste(base_url, query_string, sep = "?")
  } else {
    full_url <- base_url
  }

  return(full_url)
}

# Function to get data in XML format
get_prices_xml <- function(
  base_url = "https://web-api.tp.entsoe.eu/api",
  securityToken = Sys.getenv("ENTSOE_SECURITY_TOKEN"),
  documentType = "A44",
  out_Domain = "10YLV-1001A00074",
  in_Domain = out_Domain,
  dateStart = Sys.Date(),
  dateEnd = dateStart
) {
  api_url <- make_api_url(
    base_url = base_url,
    params = list(
      securityToken = securityToken,
      documentType = documentType,
      out_Domain = out_Domain,
      in_Domain = in_Domain,
      periodStart = paste0(gsub("-", "", dateStart - 1L), "2300"),
      periodEnd = paste0(gsub("-", "", dateEnd), "2245")
    )
  )

  cat(api_url, "\n")

  return(
    httr2::request(api_url) |> httr2::req_perform() |> httr2::resp_body_xml()
  )
}

# Function to extract prices from XML data
extract_prices <- function(xml_data, tzone = "EET") {
  # Extract and rename the default namespace to "def"
  ns <- xml2::xml_ns_rename(xml2::xml_ns(xml_data), d1 = "def")

  # Check if the renamed namespace exists
  if (is.null(ns[["def"]])) {
    stop("No default namespace found in the XML file.") # Fail if no namespace is found
  }

  # Iterate over TimeSeries nodes
  data <- data.table::rbindlist(
    purrr::map(
      xml2::xml_find_all(xml_data, ".//def:TimeSeries", ns = ns),
      function(time_series) {
        # Iterate over Period nodes within the current TimeSeries
        purrr::map_dfr(
          xml2::xml_find_all(time_series, ".//def:Period", ns = ns),
          function(period) {
            # Extract the start time for the period
            start_time <- xml2::xml_find_first(
              period,
              ".//def:start",
              ns = ns
            ) |>
              xml2::xml_text()
            start_time <- as.POSIXct(
              start_time,
              format = "%Y-%m-%dT%H:%MZ",
              tz = "UTC"
            )
            start_time <- format(start_time, tz = tzone, usetz = TRUE)

            # Iterate over Point nodes within the current Period
            purrr::map_dfr(
              xml2::xml_find_all(period, ".//def:Point", ns = ns),
              function(point) {
                # Extract position and price
                position <- xml2::xml_find_first(
                  point,
                  ".//def:position",
                  ns = ns
                ) |>
                  xml2::xml_integer()
                price <- xml2::xml_find_first(
                  point,
                  ".//def:price.amount",
                  ns = ns
                ) |>
                  xml2::xml_double()

                # Calculate the datetime for the current point
                datetime <- as.POSIXct(start_time) + (position - 1) * 900

                # Return a single-row data.table
                data.table::data.table(datetime = datetime, price = price)
              }
            )
          }
        )
      }
    )
  )

  return(data)
}

# get_prices_xml() |> extract_prices()

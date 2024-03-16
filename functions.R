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
      documentType  = documentType,
      out_Domain    = out_Domain,
      in_Domain     = in_Domain,
      periodStart   = paste0(gsub("-", "", dateStart - 1L), "2300"),
      periodEnd     = paste0(gsub("-", "", dateEnd), "2200")
    )
  )
  
  cat(api_url, "\n")
  
  return(
    httr2::request(api_url) |> httr2::req_perform() |> httr2::resp_body_xml()
  )
  
}

convert_prices_dt <- function(xml_data, tzone = "EET") {
  
  # Since the XML has a default namespace,
  # assign a prefix to it for use in XPath queries
  ns <- xml2::xml_ns_rename(xml2::xml_ns(xml_data), d1 = "def")
  
  # Extract all Periods
  Periods <- xml2::xml_find_all(xml_data, ".//def:Period", ns = ns)
  
  datetime <- xml2::xml_find_all(Periods, ".//def:start", ns = ns) |>
    xml2::xml_text() |>
    as.POSIXct(tz = "UTC", format = "%Y-%m-%dT%H:%M") |>
    lubridate::with_tz(tzone = tzone) |>
    rep(each = 24) + rep(0:23 * 3600, times = length(Periods))
  
  price <- xml2::xml_find_all(Periods, ".//def:price.amount", ns = ns) |>
    xml2::xml_double()
  
  # Combine into a data.frame
  return(
    data.frame(datetime, price)
  )
  
}

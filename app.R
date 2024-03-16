library(shiny)
library(bslib)
library(data.table)
library(gt)

source("functions.R")

# Define UI with page_sidebar for layout
ui <- page_sidebar(
  title = "Elektrības cenas",
  sidebar = tagList(
    checkboxInput(
      inputId = "tax",
      label = "ar PVN",
      value = TRUE
    ),
    checkboxInput(
      inputId = "unit",
      label = "centi/kWh",
      value = TRUE
    )
  ),
  tagList(
    gt_output("table")
  )
)

# Define server logic
server <- function(input, output) {
  
  # data
  xml_data <- get_prices_xml(
    dateStart = Sys.Date() - 1,
    dateEnd = Sys.Date() + 1
  )
  
  dat <- convert_prices_dt(xml_data) |> setDT(key = "datetime")
  
  dat[, date := format(datetime, format = "%Y-%m-%d")]
  dat[, time := format(datetime, format = "%H:%M")]
  
  # Render the gt table
  output$table <- render_gt({
    
    dat[, price_final := price *
          ifelse(input$tax, 1.21, 1) /
          ifelse(input$unit, 10, 1)
    ]
    
    dcast.data.table(
      data = dat,
      formula = time ~ date,
      value.var = "price_final"
    ) |>
      gt() |>
      sub_missing(missing_text = "") |>
      fmt_number() |>
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_column_labels()
      ) |>
      data_color(
        columns = matches("[0-9]"),
        target_columns = matches("[0-9]"),
        palette = "Reds",
        na_color = "white"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

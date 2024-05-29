library(tidyverse)
library(shiny)
library(highcharter)
library(xts)
# Erstes Diagramm
hc1 <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Erstes Diagramm") %>%
  hc_xAxis(categories = c('Englisch', 'Deutsch', 'Spanisch')) %>%
  hc_add_series(name = "Sprachen", data = c(111, 214, 213))
# Beispiel-Daten erstellen
dates <- seq(as.Date("2019-01-01"), as.Date("2024-01-04"), by="year")
courses_english <- xts(c(2, 3, 5, 7, 10, 12), dates)
courses_spanish <- xts(c(1, 2, 3, 5, 8, 11), dates)
courses_german <- xts(c(0.5, 1, 1.5, 2, 3, 4), dates)

# Diagramm erstellen
hc2 <- highchart(type = "stock") %>%
  hc_yAxis_multiples(
    list(top = "0%", height = "33%", title = list(text = "English Courses")),
    list(top = "33%", height = "33%", title = list(text = "Spanish Courses")),
    list(top = "66%", height = "33%", title = list(text = "German Courses"))
  ) %>%
  hc_add_series(courses_english, yAxis=0, name = "English Courses", color="blue") %>%
  hc_add_series(courses_spanish, yAxis=1, name = "Spanish Courses", color="red") %>%
  hc_add_series(courses_german, yAxis=2, name = "German Courses", color="green")


library(shiny)

ui <- fluidPage(
  actionButton("switch", "Kurse im Zeitverlauf"),
  highchartOutput("chart")
)

server <- function(input, output, session) {
  current_chart <- reactiveVal(hc1)
  
  observeEvent(input$switch, {
    if (identical(current_chart(), hc1)) {
      current_chart(hc2)
    } else {
      current_chart(hc1)
    }
  })
  
  output$chart <- renderHighchart({
    current_chart()
  })
}

shinyApp(ui, server)

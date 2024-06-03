library(tidyverse)
library(shiny)
library(highcharter)
library(xts)
library(htmlwidgets)

# Beispiel-Daten für hc1 erstellen
df_languages <- data.frame(
  language = c('Englisch', 'Deutsch', 'Spanisch'),
  value = c(3311, 1214, 1213),
  color = c("#195365", "#E73F0C", "#AFD700")
)

list_parse <- function(df) {
  lapply(1:nrow(df), function(i) {
    list(
      y = df$value[i],
      color = df$color[i],
      name = df$language[i]
    )
  })
}

# Beispiel-Daten für hc2 erstellen
dates <- seq(as.Date("2019-01-01"), as.Date("2024-01-04"), by = "year")
courses_english <- xts(c(2000, 3000, 3500, 4000, 3800, 3000), dates)
courses_spanish <- xts(c(1500, 2000, 3000, 3500, 3500, 3800), dates)
courses_german <- xts(c(1000, 1500, 2000, 2500, 3000, 3500), dates)

# Daten in data.frame umwandeln
df_english <- data.frame(date = index(courses_english), value = coredata(courses_english))
df_spanish <- data.frame(date = index(courses_spanish), value = coredata(courses_spanish))
df_german <- data.frame(date = index(courses_german), value = coredata(courses_german))
df_english$date <- format(df_english$date, "%Y")
df_spanish$date <- format(df_spanish$date, "%Y")
df_german$date <- format(df_german$date, "%Y")

# Highcharts-Theme mit Montserrat-Schriftart erstellen
highchart_theme <- hc_theme(
  chart = list(
    style = list(
      fontFamily = "Montserrat"
    )
  ),
  title = list(
    style = list(
      fontFamily = "Montserrat"
    )
  ),
  subtitle = list(
    style = list(
      fontFamily = "Montserrat"
    )
  ),
  xAxis = list(
    labels = list(
      style = list(
        fontFamily = "Montserrat"
      )
    )
  ),
  yAxis = list(
    labels = list(
      style = list(
        fontFamily = "Montserrat"
      )
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Montserrat"
    )
  ),
  tooltip = list(
    style = list(
      fontFamily = "Montserrat"
    )
  )
)

# Erstes Diagramm mit data.frame
hc1 <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Sprachen in Kursen im WS/2023") %>%
  hc_xAxis(categories = df_languages$language, title = list(text = "Sprachen")) %>%
  hc_yAxis(title = list(text = "Anzahl der Kurse")) %>%
  hc_add_series(name = "Sprachen", data = list_parse(df_languages)) %>%
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS("function() { return '<b>' + this.category + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Kurse'; }")
  ) %>%
  hc_legend(enabled = FALSE) %>%
  hc_add_theme(highchart_theme)

# Zweites Diagramm mit definierten Daten
hc2 <- highchart() %>%
  hc_yAxis(title = list(text = "Anzahl der Kurse")) %>%
  hc_xAxis(categories = as.character(df_english$date), title = list(text = "Semester")) %>%
  hc_add_series(name = "Englisch", data = df_english$value, color = "#195365") %>%
  hc_add_series(name = "Deutsch", data = df_spanish$value, color = "#E73F0C") %>%
  hc_add_series(name = "Spanisch", data = df_german$value, color = "#AFD700") %>%
  hc_add_theme(highchart_theme) %>%
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS("function() { return '<b>' + this.category + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Kurse'; }")
  )

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Montserrat', 'Helvetica Neue', 'Arial', sans-serif;
      }
      .switch {
        background-color: white;
        border: 1px solid #4a4a4a;
        color: #4a4a4a;
        padding: 5px 5px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 10px;
        margin: 4px 2px;
        cursor: pointer;
        border-radius: 40px;
        width: 150px;
        height: 40px;
        box-shadow: none;
        outline: none;
      }
      .switch:active {
         transform: translateY(2px);
      }
      .switch:hover {
        background-color: #f9f9f9;
      }
    "))
  ),
  fluidRow(
    column(6,
           actionButton("switch1", "Kurse im Zeitverlauf", class = "switch"),
           highchartOutput("chart1", height = "250px")
    ),
    column(6,
           actionButton("switch2", "Kurse im Zeitverlauf", class = "switch"),
           highchartOutput("chart2", height = "250px")
    )
  ),
  fluidRow(
    column(6,
           actionButton("switch3", "Kurse im Zeitverlauf", class = "switch"),
           highchartOutput("chart3", height = "250px")
    )
  )
)

server <- function(input, output, session) {
  current_chart1 <- reactiveVal(hc1)
  current_chart2 <- reactiveVal(hc1)
  current_chart3 <- reactiveVal(hc1)
  
  observeEvent(input$switch1, {
    if (identical(current_chart1(), hc1)) {
      current_chart1(hc2)
      updateActionButton(session, "switch1", label = "Sprachen im WS/2024")
    } else {
      current_chart1(hc1)
      updateActionButton(session, "switch1", label = "Kurse im Zeitverlauf")
    }
  })
  
  observeEvent(input$switch2, {
    if (identical(current_chart2(), hc1)) {
      current_chart2(hc2)
      updateActionButton(session, "switch2", label = "Sprachen im WS/2024")
    } else {
      current_chart2(hc1)
      updateActionButton(session, "switch2", label = "Kurse im Zeitverlauf")
    }
  })
  
  observeEvent(input$switch3, {
    if (identical(current_chart3(), hc1)) {
      current_chart3(hc2)
      updateActionButton(session, "switch3", label = "Sprachen im WS/2024")
    } else {
      current_chart3(hc1)
      updateActionButton(session, "switch3", label = "Kurse im Zeitverlauf")
    }
  })
  
  output$chart1 <- renderHighchart({
    current_chart1()
  })
  
  output$chart2 <- renderHighchart({
    current_chart2()
  })
  
  output$chart3 <- renderHighchart({
    current_chart3()
  })
}

shinyApp(ui, server)

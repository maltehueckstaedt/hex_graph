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
  hc_add_series(name = "Sprachen", data = list_parse(df_languages)) %>% # Ändere die Daten hier
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS("function() { return '<b>' + this.category + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Kurse'; }")
  )%>%
  hc_legend(enabled = FALSE) %>% # Blende die Legende aus
  hc_add_theme(highchart_theme) # Füge das Theme hinzu

# Zweites Diagramm mit definierten Daten
hc2 <- highchart() %>%
  hc_yAxis(title = list(text = "Anzahl der Kurse")) %>%
  hc_xAxis(categories = as.character(df_english$date), title = list(text = "Semester")) %>%
  hc_add_series(name = "Englisch", data = df_english$value, color = "#195365") %>%
  hc_add_series(name = "Deutsch", data = df_spanish$value, color = "#E73F0C") %>%
  hc_add_series(name = "Spanisch", data = df_german$value, color = "#AFD700") %>%
  hc_add_theme(highchart_theme) |> 
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
      #switch {
        background-color: white; /* Weiße Hintergrundfarbe */
        border: 1px solid #4a4a4a; /* Schwarze Konturlinie */
        color: #4a4a4a; /* Schwarze Textfarbe */
        padding: 5px 5px; /* Etwas Innenabstand */
        text-align: center; /* Zentrierter Text */
        text-decoration: none; /* Unterstreichen entfernen */
        display: inline-block; /* Als Blockelement anzeigen */
        font-size: 10px; /* Schriftgröße */
        margin: 4px 2px; /* Etwas Außenabstand */
        cursor: pointer; /* Hand-Cursor */
        border-radius: 40px; /* Abgerundete Ecken */
        width: 150px; /* Feste Breite */
        height: 40px; /* Feste Höhe */
        box-shadow: none; /* Kein Schatten */
        outline: none; /* Entfernen der Fokuslinie */
      }
  
      #switch:active {
         transform: translateY(2px); /* Button nach unten verschieben beim Klicken */
      }
  
      #switch:hover {
        background-color: #f9f9f9; /* Dunklerer Hintergrund beim Hover */
      }
    "))
  ),
  actionButton("switch", "Kurse im Zeitverlauf"),
  highchartOutput("chart")
)

server <- function(input, output, session) {
  current_chart <- reactiveVal(hc1)
  
  observeEvent(input$switch, {
    if (identical(current_chart(), hc1)) {
      current_chart(hc2)
      updateActionButton(session, "switch", label = "Sprachen im WS/2024")
    } else {
      current_chart(hc1)
      updateActionButton(session, "switch", label = "Sprachen zwischen <br> WS/2019 und WS/2024")
    }
  })
  
  output$chart <- renderHighchart({
    current_chart()
  })
}

shinyApp(ui, server)

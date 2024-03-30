library(shiny)
library(plotly)
library(forecast)

ui <- fluidPage(
  titlePanel("Análisis Predictivo con ARIMA - AirPassengers"),
  sidebarLayout(
    sidebarPanel(
      h3("Parámetros del Modelo ARIMA"),
      numericInput("p", "Orden AR (p):", 1),
      numericInput("d", "Orden de Diferenciación (d):", 1),
      numericInput("q", "Orden MA (q):", 0)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico Interactivo", plotlyOutput("airpass_plot")),
        tabPanel("Predicción ARIMA", plotlyOutput("arima_pred")),
        tabPanel("Resumen del Modelo", verbatimTextOutput("model_summary"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$airpass_plot <- renderPlotly({
    dates <- seq(as.Date("1949-01-01"), length = length(AirPassengers), by = "month")
    p <- plot_ly(x = dates, y = AirPassengers, type = 'scatter', mode = 'lines') %>%
      layout(title = 'AirPassengers 1949-1960',
             xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Número de Pasajeros'))
    return(p)
  })
  
  model <- reactive({
    arimaModel <- auto.arima(AirPassengers)
    return(arimaModel)
  })
  
  output$arima_pred <- renderPlotly({
    future <- forecast(model(), h = 12)
    last_date <- tail(seq(as.Date("1949-01-01"), length = length(AirPassengers), by = "month"), 1)
    dates_future <- seq(from = last_date, by = "month", length.out = 13)[-1] # Genera fechas futuras para la predicción
    p <- plot_ly() %>%
      add_lines(x = dates_future, y = future$mean, name = 'Predicción') %>%
      add_ribbons(x = dates_future, ymin = future$lower[,2], ymax = future$upper[,2], name = 'Intervalo de Confianza 95%', fillcolor = 'rgba(205, 12, 24, 0.4)') %>%
      layout(title = 'Predicción ARIMA para los próximos 12 meses',
             xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Número de Pasajeros'))
    return(p)
  })
  
  output$model_summary <- renderPrint({
    summary(model())
  })
}

shinyApp(ui = ui, server = server)

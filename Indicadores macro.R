library(shiny)
library(shinydashboard)
library(rbcb)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

# Indicadores
series <- list(
  "IPCA (%)" = 433,
  "IPCA 12m (%)" = 13522,
  "SELIC meta (%)" = 4390,
  "SELIC acumulada ano (%)" = 4189,
  "IBC-Br" = 24363,
  "PIB trimestral (%)" = 7326,
  "Inadimplência PF (%)" = 20727,
  "Crédito PF (R$ milhões)" = 20686,
  "Dívida Bruta (% PIB)" = 4514,
  "Índice de Commodities Brasil" = 11741
)

load_data <- function(codigo) {
  df <- get_series(codigo)
  rename(df, data = 1, valor = 2) %>%
    filter(data >= as.Date("2012-01-01"))
}

dados <- lapply(series, load_data)
names(dados) <- names(series)

#Interface

ui <- dashboardPage(
  dashboardHeader(title = "Indicadores Econômicos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "geral", icon = icon("dashboard")),
      menuItem("Indicadores Individuais", tabName = "individual", icon = icon("chart-line"))
    ),
    selectInput("indicador", "Escolha o indicador:", choices = names(series), selected = "IPCA (%)")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "geral",
              fluidRow(
                lapply(names(series), function(nome) {
                  box(title = nome, width = 6, plotlyOutput(paste0("plot_", gsub(" ", "_", nome))))
                })
              )
      ),
      tabItem(tabName = "individual",
              fluidRow(
                box(width = 12, plotlyOutput("plot_individual")),
                box(width = 12, downloadButton("download_data", "Baixar dados CSV"))
              )
      )
    )
  )
)

#servidor

server <- function(input, output) {
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("dados_", gsub(" ", "_", input$indicador), ".csv")
    },
    content = function(file) {
      write.csv(dados[[input$indicador]], file, row.names = FALSE)
    }
  )
  for (nome in names(series)) {
    local({
      indicador <- nome
      output[[paste0("plot_", gsub(" ", "_", indicador))]] <- renderPlotly({
        df <- dados[[indicador]] %>% filter(data >= as.Date("2018-01-01"))
        g <- ggplot(df, aes(x = data, y = valor)) +
          geom_line(color = "steelblue") +
          labs(title = indicador, x = "Data", y = NULL) +
          theme_minimal()
        ggplotly(g)
      })
    })
  }
  
  output$plot_individual <- renderPlotly({
    df <- dados[[input$indicador]]
    g <- ggplot(df, aes(x = data, y = valor)) +
      geom_line(color = "darkred") +
      labs(title = input$indicador, x = "Data", y = NULL) +
      theme_minimal()
    ggplotly(g)
  })
}

shinyApp(ui, server)


# server.R

library(shiny)
library(tidyverse)
library(tidytext)
library(sentimentr)
library(DT)
library(plotly)

server <- function(input, output) {

  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df
  })

  filtered_data <- reactive({
    df <- data()
    if (input$airline != "All") {
      df <- df %>% filter(airline == input$airline)
    }
    df
  })

  sentiment_data <- eventReactive(input$analyze, {
    df <- filtered_data()
    df %>%
      mutate(sentiment = sentiment_by(text)$ave_sentiment) %>%
      group_by(airline) %>%
      summarise(average_sentiment = mean(sentiment, na.rm = TRUE))
  })

  sentiment_plot_data <- eventReactive(input$analyze, {
    df <- filtered_data()
    df %>%
      mutate(sentiment = sentiment_by(text)$ave_sentiment)
  })

  output$raw_data <- DT::renderDataTable({
    data()
  })

  output$sentiment_summary <- DT::renderDataTable({
    sentiment_data()
  })

  output$sentiment_plot <- renderPlotly({
    plot_data <- sentiment_plot_data()

    plot_ly(plot_data, x = ~airline, y = ~sentiment, type = 'box', color = ~airline) %>%
      layout(title = "Sentiment Distribution by Airline")
  })
}

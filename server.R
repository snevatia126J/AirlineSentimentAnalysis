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
    df <- as.data.frame(lapply(df, as.character)) # Convert all columns to character
    df
  })

  sentiment_data <- eventReactive(input$analyze, {
    df <- data()
    if (nrow(df) > 0) {
      df %>%
        mutate(sentiment = sentiment_by(text)$ave_sentiment) %>%
        summarise(average_sentiment = mean(sentiment, na.rm = TRUE))
    } else {
      data.frame(average_sentiment = numeric())
    }
  })

  sentiment_plot_data <- eventReactive(input$analyze, {
    df <- data()
    if (nrow(df) > 0) {
      df %>%
        mutate(sentiment = sentiment_by(text)$ave_sentiment)
    } else {
      data.frame(sentiment = numeric())
    }
  })

  output$raw_data <- DT::renderDataTable({
    data()
  })

  output$sentiment_summary <- DT::renderDataTable({
    sentiment_data()
  })

  output$sentiment_plot <- renderPlotly({
    plot_data <- sentiment_plot_data()
    if (nrow(plot_data) > 0) {
      plot_ly(plot_data, y = ~sentiment, type = "box") %>%
        layout(title = "Sentiment Distribution")
    } else {
      plotly_empty()
    }
  })
}

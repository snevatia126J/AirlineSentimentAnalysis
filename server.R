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

  filtered_data <- reactive({
    df <- data()
    if (input$airline != "All") {
      df <- df %>% filter(airline == input$airline)
    }
    df
  })

  sentiment_data <- eventReactive(input$analyze, {
    df <- filtered_data()
    if(nrow(df) > 0){ #Check if there is data to analyze
      df %>%
        mutate(sentiment = sentiment_by(text)$ave_sentiment) %>%
        group_by(airline) %>%
        summarise(average_sentiment = mean(sentiment, na.rm = TRUE))
    } else {
      data.frame(airline = character(), average_sentiment = numeric()) #Return an empty data frame if no data
    }
  })

  sentiment_plot_data <- eventReactive(input$analyze, {
    df <- filtered_data()
    if(nrow(df) > 0){
      df %>%
        mutate(sentiment = sentiment_by(text)$ave_sentiment)
    } else {
      data.frame(airline = character(), sentiment = numeric()) #Return an empty data frame if no data
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
    if(nrow(plot_data) > 0){
      plot_ly(plot_data, x = ~airline, y = ~sentiment, type = 'box', color = ~airline) %>%
        layout(title = "Sentiment Distribution by Airline")
    } else {
      plotly_empty() #Return an empty plotly plot if no data
    }
  })
}

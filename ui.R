# ui.R

library(shiny)
library(DT)
library(plotly)

ui <- fluidPage(
  titlePanel("Airline Tweet Sentiment Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File (airline_tweets_sample 1k.csv)", accept = c(".csv")),
      selectInput("airline", "Select Airline (Optional)", choices = c("All", "US Airways", "United", "American", "Southwest", "Delta", "Virgin America")),
      actionButton("analyze", "Analyze Sentiment")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
        tabPanel("Sentiment Summary", DT::dataTableOutput("sentiment_summary")),
        tabPanel("Sentiment Plot", plotlyOutput("sentiment_plot"))
      )
    )
  )
)

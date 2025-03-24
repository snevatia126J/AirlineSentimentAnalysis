
# ui.R

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Airline Sentiment Analysis"),
  dashboardSidebar(
    fileInput("file1", "Upload CSV File", accept = ".csv"),
    selectInput("sentiment", "Choose Sentiment Lexicon:",
                choices = c("bing", "afinn", "nrc"), selected = "bing"),
    checkboxGroupInput("airlines", "Select Airlines to Compare:", choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      box(title = "Sentiment Summary", width = 12, solidHeader = TRUE, status = "primary",
          tableOutput("summaryTable")
      )
    ),
    fluidRow(
      box(title = "Sentiment Distribution", width = 6, plotOutput("barPlot")),
      box(title = "Word Cloud", width = 6, plotOutput("wordcloudPlot"))
    )
  )
)

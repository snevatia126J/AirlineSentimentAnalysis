# ui.R
source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Airline Tweet Sentiment"),
  dashboardSidebar(
    fileInput("file1", "Upload CSV File", accept = ".csv"),
    selectInput("sentiment", "Select Sentiment Lexicon:", choices = c("bing", "afinn", "nrc"), selected = "bing"),
    checkboxGroupInput("airlines", "Filter Airlines:", choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      box(title = "Sentiment Summary Table", width = 12, status = "primary", solidHeader = TRUE,
          tableOutput("summaryTable"))
    ),
    fluidRow(
      box(title = "Sentiment Bar Plot", width = 6, plotOutput("barPlot")),
      box(title = "Word Cloud", width = 6, plotOutput("wordcloudPlot"))
    )
  )
)

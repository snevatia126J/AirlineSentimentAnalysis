
# server.R

library(shiny)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)
library(SnowballC)
library(readr)
library(stringr)
library(tidyr)

server <- function(input, output, session) {

  dataInput <- reactive({
    req(input$file1)
    df <- read_csv(input$file1$datapath)
    df <- df %>% filter(!is.na(text))

    # Extract airline name from handle (e.g., @JetBlue)
    df$airline <- stringr::str_extract(df$text, "@\\w+")
    df$airline <- gsub("@", "", df$airline)
    df$airline <- tolower(df$airline)

    updateCheckboxGroupInput(session, "airlines", choices = unique(df$airline), selected = unique(df$airline))
    return(df)
  })

  cleanTokens <- reactive({
    df <- dataInput()
    df <- df %>% filter(airline %in% input$airlines)
    df$text <- tolower(df$text)
    df$text <- gsub("http\\S+", "", df$text)
    df$text <- gsub("[^a-zA-Z\\s]", "", df$text)

    tidy_df <- df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word")

    tidy_df
  })

  sentimentResults <- reactive({
    lexicon <- input$sentiment
    tidy_df <- cleanTokens()
    sentiments <- get_sentiments(lexicon)

    sentiment_df <- tidy_df %>%
      inner_join(sentiments, by = "word") %>%
      count(airline, sentiment, sort = TRUE)

    sentiment_df
  })

  output$summaryTable <- renderTable({
    sentiment_df <- sentimentResults()
    sentiment_df %>% pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  })

  output$barPlot <- renderPlot({
    sentiment_df <- sentimentResults()
    ggplot(sentiment_df, aes(x = airline, y = n, fill = sentiment)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Sentiment Count by Airline", y = "Count", x = "Airline")
  })

  output$wordcloudPlot <- renderPlot({
    tidy_df <- cleanTokens()
    words <- tidy_df %>% count(word, sort = TRUE)
    wordcloud(words = words$word, freq = words$n, max.words = 100, colors = brewer.pal(8, "Dark2"))
  })
}

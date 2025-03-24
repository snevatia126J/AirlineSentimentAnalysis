
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

    # Properly escaped backslash for regex
    df$airline <- stringr::str_extract(df$text, "@\\\w+")
    df$airline <- gsub("@", "", df$airline)
    df$airline <- tolower(df$airline)

    updateCheckboxGroupInput(session, "airlines", choices = unique(df$airline), selected = unique(df$airline))
    return(df)
  })

  cleanTokens <- reactive({
    df <- dataInput()
    df <- df %>% filter(airline %in% input$airlines)
    df$text <- tolower(df$text)

    # Fix all escaping issues
    df$text <- gsub("http\S+|https\S+", "", df$text)
    df$text <- gsub("@\w+", "", df$text)
    df$text <- gsub("#", "", df$text)
    df$text <- gsub("[^a-z\s]", "", df$text)
    df$text <- gsub("\s+", " ", df$text)

    tidy_df <- df %>%
      unnest_tokens(word, text, token = "words") %>%
      filter(nchar(word) > 2 & nchar(word) < 15) %>%
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
    wordcloud(words = words$word,
              freq = words$n,
              max.words = 100,
              colors = brewer.pal(8, "Dark2"),
              scale = c(4, 0.8),
              random.order = FALSE)
  })
}

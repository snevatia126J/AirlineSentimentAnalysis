library(shiny)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(stringr)
library(tidyr)

server <- function(input, output, session) {

  dataInput <- reactive({
    req(input$file1)
    df <- read_csv(input$file1$datapath, col_types = cols(text = col_character()))
    df <- df %>% filter(!is.na(text))

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

    df$text <- gsub("http\\S+|https\\S+", "", df$text)
    df$text <- gsub("@\\w+", "", df$text)
    df$text <- gsub("#", "", df$text)
    df$text <- gsub("[^a-z\\s']", "", df$text)
    df$text <- gsub("\\s+", " ", df$text)

    tidy_df <- df %>%
      unnest_tokens(word, text, token = "words") %>%
      filter(nchar(word) > 2) %>%
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
    words <- tidy_df %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(word, sentiment, sort = TRUE) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
      mutate(sentiment_score = positive - negative)

    # Debugging: check if there are words and sentiment scores
    print(head(words)) # Print the first few rows of the words data frame

    if (nrow(words) == 0 || all(is.na(words$sentiment_score))) {
      plot.new()
      text(0.5, 0.5, "No meaningful words to display", cex = 1.5)
    } else {
      # Filter out rows with NA sentiment scores
      words <- words %>% filter(!is.na(sentiment_score))
      if (nrow(words) > 0) {
        wordcloud(words = words$word,
                  freq = words$sentiment_score,
                  max.words = 100,
                  colors = brewer.pal(8, "Dark2"),
                  scale = c(4, 0.8),
                  random.order = FALSE)
      } else {
        plot.new()
        text(0.5, 0.5, "No words with sentiment scores to display", cex = 1.5)
      }
    }
  })
}

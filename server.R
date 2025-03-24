# server.R

library(shiny)
library(tidyverse)
library(tidytext)
library(sentimentr)
library(wordcloud)
library(RColorBrewer)

server <- function(input, output) {

  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- as.data.frame(lapply(df, as.character))
    df
  })

  sentiment_words <- eventReactive(input$analyze, {
    df <- data()
    if (nrow(df) > 0) {
      df %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
    } else {
      data.frame(word = character(), sentiment = character(), n = numeric())
    }
  })

  output$sentiment_wordcloud <- renderPlot({
    word_data <- sentiment_words()
    if (nrow(word_data) > 0) {
      word_data %>%
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
        mutate(sentiment_score = positive - negative) %>%
        arrange(desc(abs(sentiment_score))) %>%
        top_n(100, abs(sentiment_score)) %>%
        with(wordcloud(word, sentiment_score, random.order = FALSE,
                         colors = brewer.pal(8, "Dark2")))
    } else {
      plot.new() # Empty plot
    }
  })

  output$raw_data <- DT::renderDataTable({
    data()
  })
}

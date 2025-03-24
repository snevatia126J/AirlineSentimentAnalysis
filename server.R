# server.R
source("global.R")

server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$file1)
    df <- read_csv(input$file1$datapath)
    df <- df %>% filter(!is.na(text))
    df$airline <- str_extract(df$text, "@\\w+") %>% str_remove("@") %>% tolower()
    updateCheckboxGroupInput(session, "airlines", choices = unique(df$airline), selected = unique(df$airline))
    df
  })
  
  tidy_tokens <- reactive({
    df <- raw_data()
    df <- df %>% filter(airline %in% input$airlines)
    df$text <- clean_text(df$text)
    
    tidy_df <- df %>%
      unnest_tokens(word, text) %>%
      filter(nchar(word) > 2) %>%
      anti_join(stop_words, by = "word")
    
    tidy_df
  })
  
  sentiment_data <- reactive({
    lexicon <- input$sentiment
    tidy_df <- tidy_tokens()
    sentiments <- get_sentiments(lexicon)
    
    tidy_df %>%
      inner_join(sentiments, by = "word") %>%
      count(airline, sentiment, sort = TRUE)
  })

  output$summaryTable <- renderTable({
    sentiment_data() %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  })
  
  output$barPlot <- renderPlot({
    df <- sentiment_data()
    ggplot(df, aes(x = airline, y = n, fill = sentiment)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Sentiment by Airline", x = "Airline", y = "Count") +
      theme_minimal()
  })
  
  output$wordcloudPlot <- renderPlot({
    tidy_df <- tidy_tokens()
    wc_data <- tidy_df %>% count(word, sort = TRUE)
    wordcloud(words = wc_data$word, freq = wc_data$n, max.words = 100,
              scale = c(3.5, 0.7), colors = brewer.pal(8, "Dark2"))
  })
}

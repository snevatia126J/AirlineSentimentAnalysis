# global.R
library(shiny)
library(shinydashboard)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)

# Load sentiment lexicons
data("stop_words")

# Helper function to clean tweet text
clean_text <- function(text) {
  text %>%
    tolower() %>%
    str_replace_all("http\\S+|https\\S+", "") %>%
    str_replace_all("@\\w+", "") %>%
    str_replace_all("#", "") %>%
    str_replace_all("[^a-z\\s]", "") %>%
    str_replace_all("\\s+", " ")
}

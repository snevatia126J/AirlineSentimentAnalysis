
# dependencies.R

packages <- c("shiny", "tidytext", "dplyr", "ggplot2", "wordcloud", "tm",
              "SnowballC", "readr", "shinydashboard", "stringr", "tidyr")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(sapply(packages, install_if_missing))

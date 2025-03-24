# dependencies.R

# List of required packages
required_packages <- c("shiny", "tidyverse", "tidytext", "sentimentr", "DT", "plotly")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  missing_packages <- setdiff(packages, rownames(installed.packages()))
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

# Install missing packages
install_missing_packages(required_packages)

# Load required packages
lapply(required_packages, require, character.only = TRUE)

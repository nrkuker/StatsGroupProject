# Question 4 ####

# Packages ####
library(tidyverse)

# Load Data ####
bikes <- read.csv("data/Capital Bike Sharing data by hour.csv") %>% 
  as_tibble()

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

season_levels <- c("Winter", "Spring", "Summer", "Fall")


# QUESTION 4 ##########
# What type of relationship do you see between weather and bike rental?
# Is the relationship the same for registered vs. casual users?



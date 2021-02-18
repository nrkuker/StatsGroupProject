# Question 2 ####

# Packages ####
library(tidyverse)

# Load Data ####
bikes <- read.csv("data/Capital Bike Sharing data by hour.csv") %>% 
  as_tibble()

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
season_levels <- c("Winter", "Spring", "Summer", "Fall")
weather_levels <- c("Clear", "Cloudy", "Light Precip.", "Heavy Precip.")
year_levels <- c(2011, 2012)

total_riders <- bikes %>% summarise(totalRiders = sum(cnt)) %>% as.numeric()  # 3,292,679
total_cas <- bikes %>% summarise(totalCas = sum(casual)) %>% as.numeric()
total_reg <- bikes %>% summarise(totalReg = sum(registered)) %>% as.numeric()
prop_reg <- round(total_reg/total_riders, 4)

# Processed datasets ####
season <- c(1,2,3,4)
Seasons <- data.frame(season, season_levels)

mnth <- c(1:12)
Month <- data.frame(mnth, month_levels)

yr <- c(0,1)
Years <- data.frame(yr, year_levels)

# QUESTION 2  ##########
#The data science group at Capital bike share hypothesize that:
#a. There must be high demand during office timings:
#Early morning and late evening can have different trend (cyclist) and low demand during 10:00pm to 4:00am. 
#Do you agree? 
#b. Registered users demand more bike on weekdays compared to the weekend or holiday. Do you agree?

#Create map of hourly demand to time of day windows
TOD <- read.csv("data/TOD.csv") %>% 
  as_tibble()
names(TOD)[names(TOD) == "ï..Hour"] <- "Hour"

#Create clean dataset for hourly demand
Hypothesis <- select(bikes, Hour = hr, Weekday = weekday, Holiday = holiday, Workingday = workingday, 
         Casual = casual, Registered = registered, Total = cnt) 

Hypothesis <- left_join(Hypothesis, TOD, by = "Hour")

#Create summary of hourly demand by TOD
Demand_Sum <- Hypothesis %>%
  group_by(TOD) %>%
  summarise(Avg_Demand = mean(Total), .groups = 'drop')

Demand_Sum
# Question 3 ####

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

# QUESTION 3  ##########
# Is there any relationship between season and bike rental? 
# Create a visualization displaying the relationship.

# rel b/t categorical (ordinal) and quant (contin, sorta)
# bar, box, dotplot, violin

#Join Season Name to Bike Demand Data Frame
Bike_Season <- bikes
Bike_Season <- left_join(Bike_Season, Seasons, by = "season")
Bike_Season <- left_join(Bike_Season, Years, by = "yr")

#Create Count for Days in Season
Bike_Season$days_cnt <- 1
str(Bike_Season)

#Calculate Daily Demand by Season
Bike_Seasonality <- Bike_Season %>%
  select(Season = season_levels, Year = year_levels, Date = dteday, Demand = cnt, Count = days_cnt) %>%
  group_by(Season, Year) %>%
  summarise(Daily_Demand = sum(Demand)/sum(Count))

Bike_Seasonality$Season <- factor(Bike_Seasonality$Season, levels = season_levels)

#Create Bar Plot
ggplot(Bike_Seasonality, aes(x=Year, y=Daily_Demand, fill=Season)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Paired")

#Thoughts ####

#As expected, daily demand spikes in the summer and is fairly consistent between spring and fall.
#Winter is seriously depressed compared to the other seasons.

#Join Month Name to Bike Demand Data Frame
Bike_Month <- bikes
Bike_Month <- left_join(Bike_Month, Month, by = "mnth")
Bike_Month <- left_join(Bike_Month, Years, by = "yr")

#Create Count for Hours in Month
Bike_Month$hours_cnt <- 1
str(Bike_Month)

#Calculate Hourly Demand by Month
Bike_Monthly <- Bike_Month %>%
  select(Month = month_levels, Year = year_levels, Date = dteday, Demand = cnt, Count = hours_cnt) %>%
  group_by(Month, Year) %>%
  summarise(Hourly_Demand = sum(Demand)/sum(Count))

Bike_Monthly$Month <- factor(Bike_Monthly$Month, levels = month_levels)

#Create Bar Plot
ggplot(Bike_Monthly, aes(x=Year, y=Hourly_Demand, fill=Month)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Paired")

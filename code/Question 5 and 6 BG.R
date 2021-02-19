# Questions 5 and 6  ####

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

bikes$temp <- bikes$temp * 41
bikes$atemp <- bikes$atemp * 50

# Question 5 #####
#Fit a linear model predicting the total bike rental demand from daily temperature. 
#What kind of insights can you generate?
#(make sure to write the linear model and interpret it in the context of the data)

Temp_Plot <- bikes %>%
  ggplot(aes(x = temp,
             y = cnt)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="grey60") 

Temp_Plot

Predict <- lm(cnt ~ temp, data = bikes)
summary(Predict)
coefficients(Predict)

#demand = -0.0356 + (9.2999 * temp)
#low r squared
#wide range of demand at every temp
#statistically significant (p value <= alpha of 0.05)

#Question 6 ##### 
#Fit another linear model predicting total daily bike rentals from daily feeling temperature. 
#Write the linear model, interpret the slope etc. 
#Is the temperature or feeling temperature a better predictor of bike rentals?

Atemp_Plot <- bikes %>%
  ggplot(aes(x = atemp,
             y = cnt)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="grey60") 

Atemp_Plot

Predict_A <- lm(cnt ~ atemp, data = bikes)
summary(Predict_A)
coefficients(Predict_A)

#demand = -11.88 + (8.46 * atemp)
#slightly lower r squared
#wide range of demand at each feels like temp and less steep slope so each degree
#increase in feels like temp is less impactful on demand than actual temp
#statistically significant (p value <= alpha of 0.05)
#temp is a better predictor of bike rentals (r squared, standard error)

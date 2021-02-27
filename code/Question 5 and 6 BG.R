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

bikes$temp_c <- bikes$temp * 41
bikes$atemp_c <- bikes$atemp * 50

# Question 5 #####
#Fit a linear model predicting the total bike rental demand from daily temperature. 
#What kind of insights can you generate?
#(make sure to write the linear model and interpret it in the context of the data)

Temp_Cor <- cor.test(bikes$temp, bikes$cnt, method = "pearson")
Temp_Cor

#The p-value of r is less than alpha a 0.05 so we can conclude that temp and demand are
#correlated with a correlation coefficient of 0.405

Temp_Plot <- bikes %>%
  ggplot(aes(x = temp,
             y = cnt)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="blue") +
  labs(title = "Daily Temperature versus Rentals", 
       x = "Daily Temperature (Normalised)", y = "Total Riders") +
  theme_igray() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 11))

Temp_Plot

Predict <- lm(cnt ~ temp, data = bikes)
summary(Predict)
coefficients(Predict)

#demand = -0.0356 + (381.295 * temp)
#low r squared
#wide range of demand at every temp
#statistically significant (p value <= alpha of 0.05)

#Question 6 ##### 
#Fit another linear model predicting total daily bike rentals from daily feeling temperature. 
#Write the linear model, interpret the slope etc. 
#Is the temperature or feeling temperature a better predictor of bike rentals?

ATemp_Cor <- cor.test(bikes$atemp, bikes$cnt, method = "pearson")
ATemp_Cor

#The p-value of r is less than alpha a 0.05 so we can conclude that a_temp and demand are
#correlated with a correlation coefficient of 0.401

Atemp_Plot <- bikes %>%
  ggplot(aes(x = atemp,
             y = cnt)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="blue") +
  labs(title = "Daily Feeling Temperature versus Rentals", 
       x = "Daily Feeling emperature (Normalised)", y = "Total Riders") +
  theme_igray() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 11))

Atemp_Plot

Predict_A <- lm(cnt ~ atemp, data = bikes)
summary(Predict_A)
coefficients(Predict_A)

#demand = -11.88 + (423.18 * atemp)
#slightly lower r squared
#wide range of demand at each feels like temp and less steep slope so each degree
#increase in feels like temp is less impactful on demand than actual temp
#statistically significant (p value <= alpha of 0.05)
#temp is a better predictor of bike rentals (r squared, standard error)

# Split by year ####

# Filter by year
bikes <- left_join(bikes, Years)

Temp_2011 <- bikes %>%
  filter(year_levels == 2011)

Temp_2012 <- bikes %>%
  filter(year_levels == 2012)

Predict_2011 <- lm(cnt ~ temp, data = Temp_2011)
summary(Predict_2011)
coefficients(Predict_2011)

Predict_2012 <- lm(cnt ~ temp, data = Temp_2012)
summary(Predict_2012)
coefficients(Predict_2012)

#demand = -0.0356 + (381.295 * temp)
#low r squared
#wide range of demand at every temp
#statistically significant (p value <= alpha of 0.05)

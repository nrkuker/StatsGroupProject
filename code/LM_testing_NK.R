# Questions 5 and 6  ####

# Packages ####
library(tidyverse)
library(moments)

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
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  geom_abline(slope = finalcoef[2], intercept = finalcoef[1])


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

Atemp_Plot <- bikes %>%
  ggplot(aes(x = atemp,
             y = cnt)) + 
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = lm, se = FALSE, color="red") 

Atemp_Plot

Predict_A <- lm(sqrt(cnt) ~ atemp, data = bikes)
summary(Predict_A)
coefficients(Predict_A)

#demand = -11.88 + (423.18 * atemp)
#slightly lower r squared
#wide range of demand at each feels like temp and less steep slope so each degree
#increase in feels like temp is less impactful on demand than actual temp
#statistically significant (p value <= alpha of 0.05)
#temp is a better predictor of bike rentals (r squared, standard error)

summary(lm(data = bikes[bikes$season == 4, ], formula = casual ~ temp))

bikes %>%
  filter(holiday == 1) %>% 
  ggplot(aes(x = temp,
             y = casual)) + 
  geom_point(aes(color = factor(season))) +
  geom_smooth(method = lm, se = FALSE, color="black") #+
  # geom_smooth(data = subset(bikes, season == 1),
  #             method = lm, se = FALSE, color="red") +
  # geom_smooth(data = subset(bikes, season == 2),
  #             method = lm, se = FALSE, color="green") +
  # geom_smooth(data = subset(bikes, season == 3),
  #             method = lm, se = FALSE, color="blue") +
  # geom_smooth(data = subset(bikes, season == 4),
  #             method = lm, se = FALSE, color="purple")
  
# R-squared of .3478 for holidays, cnt ~ temp
Predict_hols <- lm(data = bikes[bikes$holiday == 1, ], formula = cnt ~ temp)

# R-squared of .4653 for holidays, casual ~ temp
Predict_holscas <- lm(data = bikes[bikes$holiday == 1, ], formula = casual ~ temp)


bikes %>%
  ggplot(aes(x = temp,
             y = registered)) + 
  geom_jitter(alpha = 0.2) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1000))




ggplot() + geom_point(aes(x = Predict$fitted.values, y = Predict$residuals))

ggplot() + geom_point(aes(x = Predict_A$fitted.values, y = Predict_A$residuals))

ggplot() + geom_point(aes(x = Predict_hols$fitted.values, y = Predict_hols$residuals))
ggplot() + geom_point(aes(x = Predict_hols$fitted.values, y = Predict_holscas$residuals))


cor(x = bikes$temp, y = (bikes$cnt), method = "pearson")
cor(x = bikes$atemp, y = (bikes$cnt), method = "pearson")
# strongest correlation between temp & casual, lowest atemp & registered

bikes %>% filter(holiday == 1) %>% 
  summarise(cor_cnt = cor(temp, cnt, method = "pearson"),
            cor_cas = cor(temp, casual, method = "pearson"),
            cor_reg = cor(temp, registered, method = "pearson"),
            )


ggplot() + geom_histogram(aes(Predict$residuals))
ggplot() + geom_histogram(aes(Predict_A$residuals))
ggplot() + geom_histogram(aes(Predict_hols$residuals))
ggplot() + geom_histogram(aes(Predict_holscas$residuals))


var(Predict$residuals)
var(Predict_A$residuals)
var(Predict_hols$residuals)
var(Predict_holscas$residuals)

skewness(Predict$residuals)
skewness(Predict_A$residuals)
skewness(Predict_hols$residuals)
skewness(Predict_holscas$residuals)

kurtosis(Predict$residuals)
kurtosis(Predict_A$residuals)
kurtosis(Predict_hols$residuals)
kurtosis(Predict_holscas$residuals)

qqline(#x = Predict$fitted.values,
       y = Predict$residuals)
# idk what this is telling me


# neither Predict nor Predict_A are very strong models
  # residuals are not normally distributed
  # residuals vs fitted values not random

# Thoughts:
# Use time of day that Brendan did in Q2 to evaluate based on time of day





# 
# logSalePrice <-log(ames2$SalePrice)
# 
# ggplot(ames2) +
#   aes(x = Gr.Liv.Area, y = logSalePrice) +
#   geom_point(colour = "#0c4c8a") +
#   theme_minimal()
# 
# 
# model2 <- lm(logSalePrice ~ Gr.Liv.Area, data = ames2)
# summary(model2)
# coef  <- coefficients(model2)       # coefficients
# 
# finalcoeff = exp(coef)
# finalcoeff
# 
# #For 100 square feet increase in above ground living area, exponentiate the coeff*100
# 
exp(.0005939377*100)
exp(.0005939377)+exp(100)


bikes_transform <- bikes %>% 
  mutate(cnt_sqrt = sqrt(cnt),
         cnt_ln = log(cnt),
         cnt_log = log10(cnt))


bikes_transform %>% 
  # filter(season == 3) %>%
  ggplot(aes(
    x = (temp),
    y = sqrt(casual)
  )) +
  geom_jitter(alpha = 0.2) #+ 
  #geom_abline(slope = finalcoef[2], intercept = finalcoef[1])
  # geom_smooth(method = "lm", se = FALSE, color = "red")

cor(bikes$temp, sqrt(bikes$cnt))

ols.sqrt.y <- lm(data = bikes, formula = sqrt(cnt) ~ temp)
summary(ols.sqrt.y)
coef <- coefficients(ols.sqrt.y)

finalcoef <- coef^2
finalcoef[1]


ols.sqrt.y.casual <- lm(data = bikes, formula = sqrt(casual) ~ temp)
summary(ols.sqrt.y.casual)

ols.sqrt.y.registered <- lm(data = bikes, formula = sqrt(registered) ~ temp)
summary(ols.sqrt.y.registered)


ols.y <- lm(data = bikes, formula = (cnt) ~ temp)
summary(ols.y)

ols.y.casual <- lm(data = bikes, formula = (casual) ~ temp)
summary(ols.y.casual)

ols.y.registered <- lm(data = bikes, formula = (registered) ~ temp)
summary(ols.y.registered)



# Days of month? ####
days <- bikes %>% 
  mutate(day = str_sub(dteday, 9, 10)) %>% 
  select(instant, dteday, day)
table(days$day)
ggplot(data = days) + geom_bar(aes(x = day))

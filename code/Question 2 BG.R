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


#Create subset of hourly demand for day-time
Day_Demand <- Hypothesis %>%
  filter(Window == "Day")

#Create mean of hourly demand for day
Avg_DayDemand <- mean(Day_Demand$Total)

#Create subset of hourly demand for office peak
Office_Peak <- Hypothesis %>%
  filter(TOD == "Office Peak")

#Null hypothesis: Office Peak Demand >= Total Demand

#Test whether office peak demand is greater than total demand
t.test(Office_Peak$Total, mu=Avg_DayDemand, alternative="less")

#Conclusion: accept the null as p-value is greater than alpha 0.05

#Create subset of hourly demand for night
Night <- Hypothesis %>%
  filter(TOD == "Night")

#Null hypothesis: Night Demand <= Total Demand

#Test whether night demand is less than total demand
t.test(Night$Total, mu=Avg_DayDemand, alternative="greater")

#Conclusion: accept the null as p-value is greater than alpha 0.05
sd(Office_Peak$Total)
# Alternative Approach:
#Null hypothesis: Office Peak Demand >= Total Demand
xbar=309.4155             #sample mean
mu0=259.8624             #hypothesized value
sd(Day_Demand$Total)    # population SD
sigma=202.5208             #population standard deviation
n=5824                  #sample size

t= (xbar-mu0)/(sigma/sqrt(n))
t                     #test statistic

pval = pt(t, df=5823, lower.tail=TRUE)
pval                  #lower tail p value (pt gives the probability distribution function)

#Conclusion: accept the null as p-value is greater than alpha 0.05

#Null hypothesis: Night Demand <= Total Demand
xbar=50.2096            #sample mean
mu0=189.4631           #hypothesized value
sd(Day_Demand$Total)    # population SD
sigma=58.44815             #population standard deviation
n=5015                  #sample size

t= (xbar-mu0)/(sigma/sqrt(n))
t                     #test statistic

pval = pt(t, df=5014, lower.tail=FALSE)
pval                  #lower tail p value (pt gives the probability distribution function)

#Conclusion = accept the null as p-value is greater than alpha 0.05

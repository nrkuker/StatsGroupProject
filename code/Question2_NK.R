# Question 2 ####

# Nicole note to self:
# not factoring in day of week when conducting hypothesis tests for office hours


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
Day <- Hypothesis %>%
  filter(Window == "Day")

#Create mean of hourly demand for day
Avg_DayDemand <- mean(Day$Total)

#Create subset of hourly demand for office peak
Office <- Hypothesis %>%
  filter(TOD == "Office")

Avg_OfficeDemand <- mean(Office$Total)

#Create subset of hourly demand for night
Night <- Hypothesis %>%
  filter(TOD == "Night")

#Create subset of hourly demand for day trough
Trough <- Hypothesis %>%
  filter(TOD == "Trough")

Trough_Demand <- mean(Trough$Total)

# Create subset of demand during commute times
Commute <- Hypothesis %>% 
  filter(Commute == 1)

Noncommute <- Hypothesis %>% 
  filter(Commute == 0, TOD == "Office")



# DAY TIME ####

#Null hypothesis: Office Peak Demand >= Total Demand

#Test whether office peak demand is greater than total demand
t.test(Office$Total, mu=Avg_DayDemand, alternative="two.sided")

#Conclusion: accept the null as p-value is greater than alpha 0.05



# NK EDIT:

# H0: avg demand during daylight == avg demand over entire day
# Ha: avg demand during daylight != 

t.test(Day$Total, 
       mu = mean(Hypothesis$Total, na.rm = T), 
      alternative = "two.sided")
# reject null

# H0: avg demand during office hours == avg demand during daylight
# Ha: avg demand during office hours != 

t.test(Office$Total, 
       mu = mean(Day$Total, na.rm = T), 
       alternative = "two.sided")
# reject null, at alpha 0.05 and 0.01
# 95% CI for avg demand during office hours is [283.0607, 291.2821], x-bar = 287.1714
# entire 95% CI above avg demand during daylight












# NIGHT TIME ####

#Null hypothesis: Night Demand <= Total Demand

#Test whether night demand is less than total demand
t.test(Night$Total, mu=Avg_DayDemand, alternative="greater")

#Conclusion: accept the null as p-value is greater than alpha 0.05


# NK edits:

# H0: avg demand during nighttime == avg demand over entire day
# Ha: avg demand during nighttime !=

t.test(Night$Total, 
       mu = mean(Hypothesis$Total, na.rm = T), 
       alternative = "two.sided")
# absolutely reject null










# Alternative Approach: #####
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

# Trough vs Peak #####

#Null hypothesis: Office Peak Demand >= Trough Demand

#Test whether office peak demand is greater than trough demand
t.test(Office$Total, mu=Trough_Demand, alternative="less")



# NK edits:

# H0: avg demand during trough == avg demand over entire day
# Ha: avg demand during trough !=

t.test(Trough$Total, 
       mu = mean(Hypothesis$Total, na.rm = T), 
       alternative = "two.sided")
# reject null, trough not equal, trough less than


# H0: avg demand during trough <= avg demand during night
# Ha: avg demand during trough >

t.test(Trough$Total, 
       mu = mean(Night$Total, na.rm = T), 
       alternative = "greater")
# reject null


# H0: avg demand during trough <= avg demand during daylight
# Ha: avg demand during trough >

t.test(Trough$Total, 
       mu = mean(Day$Total, na.rm = T), 
       alternative = "two.sided")
# accept null





# NK edits:
# COMMUTE TIMES

# H0: avg demand during commute times == avg demand during office hours
# Ha: avg demand during commute times !=

t.test(Commute$Total, 
       mu = mean(Office$Total, na.rm = T), 
       alternative = "two.sided")
# reject null

# H0: avg demand during commute times <= avg demand during office hours
# Ha: avg demand during commute times >
t.test(Commute$Total, 
       mu = mean(Office$Total, na.rm = T), 
       alternative = "greater")
# reject null


# H0: avg demand during commute times == avg demand during noncommute office hours
# Ha: avg demand during commute times !=
t.test(Commute$Total, 
       mu = mean(Noncommute$Total, na.rm = T), 
       alternative = "two.sided")
# reject null

t.test(Commute$Total, 
       mu = mean(Noncommute$Total, na.rm = T), 
       alternative = "greater")
# reject null


# H0: avg demand during noncommute office hours == avg demand during daylight
# Ha: avg demand during noncommute office hours !=
t.test(Noncommute$Total,
       mu = mean(Day$Total, na.rm = T),
       alternative = "two.sided")
# reject null

t.test(Noncommute$Total,
       mu = mean(Day$Total, na.rm = T),
       alternative = "less")
# reject null


# H0: avg demand during noncommute office hours == avg demand during trough
# Ha: avg demand during noncommute office hours !=
t.test(Noncommute$Total,
       mu = mean(Trough$Total, na.rm = T),
       alternative = "two.sided")
# reject null



# For completeness' sake
t.test(Trough$Total,
       mu = mean(Office$Total, na.rm = T),
       alternative = "two.sided")
t.test(Night$Total,
       mu = mean(Office$Total, na.rm = T),
       alternative = "two.sided")
t.test(Night$Total,
       mu = mean(Day$Total, na.rm = T),
       alternative = "two.sided")
t.test(Commute$Total,
       mu = mean(Day$Total, na.rm = T),
       alternative = "two.sided")
t.test(Commute$Total,
       mu = mean(Trough$Total, na.rm = T),
       alternative = "two.sided")
t.test(Commute$Total,
       mu = mean(Night$Total, na.rm = T),
       alternative = "two.sided")
t.test(Noncommute$Total,
       mu = mean(Office$Total, na.rm = T),
       alternative = "two.sided")
t.test(Noncommute$Total,
       mu = mean(Night$Total, na.rm = T),
       alternative = "two.sided")
t.test(Office$Total,
       mu = mean(Hypothesis$Total, na.rm = T),
       alternative = "two.sided")
t.test(Commute$Total,
       mu = mean(Hypothesis$Total, na.rm = T),
       alternative = "two.sided")
t.test(Noncommute$Total,
       mu = mean(Hypothesis$Total, na.rm = T),
       alternative = "two.sided")




# Two population hypothesis test #####

# Null: Difference between two means is equal

t.test(formula = Total ~ TOD, data = Hypothesis, subset = TOD %in% c("Trough", "Office"))

#Conclusion = reject the null as p


Hypothesis_2 <- Hypothesis %>%
  group_by(Hour) %>%
  summarise(Avg_Dmd = mean(Total))

ggplot(Hypothesis_2, aes(x = Hour,
                         y = Avg_Dmd)) +
  geom_bar(stat='identity')



# NK edits: Question 2b

Working <- Hypothesis %>% 
  filter(Workingday == 1)

Nonwork <- Hypothesis %>% 
  filter(Workingday == 0)



# H0: avg demand during workdays == avg demand during nonworkdays (i.e. weekends & holidays)
# Ha: avg demand during workdays !=
t.test(Working$Total, 
       mu = mean(Nonwork$Total, na.rm = T), 
       alternative = "two.sided")
# reject null

t.test(Working$Total, 
       mu = mean(Nonwork$Total, na.rm = T), 
       alternative = "greater")
# reject null

# H0: avg registered demand during workdays == avg registered demand during nonworkdays (i.e. weekends & holidays)
# Ha: avg registered demand during workdays !=
t.test(Working$Registered, 
       mu = mean(Nonwork$Registered, na.rm = T), 
       alternative = "two.sided")
# reject null

# H0: avg casual demand during workdays == avg casual demand during nonworkdays (i.e. weekends & holidays)
# Ha: avg casual demand during workdays !=
t.test(Working$Casual, 
       mu = mean(Nonwork$Casual, na.rm = T), 
       alternative = "two.sided")
# reject null

# examining registered & casual for different TOD

# commute times for reg&cas on work vs nonwork
t.test(Working$Registered[Working$Commute == 1], 
       mu = mean(Nonwork$Registered[Nonwork$Commute == 1]), 
       alternative = "two.sided")
t.test(Working$Casual[Working$Commute == 1], 
       mu = mean(Nonwork$Casual[Nonwork$Commute == 1]), 
       alternative = "two.sided")

# office hours for reg&cas on work vs nonwork
t.test(Working$Registered[Working$TOD == "Office"], 
       mu = mean(Nonwork$Registered[Nonwork$TOD == "Office"]), 
       alternative = "two.sided")
t.test(Working$Casual[Working$TOD == "Office"], 
       mu = mean(Nonwork$Casual[Nonwork$TOD == "Office"]), 
       alternative = "two.sided")

# daylight hours for reg&cas on work vs nonwork
t.test(Working$Registered[Working$Window == "Day"], 
       mu = mean(Nonwork$Registered[Nonwork$Window == "Day"]), 
       alternative = "two.sided")
t.test(Working$Casual[Working$Window == "Day"], 
       mu = mean(Nonwork$Casual[Nonwork$Window == "Day"]), 
       alternative = "two.sided")

# noncommute hours for reg&cas on work vs nonwork
t.test(Working$Registered[Working$Commute == 0], 
       mu = mean(Nonwork$Registered[Nonwork$Commute == 0]), 
       alternative = "two.sided")
t.test(Working$Casual[Working$Commute == 0], 
       mu = mean(Nonwork$Casual[Nonwork$Commute == 0]), 
       alternative = "two.sided")

# trough hours for reg&cas on work vs nonwork
t.test(Working$Registered[Working$TOD == "Trough"], 
       mu = mean(Nonwork$Registered[Nonwork$TOD == "Trough"]), 
       alternative = "two.sided")
t.test(Working$Casual[Working$TOD == "Trough"], 
       mu = mean(Nonwork$Casual[Nonwork$TOD == "Trough"]), 
       alternative = "two.sided")

# nighttime hours for reg&cas on work vs nonwork
t.test(Working$Registered[Working$Window == "Night"], 
       mu = mean(Nonwork$Registered[Nonwork$Window == "Night"]), 
       alternative = "two.sided")
t.test(Working$Casual[Working$Window == "Night"], 
       mu = mean(Nonwork$Casual[Nonwork$Window == "Night"]), 
       alternative = "two.sided")


# THOUGHTS ####
# Demand differs significantly between workdays & nonworkdays
  # Registered demand is significantly more on workdays
  # Casual demand is significantly less on workdays
  # This trend holds except for looking at "noncommute" hours & nighttime hours:
    # Registered demand is significantly less during noncommuting hours on workdays
    # Registered demand is significantly less during nighttime on workdays

# Hierarchy of mean demand:
  # Commuting times > Office hours > Daytime hours > Non-commuting office hours > Avg of entire dataset > Trough window > Nighttime hours
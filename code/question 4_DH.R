# Question 4 
# What type of relationship do you see between weather and bike rental? 
# Is the relationship the same for registered vs. casual users?

# packages and data
library(tidyverse)
library(knitr)
library(dplyr)
library(ggplot2)
bikeData <- read.csv("Desktop/Georgetown/Stats for B.A./Capital Bike Sharing data by hour.csv") %>%
  as_tibble()

# preview data
head(bikeData)
summary(bikeData)

# explotatory analysis
# total bike rentals over time
ggplot(bikeData, aes(x = instant, y= registered)) +
  geom_line()
# we can see shape of distribution, bimodal (first is 1/1/11 is and last 12/31/12)

# contingency table for weathersit and registered, not working?
tab1 <- table(bikeData$weathersit, bikeData$registered)
rowSums(tab1)
colSums(tab1)
prop.table((tab1), margin = 2)*100
prop.table((tab1), margin = 1)*100
  
# Part 1: relationship between weather and bike rental
# using atemp & count
OLS.1 <- lm(bikeData$cnt ~ atemp, data = bikeData)
OLS.1

qplot(atemp , cnt, data = bikeData)

# box plots by month, double check  
monthNames <- c("Jan", "Feb", "Mar", "April", "May", "June","July", "Aug", "Sept", "Oct", "Nov", "Dec")
boxplot <- ggplot(bikeData, aes(x = mnth, y = cnt,
                                group = interaction(mnth))) +  geom_boxplot(color ="blue",outlier.colour="red", outlier.shape=8,
                                                                            outlier.size=4) + scale_x_discrete(labels=monthNames)

# Part 2: registered vs casual users
OLS.registered <-lm(bikeData$registered ~ atemp, data = bikeData)
OLS.registered # intercept 14.43 , atemp 292.90
# can be removed: qplot(atemp , registered, data = bikeData)

OLS.casual <- lm(bikeData$casual ~ atemp, data = bikeData)
OLS.casual # intercept -26.31 , atemp 130.28
qplot(atemp , casual, data = bikeData)

# change ponint shapes, colors by season
# need to make weathersit a level, can not be continuous 
#ggplot(bikeData, aes(x=atemp, y=registered, shape= weathersit, color= weathersit)) +
  #geom_point()

bikeData %>% group_by(weathersit)

ggplot( bikeData,aes( x= atemp, y= registered)) +
  geom_point(alpha = 0.6)

ggplot( bikeData,aes( x= atemp, y= casual)) +
  geom_point(alpha = 0.6)


# mean temp for registered
# mean temp for casual 
bikeData%>%
  group_by(cnt) %>%
  summarise(MeanTemp = mean(temp, n.rm = TRUE),
            obs = n())
           



BikeShare = read.csv("C:/Users/antho/Documents/OPIM_601-030_Stats_For_Business_Analytics/Team_Project_Bikesharing\\Capital Bike Sharing data by hour.csv")

### 2b)

# Ho: Registered users demand more bikes on weekdays than weekends or holidays
# Ha: Registeres users demand less bikes on weekdays than weekends or holidays

# Install necessary packages

library(tidyverse)
library(grid)
install.packages("gridExtra")
library(gridExtra)
library(lattice)
library(dplyr)
library(ggplot2)
# Create table that only includes Rides, Weekdays, Holidays of Registered Users

Registered_Users<- BikeShare %>%
  dplyr::select(registered, instant, weekday, holiday)

# Generate sample of Registered Users (May not be needed)

Registered_Users %>%
  sample_n(100)

# Change "0" in weekday to 7, for easier filtering
Registered_Users$weekday[Registered_Users$weekday == 0] <- 7

# Create table that includes observations for weekdays only
Registered_Users_Weekday <- BikeShare %>%
  dplyr::select(registered, instant, weekday, holiday) %>%
  filter(weekday > 0 & weekday <= 5) %>%
  filter(holiday == 0)

# Create sample - weekday
smple_RUWday <- Registered_Users_Weekday %>%
  sample_n(150)

# Summary statistics
summary(smple_RUWday$instant)

# Create table that includes observations for weekends only
Registered_Users_Weekend <- BikeShare %>%
  dplyr::select(registered, instant, weekday, holiday) %>%
  filter(weekday > 5 & weekday <= 7) %>%
  filter(holiday == 0)

# Create sample - weekend
smple_RUWend <- Registered_Users_Weekend %>%
  sample_n(150)

# Create table that includes observations for holidays only
Registered_Users_Holiday <- BikeShare %>%
  dplyr::select(registered, instant, weekday, holiday) %>%
  filter(holiday == 1)

# Create sample - Holiday
smple_RUH <- Registered_Users_Holiday %>%
  sample_n(150)

# Generate Comparative Plots to see distribution
p1 <- ggplot(smple_RUWday, aes(instant)) + 
  geom_histogram(fill = "white", color = "grey30")

p2 <- ggplot(smple_RUWend, aes(instant)) + 
  geom_histogram(fill = "blue", color = "grey30")

p3 <- ggplot(smple_RUH, aes(instant)) + 
  geom_histogram(fill = "red", color = "grey30")


grid.arrange(p1, p2, p3, ncol = 3)


#Test whether weekday demand is less than weekend demand
t.test(smple_RUWday$instant, smple_RUWend$instant, alternative="less")  

# Test whether weekday demand is less than holiday demand
t.test(smple_RUWday$instant, smple_RUH$instant, alternative = "less")


# CONCLUSIONS:

# For t.test 1, p-value = 0.1678 which is greater than alplha(0.05)
# Fail to Reject NULL

# For t.test 2 p-value =  0.2339 which is greater than alpha(0.05)
# Fail to Reject NULL

# Should this be re-done with log?



# DEATCH PACKAGES TO AVOID CONFLICT WITH OTHER CHUNKS
detach("package:tidyverse", unload = TRUE)
detach("package:grid", unload = TRUE)
detach("package:gridExtra", unload = TRUE)
detach("package:lattie", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
detach("package:dplyr", unload = TRUE)


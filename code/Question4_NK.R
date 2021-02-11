# Question 4 ####

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
workday_levels <- c(`0` = "No", `1` = "Yes")

total_riders <- bikes %>% summarise(totalRiders = sum(cnt)) %>% as.numeric()  # 3,292,679
total_cas <- bikes %>% summarise(totalCas = sum(casual)) %>% as.numeric()
total_reg <- bikes %>% summarise(totalReg = sum(registered)) %>% as.numeric()
prop_reg <- round(total_reg/total_riders, 4)


# Processed datasets ####
B <- bikes %>% group_by(weathersit) %>% 
  mutate(weathersit = recode_factor(weathersit, !!!weather_levels)) %>%
  summarise(riders = sum(cnt), 
            reg = sum(registered), 
            cas = sum(casual),
            prop_reg = sum(registered) / riders)

b_propreg <- bikes %>% group_by(weathersit) %>% 
  mutate(weathersit = recode_factor(weathersit, !!!weather_levels),
         prop_reg = registered / cnt)

b_pivotlong <- bikes %>% select(dteday, hr, weathersit, temp, workingday, casual:cnt) %>%
  pivot_longer(cols = c("casual", "registered"), names_to = "ridertype", values_to = "numRiders") %>%
  mutate(weathersit = recode_factor(weathersit, !!!weather_levels)) %>%
  group_by(weathersit)

b_pivotlong_normalised <- b_pivotlong %>% 
  group_by(ridertype) %>%
  mutate(numRiders_norm = numRiders / max(numRiders))

# to find max obs of ridertype to use in normalizing ibid
b_pivotlong %>% 
  group_by(ridertype) %>% 
  summarise(max = max(numRiders))


b_pivotlong.summary <- b_pivotlong %>% group_by(weathersit, ridertype) %>% 
  summarise(N = sum(numRiders)) %>% 
  mutate(prop_ridergivenweather = N/sum(N))

b_pivotlong.summary2 <- b_pivotlong %>% group_by(weathersit, workingday, ridertype) %>% 
  summarise(N = sum(numRiders)) %>% 
  mutate(prop_ridergivenweather = N/sum(N))




# QUESTION 4 ##########
# What type of relationship do you see between weather and bike rental?
# Is the relationship the same for registered vs. casual users?

# rel b/t categorical (ordinal) and quant (contin, sorta)

# num hrs w/ obs per weather type NOT NUM RIDERS
bikes %>% group_by(weathersit) %>% tally()

bikes %>% filter(weathersit == 4)

table(b_pivotlong$ridertype, b_pivotlong$weathersit)




# this is showing num Obs
# ggplot(b_pivotlong, aes(x = ridertype, y = ..count..)) + geom_bar() + 
#   facet_grid(~weathersit)

# THIS IS THE GOOD ONE! Still in progress but good start. ----
ggplot(b_pivotlong.summary, aes(weathersit, y = N, fill = ridertype)) + 
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "How Weather Affects Ridership", 
       x = "Type of Weather", y = "Proportion of Rider Type",
       fill = "Rider Type") + 
  geom_text(aes(label = round(prop_ridergivenweather, 3)), position = "fill", vjust = 1)

# OOH THIS ONE'S GOOD TOO! Sliced by workingday
ggplot(b_pivotlong.summary2, aes(recode_factor(workingday, !!!workday_levels), y = N, fill = ridertype)) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(. ~ weathersit) + 
  labs(title = "How Weather & Workday Affect Ridership",
       x = "Workday?", y = "Proportion of Rider Type",
       fill = "Rider Type") +
  geom_text(aes(label = round(prop_ridergivenweather, 3)), position = "fill", vjust = 1)




# overall ridership by weather
ggplot(B, aes(x = weathersit, y = riders)) + geom_col() + 
  geom_text(aes(label = riders), vjust = -0.5)

# ugh so unhelpful
# b_pivotlong %>% ggplot(aes(x = weathersit, fill = ridertype)) + geom_bar(position = "fill")

# ggplot(b_propreg, aes(x = weathersit, y = cnt)) + 
#   geom_violin(scale = "area")


# boxplot showing proportion of registered riders increases as weather worsens
ggplot(b_propreg, aes(x = weathersit, y = prop_reg)) + 
  geom_boxplot() + coord_flip()






# statistical testing? ####
# where 0 = casual, 1 = registered
mean_store <- numeric()
for(i in 1:1000){
  mean_store[i] <- mean(sample(c(0, 1), 
                               size = 100, 
                               replace = T, 
                               prob = c(1-prop_reg, prop_reg)))
}
hist(mean_store)


sum(b_pivotlong.summary[1:2,3])
prop_reg
prop.test(1875428, 2338173, p = 0.8117)  # Yeah, and? Now what?



# THOUGHTS ####
# Proportion of registered riders in total as a null hypothesis?
# Does proportion differ significantly when sliced by weather? Does this matter?
# Filter by weekday/weekend? What's the crosstab then?



# 2.11.21

ggplot(bikes, aes(x = cnt)) + 
  geom_freqpoly(mapping = aes(colour = factor(weathersit)))

ggplot(bikes, aes(x = cnt, y = factor(weathersit))) + 
  geom_boxplot()


# temp by rider, faceted by weathersit
ggplot(bikes, aes(temp, cnt)) + 
  geom_point() + 
  facet_wrap(~ weathersit)

# using alpha
ggplot(b_pivotlong_normalised, aes(temp, numRiders_norm)) + 
  geom_point(alpha = 0.1) + 
  facet_grid(ridertype ~ weathersit) + 
  xlim(0, 0.3)
# ibid but heatmap
ggplot(b_pivotlong_normalised, aes(temp, numRiders_norm)) + 
  geom_bin2d() + 
  facet_grid(ridertype ~ weathersit)


# Ok, so what am I seeing:
# more registered riders than casual in general
# as weather situation worsens, ridership decreases (reg & cas)
# as weather situation worsens, proportion of registered increases (casual riders prob don't want to ride in shitty weather)
# this trend holds true for workdays & non-workdays
  # though proportion of casual riders higher for non-workdays

# at lower temps, fewer casual riders across all weather categories




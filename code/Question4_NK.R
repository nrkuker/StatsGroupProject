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

B2 <- bikes %>% group_by(weathersit) %>% 
  mutate(weathersit = recode_factor(weathersit, !!!weather_levels),
         prop_reg = registered / cnt)

B.long <- bikes %>% select(dteday, hr, weathersit, casual:cnt) %>%
  pivot_longer(cols = c("casual", "registered"), names_to = "ridertype", values_to = "numRiders") %>%
  mutate(weathersit = recode_factor(weathersit, !!!weather_levels)) %>%
  group_by(weathersit)

B.long.summary <- B.long %>% group_by(weathersit, ridertype) %>% 
  summarise(N = sum(numRiders)) %>% 
  group_by(weathersit) %>% 
  mutate(prop_weather = N/sum(N))




# QUESTION 4 ##########
# What type of relationship do you see between weather and bike rental?
# Is the relationship the same for registered vs. casual users?

# rel b/t categorical (ordinal) and quant (contin, sorta)
# bar, box, dotplot, violin

# num hrs w/ obs per weather type NOT NUM RIDERS
bikes %>% group_by(weathersit) %>% tally()

bikes %>% filter(weathersit == 4)

table(B.long$ridertype, B.long$weathersit)



# bar of riders for each weather cat

# this is showing num Obs
ggplot(B.long, aes(x = ridertype, y = ..count..)) + geom_bar() + 
  facet_grid(~weathersit)

# THIS IS THE GOOD ONE! Still in progress but good start. ----
ggplot(B.long.summary, aes(weathersit, y = N, fill = ridertype)) + 
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "How Weather Affects Ridership", 
       x = "Type of Weather", y = "Proportion of Rider Type",
       fill = "Rider Type") + 
  geom_text(aes(label = round(prop_weather, 3)), position = "fill", vjust = 1)

ggplot(B, aes(x = weathersit, y = reg)) + geom_col()

# ugh so unhelpful
# B.long %>% ggplot(aes(x = weathersit, fill = ridertype)) + geom_bar(position = "fill")

ggplot(B2, aes(x = weathersit, y = prop_reg)) + 
  geom_boxplot() + coord_flip()

boxplot(prop_reg ~ weathersit, data=B2, col='lightblue')



ggplot(B2, aes(x = weathersit, y = cnt)) + 
  geom_violin(scale = "area")



# statistical testing?
# where 0 = casual, 1 = registered
mean_store <- numeric()
for(i in 1:1000){
  mean_store[i] <- mean(sample(c(0, 1), 
                               size = 100, 
                               replace = T, 
                               prob = c(1-prop_reg, prop_reg)))
}
hist(mean_store)


sum(B.long.summary[1:2,3])
prop_reg
prop.test(1875428, 2338173, p = 0.8117)  # Yeah, and? Now what?



# THOUGHTS ####
# Proportion of registered riders in total as a null hypothesis?
# Does proportion differ significantly when sliced by weather? Does this matter?
# Crosstab of weather by prop_reg?
# Filter by weekday/weekend? What's the crosstab then?




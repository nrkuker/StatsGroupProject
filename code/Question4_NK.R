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
weather_levels <- c("Clear", "Mist", "Light Precip.", "Heavy Precip.")
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
b_pivotlong.summary <- b_pivotlong.summary %>% group_by(ridertype) %>% mutate(prop_weathergivenrider = N/sum(N))
# Is there a cleaner way to do above with table()?

b_pivotlong %>% group_by(weathersit) %>% 
  summarise(N = sum(numRiders)) %>% 
  mutate(prop_weather = N/sum(N))


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






# Shows proportion of casual decreases with worsening weather ----
ggplot(b_pivotlong.summary, aes(weathersit, y = N, fill = ridertype)) + 
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "How Weather Affects Ridership", 
       x = "Type of Weather", y = "Proportion of Rider Type",
       fill = "Rider Type") + 
  geom_text(aes(label = round(prop_ridergivenweather, 3)), position = "fill", vjust = 1)

# breakdown of obs b/t weather cats similar for both rider types
  # does this contradict previous plot?
ggplot(b_pivotlong.summary, aes(ridertype, y = N, fill = weathersit)) + 
  geom_bar(stat = "identity", position = "fill") + 
  # coord_flip() + 
  labs(title = "How Weather Affects Ridership") + 
  geom_text(aes(label = round(prop_weathergivenrider, 3)), position = "fill", vjust = 1)




# This one's pretty interesting: Sliced by workingday
ggplot(b_pivotlong.summary2, aes(recode_factor(workingday, !!!workday_levels), y = N, fill = ridertype)) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(. ~ weathersit) + 
  labs(title = "How Weather & Workday Affect Ridership",
       x = "Workday?", y = "Proportion of Rider Type",
       fill = "Rider Type") +
  geom_text(aes(label = round(prop_ridergivenweather, 3)), position = "fill", vjust = 1)





# overall ridership by weather
ggplot(B, aes(x = weathersit, y = riders, fill = weathersit)) + geom_col() + 
  labs(title = "Total Riders by Weather Type", 
       x = "Weather Type", y = "Total Riders",
       fill = "Weather Type") +
  geom_text(aes(label = riders), vjust = -0.5)


# says same as above but in stacked bar chart
b_pivotlong %>% group_by(weathersit) %>% 
  summarise(N = sum(numRiders)) %>% 
  mutate(prop_weather = N/sum(N)) %>% 
  ggplot(aes(factor("Riders"), y = N, fill = weathersit)) + 
  geom_bar(stat = "identity", position = "fill") + 
  # coord_flip() + 
  labs(title = "Proportion of Weather Type Over All Observations", 
       x = NULL, y = "Proportion", 
       fill = "Weather Type") + 
  geom_text(aes(label = round(prop_weather, 3)), position = "fill", vjust = 1)

# format data for pie chart
(pie <- b_pivotlong %>% group_by(weathersit) %>% 
  summarise(N = sum(numRiders)) %>% 
  mutate(prop_weather = round((N/sum(N))*100, 1)
         ) %>% 
  arrange(desc(weathersit)) %>% 
  mutate(lab.ypos = (cumsum(prop_weather) - 0.5*prop_weather)))

# blank_theme <- theme_minimal()+
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.border = element_blank(),
#     panel.grid=element_blank(),
#     axis.ticks = element_blank(),
#     plot.title=element_text(size=14, face="bold")
#   )

# make the pie chart
ggplot(pie, aes(x = "", y = prop_weather, fill = weathersit)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop_weather), color = "white")+
  theme_void() + 
  labs(title = "Proportion of Weather Type Over All Observations",
       fill = "Weather Type")





# boxplot showing total riders decreases as weather worsens
ggplot(bikes, aes(x = cnt, y = factor(weathersit))) + 
  geom_boxplot()

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







# temp by rider, faceted by weathersit
ggplot(bikes, aes(temp, cnt)) + 
  geom_point(alpha = 0.1) + 
  facet_wrap(~ weathersit)

ggplot(b_pivotlong_normalised, aes(temp, numRiders_norm)) + 
  geom_point(alpha = 0.05) + 
  facet_grid(ridertype ~ weathersit)


# just looking at low temps
ggplot(b_pivotlong_normalised, aes(temp, numRiders_norm)) + 
  geom_point(alpha = 0.1) + 
  facet_grid(ridertype ~ weathersit) + 
  labs(title = "Riders at Low Temps") + 
  xlim(0, 0.3)
# ibid but heatmap
ggplot(b_pivotlong_normalised, aes(temp, numRiders_norm)) + 
  geom_bin2d() + 
  facet_grid(ridertype ~ weathersit)





# HAVEN'T GOTTEN THIS BIT TO WORK YET
# # workday slice from above but normalized?
# b_pivotlong_normalised %>% group_by(weathersit, workingday, ridertype) %>%
#   summarise(N = sum(numRiders))
# 
# ggplot(b_pivotlong.summary2, aes(recode_factor(workingday, !!!workday_levels), y = N, fill = ridertype)) +
#   geom_bar(stat = "identity", position = "fill") +
#   facet_grid(. ~ weathersit) +
#   labs(title = "How Weather & Workday Affect Ridership",
#        x = "Workday?", y = "Proportion of Rider Type",
#        fill = "Rider Type") +
#   geom_text(aes(label = round(prop_ridergivenweather, 3)), position = "fill", vjust = 1)







# Ok, so what am I seeing:
# more registered riders than casual in general
# as weather situation worsens, overall ridership decreases (reg & cas)
# as weather situation worsens, proportion of registered increases (casual riders prob don't want to ride in shitty weather)
# this trend holds true for workdays & non-workdays
  # though proportion of casual riders higher for non-workdays

# at lower temps, fewer casual riders across all weather categories
# tend to be more casual riders when warmer (t-norm roughly 0.4-0.8)
# registered riders more spread out, regardless of temperature
# above 2 hold true for all weather type




detach("package:tidyverse", unload = TRUE)
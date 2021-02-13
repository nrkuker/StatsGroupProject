# Capital Bikeshare data - EXPLORATORY ####

# Packages ####
library(tidyverse)
library(lubridate)
library(modeest)
library(moments)
library(car)  # for QQ plots. Not important



# Load Data ####
bikes <- read.csv("data/Capital Bike Sharing data by hour.csv") %>% 
  as_tibble()

# convert dteday into a date var
bikes <- bikes %>% mutate(
  date = ymd(dteday)) %>% 
  select(instant:dteday, date, season:cnt)


month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

season_levels <- c("Winter", "Spring", "Summer", "Fall")


# TO DO: ####
# compare riders on holidays vs not
# compare riders on weekends vs not (rider type)
# compare riders on working days vs not (though isn't that the same thing as weekend?)
# ? for Dr.D: what does the normalized weather & stuff mean? how to treat?


# Basic Descriptive Analysis ####
str(bikes)
head(bikes)

# check for NAs
for(v in 1:ncol(bikes)) {
  cat(colnames(bikes[,v]), sum(is.na(bikes[,v])), "\n")
}   # no nulls in data


min(bikes$dteday)  # earliest obs
max(bikes$dteday)  # latest obs
# range 1 Jan 2011 to 31 Dec 2012

# general desc. of categorical data
table(bikes$yr)   # slightly more hours with obs in 2012
table(bikes$mnth)   # Feb fewest obs, also shortest month
table(bikes$yr, bikes$mnth)   # improvement mostly in Jan-Mar
table(bikes$hr)   # fewest obs at 3am & 4am
table(bikes$holiday)
table(bikes$weekday)   # more obs on weekends, 0-6, week starts Sun
table(bikes$workingday)
table(bikes$weathersit)   # obs get fewer as weather gets worse
    # only 3 obs in category 4 weather
    bikes %>% filter(weathersit == 4)


# general desc. of numerical data
  # don't fully understand the normalized weather info
summary(bikes$temp)
summary(bikes$atemp)
summary(bikes$hum)
summary(bikes$windspeed)
summary(bikes$casual)
summary(bikes$registered)

quantile(bikes$casual)
quantile(bikes$registered)

# template for QQ plots, but have NO IDEA how to interpret these
with(bikes, {
  qqnorm(temp);
  qqline(temp)})

with(bikes, qqPlot(temp))




bikes %>% summarise(
  mean(casual), median(casual),
  mean(registered), median(registered),
  mean(cnt))
# mean > median, indicates long tail at high values

# get all means
bikes %>% 
  select(season:cnt) %>%
  colMeans(na.rm = T)
bikes %>% 
  select(season:cnt) %>%
  apply(., 2, mean, na.rm = T)
# colMeans(bikes[sapply(bikes, is.numeric)], na.rm = T)
apply(ames, 2, mean, na.rm = T)

# get all modes
bikes %>% 
  select(season:cnt) %>%
  apply(., 2, mfv, na_rm = T)
# most common:
    # season = 3
    # yr = 1
    # month = May, Jul
    # hr = 16, 17
    # weekday = 6

# an extremely inelegant way of showing when seasons start
# ignore season 1 end_date
bikes %>% filter(yr == 0) %>%
  group_by(season) %>% 
  summarise(start_date = min(date),
            end_date = max(date) )

# returns crosstab of weekday (1-5 = M-F) and workingday (0 F, 1 T)
bikes %>%
  group_by(weekday, workingday) %>% tally() %>%
  pivot_wider(names_from = workingday, values_from = n)
# returns crosstab of weekday (1-5 = M-F) and holiday (0 F, 1 T)
bikes %>%
  group_by(weekday, holiday) %>% tally() %>%
  pivot_wider(names_from = holiday, values_from = n)
# the non-working day weekdays correspond to holidays
# ibid but easier:
table(bikes$weekday, bikes$workingday)
table(bikes$weekday, bikes$holiday)


bikes %>% group_by(workingday) %>%
  summarise(avg_regis = mean(registered, na.rm = T),
            avg_cas = mean(casual, na.rm = T))

# scatter of registered vs casual, color denotes workingday
bikes %>% #filter(workingday == 1) %>%
  ggplot(aes(x = registered, y = casual)) + 
  geom_point(aes(colour = factor(workingday)))
# when filter working==F, some points that follow working==T line, investigate?

# crosstab of obs per month, per yr
bikes %>% 
  group_by(yr, mnth) %>% 
  mutate(yr = recode(yr, `0` = 2011, `1` = 2012)) %>%
  tally() %>% 
  pivot_wider(names_from = yr, values_from = n)
table(bikes$yr, bikes$mnth) # same thing but quicker


# Initial Graphical Analysis ####

# Continuous - Scatterplots ####
# scatter of temp vs atemp
bikes %>% 
  mutate(mnth = recode_factor(mnth, !!!month_levels)) %>%
  ggplot(aes(x = temp, y = atemp)) + 
  geom_point(aes(colour = mnth))
# all obs 2012-08-17 w/ either really low feeling temp or data entry error
bikes %>% filter(temp > 0.5, atemp < 0.4) %>% view()
bikes %>% filter(date == "2012-08-17") %>% view()

# scatter of temp vs hum
bikes %>% 
  mutate(mnth = recode_factor(mnth, !!!month_levels)) %>%
  ggplot(aes(x = temp, y = hum)) + 
  geom_point(aes(colour = mnth))
# all obs on 2011-03-10 had hum == 0
bikes %>% filter(hum < 0.05) %>% view()
bikes %>% filter(date == "2011-03-10") %>% view()

# scatter of temp vs windspeed
bikes %>% 
  mutate(mnth = recode_factor(mnth, !!!month_levels)) %>%
  ggplot(aes(x = temp, y = windspeed)) + 
  geom_point(aes(colour = mnth))
# are days with 0 windspeed considered interesting? n = 2180

# scatter of temp vs casual
bikes %>%  #filter(season == 1) %>% 
  mutate(season = recode_factor(season, !!!season_levels)) %>%
  ggplot(aes(x = temp, y = casual)) + 
  geom_point(aes(colour = season))

# scatter of temp vs registered
bikes %>% #filter(season == 1) %>% 
  mutate(season = recode_factor(season, !!!season_levels)) %>%
  ggplot(aes(x = temp, y = registered)) + 
  geom_point(aes(colour = season))

# scatter of temp vs cnt
ols.temp <- lm(cnt ~ temp, data = bikes)
bikes %>% #filter(season == 1) %>% 
  mutate(season = recode_factor(season, !!!season_levels)) %>%
  ggplot(aes(x = temp, y = cnt)) + 
  geom_point(aes(colour = season), alpha = 0.25, position = "jitter") + 
  geom_abline(slope = summary(ols.temp)[["coefficients"]][2], 
              intercept = summary(ols.temp)[["coefficients"]][1])

# scatter of atemp vs hum
bikes %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         gap = atemp >= 0.57,
         group1 = (date == "2012-08-17"),
         group2 = (date == "2011-03-10")) %>%
  ggplot(aes(x = atemp, y = hum)) + 
  geom_point(aes(colour = group2))
# there's a weird gap at 0/57/0.58, what's that about?
# majority of summer obs are > 0.58

# scatter of atemp vs windspeed
bikes %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         gap = atemp >= 0.375) %>%
  ggplot(aes(x = atemp, y = windspeed)) + 
  geom_point(aes(colour = gap))
# are days with 0 windspeed considered interesting? n = 2180
# again with the gap at 0.375

# scatter of atemp vs casual
ols.atemp <- lm(casual ~ atemp, data = bikes)
summary(ols.atemp)
bikes %>% #filter(season == 1) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2012-08-17")) %>%
  ggplot(aes(x = atemp, y = casual)) + 
  geom_point(aes(colour = season), alpha = 0.25, position = "jitter") + 
  geom_abline(slope = summary(ols.atemp)[["coefficients"]][2], 
              intercept = summary(ols.atemp)[["coefficients"]][1])

# scatter of atemp vs registered
bikes %>% #filter(season == 1) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2012-08-17")) %>%
  ggplot(aes(x = atemp, y = registered)) + 
  geom_point(aes(colour = season))

# scatter of atemp vs cnt
ols.atemp <- lm(cnt ~ atemp, data = bikes)
bikes %>% #filter(season == 1) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2012-08-17")) %>%
  ggplot(aes(x = atemp, y = cnt)) + 
  geom_point(aes(colour = season), alpha = 0.25, position = "jitter") + 
  geom_abline(slope = summary(ols.atemp)[["coefficients"]][2], 
              intercept = summary(ols.atemp)[["coefficients"]][1])

# scatter of hum vs windspeed
bikes %>% #filter(season == 2) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2011-03-10")) %>%
  ggplot(aes(x = hum, y = windspeed)) + 
  geom_point(aes(colour = group))

# scatter of hum vs casual
bikes %>% #filter(season == 2) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2011-03-10")) %>%
  ggplot(aes(x = hum, y = casual)) + 
  geom_point(aes(colour = season))

# scatter of hum vs registered
bikes %>% #filter(season == 2) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2011-03-10")) %>%
  ggplot(aes(x = hum, y = registered)) + 
  geom_point(aes(colour = season))

# scatter of windspeed vs casual
bikes %>% #filter(windspeed > 0) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2011-03-10")) %>%
  ggplot(aes(x = windspeed, y = casual)) + 
  geom_point(aes(colour = season), alpha = 0.2, position = "jitter")
# to my eye: as wind incr, num casual decr. idk if lm holds that up

# scatter of windspeed vs registered
bikes %>% #filter(season == 2) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         group = (date == "2011-03-10")) %>%
  ggplot(aes(x = windspeed, y = registered)) + 
  geom_point(aes(colour = season), alpha = 0.2, position = "jitter")

# scatter of casual vs registered
bikes %>% filter(holiday == 0) %>% 
  mutate(season = recode_factor(season, !!!season_levels),
         workday = recode(workingday, `0` = "not work day", `1` = "work day"),
         holiday = recode(holiday, `0` = "not holiday", `1` = "holiday"),
         group = (hr %in% c(6:9))) %>%
  ggplot(aes(x = casual, y = registered)) + 
  geom_point(aes(colour = group), alpha = 0.2, position = "jitter")
# 2 very different trends happening here, prob explained by workday or not



# Continuous - Histograms ####
bikes %>% 
  ggplot(aes(temp)) + 
  geom_histogram(fill = "steelblue", col = "navy", alpha = 0.8)
bikes %>% summarise(skewness(temp), kurtosis(temp))

bikes %>%
  ggplot(aes(atemp)) + 
  geom_histogram(fill = "steelblue", col = "navy", alpha = 0.8)
# modal class at ~0.625
bikes %>% summarise(skewness(atemp), kurtosis(atemp))

bikes %>%
  ggplot(aes(hum)) + 
  geom_histogram(fill = "steelblue", col = "navy", alpha = 0.8)
# hum left skewed
bikes %>% summarise(skewness(hum), kurtosis(hum))

bikes %>%
  ggplot(aes(windspeed)) + 
  geom_histogram(fill = "steelblue", col = "navy", alpha = 0.8)
# moderately right skewed
bikes %>% summarise(skewness(windspeed), kurtosis(windspeed))

bikes %>%
  ggplot(aes(casual)) + 
  geom_histogram(fill = "steelblue", col = "navy", alpha = 0.8)
# big big right skew, very leptokurtic
bikes %>% summarise(skewness(casual), kurtosis(casual))

bikes %>%
  ggplot(aes(registered)) + 
  geom_histogram(fill = "steelblue", col = "navy", alpha = 0.8)
# big right skew, fairly leptokurtic
bikes %>% summarise(skewness(registered), kurtosis(registered))

bikes %>%
  ggplot(aes(cnt)) + 
  geom_histogram(fill = "steelblue", col = "navy", alpha = 0.8)
bikes %>% summarise(skewness(cnt), kurtosis(cnt))



# Categorical - Bar ####
bikes %>% 
  mutate(yr = recode(yr, `0` = 2011, `1` = 2012)) %>%
  ggplot(aes(yr)) + geom_bar()

bikes %>% 
  mutate(mnth = recode_factor(mnth, !!!month_levels)) %>%
  ggplot(aes(mnth)) + geom_bar()

bikes %>% ggplot(aes(hr)) + geom_bar()

bikes %>% ggplot(aes(season)) + geom_bar()

bikes %>% ggplot(aes(weekday)) + geom_bar()

bikes %>% ggplot(aes(workingday)) + geom_bar()

bikes %>% ggplot(aes(holiday)) + geom_bar()

bikes %>% ggplot(aes(weathersit)) + geom_bar()

bikes %>% 
  group_by(hr) %>%
  summarise(mean_registered = mean(registered),
            time_of_day = as.factor(hr)) %>%
  ggplot(aes(x = hr, y = mean_registered)) + geom_bar(stat = "identity")

# was trying to get a way to color by proportion casual/registered
  





# OLD SHIT ####

# daily registered users over time
bikes %>% #filter(workingday == 0) %>%
  select(dteday, casual, registered) %>% 
  group_by(dteday) %>% summarise( 
    daily_cas = sum(casual),
    daily_reg = sum(registered)) %>%
  ggplot(aes(x = dteday, y = daily_reg)) + 
  geom_bar(stat = "identity")

bikes %>% group_by(season) %>%
  summarise(
    count = n()
  )
# obs by season 4200s win/spr; 4400s sum/fall

bikes %>% group_by(yr, mnth) %>%  # dist of obs over months
  summarise(
    count = n()
  ) %>% view()

bikes %>% group_by(hr) %>%  # num of obs by hr of day
  summarise(
    count = n()
  ) %>% view()
# not all hours have obs-- e.g. 3 & 4 have count = 697

bikes %>% group_by(hr) %>%  # num of obs & type of rider, by hr of day
  summarise(
    count = n(),
    num_reg = sum(registered),
    num_cas = sum(casual)
  ) %>% view()

bikes %>% filter(  # checking for any vars with NAs
  !(is.na(cnt))       # I think no nulls
) %>% print()



# adjust above to make a datetime
# THIS DOESN'T WORK, something about time zones? POSIXlt
bikes2 <- bikes %>% mutate(
  datetime = make_datetime(
    year = year(date),
    month = month(date),
    day = day(date),
    hour = hr
  )
)

# scatter of daily bike demand
  # each day has points for each hour interval of observation, that's not made clear
bikes2 %>%
  filter(date >= "2011-12-16", date <= "2012-01-15") %>%
  ggplot(aes(x = date, y = cnt)) + 
  geom_point()
# generally increasing over 2011-2012
# seems like a noticeable dip around the holidays

# gets the month datepart from date
by_month <- bikes2 %>% mutate(
  month = month(date, label = T)) %>%   # set label=F to get mon num instead of name
  group_by(month) %>%  # finds counts of riders based on month
  summarise(
    count = n(),
    num_reg = sum(registered),
    num_cas = sum(casual)
  ) %>% print()

by_month %>%
  summarise(
    record_count = n(),
    total_reg = sum(num_reg),  # 2,672,662
    total_cas = sum(num_cas)) %>%  # 620,017
  print()


# find grand total sum of reg & cas
bikes2 %>% summarise(
  record_count = n(),
  total_reg = sum(registered),  # 2,672,662
  total_cas = sum(casual)) %>%  # 620,017
  print()

# group by yr & season, get count of riders
bikes2 %>% group_by(yr,season) %>%
  summarise(
    num_riders = sum(cnt)
  ) %>% print()
  # ggplot(aes(x = season, y = num_riders)) + 
  # geom_bar(stat = "identity")
# this shows seasons grouped together over the years
  # aka season as discrete date part
# how to make them continuous?
# though fall has greatest ridership




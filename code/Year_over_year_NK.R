# Year Over Year Growth ####

# packages
library(tidyverse)
library(lubridate)
library(ggthemes)

# load data
bikes <- read.csv("data/Capital Bike Sharing data by hour.csv")

# global variables
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
year_levels <- c(2011, 2012)
season_levels <- c("Winter", "Spring", "Summer", "Fall")
weather_levels <- c("Clear", "Mist", "Light Precip.", "Heavy Precip.")



# CHECKING MY WORK ####
# daily demand by season
bikes %>% 
  group_by(yr, season) %>% 
  summarise(days = length(unique(dteday)),
            season_demand = sum(cnt),
            daily_demand = season_demand/days) %>% 
  mutate(Year = recode_factor(yr, `0` = "2011", `1` = "2012"),
         Season = recode_factor(season, levels = !!!season_levels))

# hourly demand by month
bikes %>% 
  group_by(yr, mnth) %>% 
  summarise(days = length(unique(dteday)),
            hrs = n(),
            month_demand = sum(cnt),
            daily_demand = month_demand/days, 
            hourly_demand = month_demand/hrs)





# YEAR OVER YEAR GROWTH CALCULATIONS ####
# dataframe with YOY percent growth
options(pillar.sigfig = 5)
df <- bikes %>%
  group_by(yr, season) %>%
  summarise(days = length(unique(dteday)),
            Season_Demand = sum(cnt),
            Daily_Demand = Season_Demand/days,
            Hourly_Demand = Season_Demand/n(),
            .groups = "drop") %>%
  mutate(Year = recode_factor(yr, `0` = "2011", `1` = "2012"),
         Season = recode_factor(season, levels = !!!season_levels)) %>% 
  arrange(Year, Season) %>% 
  select(Year, Season, Season_Demand, Daily_Demand, Hourly_Demand) %>% 
  mutate(YOY_Change = Season_Demand - Season_Demand[Year == 2011],
         # YOY_pct = format(round(YOY_Change/Season_Demand[Year == 2011]*100, 2), nsmall = 2),
         YOY_pct = round(YOY_Change/Season_Demand[Year == 2011]*100, 2))
df

# calculating overall YOY % growth
df %>% 
  group_by(Year) %>% 
  summarise(Yearly_Demand = sum(Season_Demand)) %>% 
  mutate(YOY_Change = Yearly_Demand - Yearly_Demand[Year == 2011],
         YOY_pct = round(YOY_Change/Yearly_Demand[Year == 2011]*100, 2))

# calculating YOY % growth for REGISTERED & CASUAL
yoy_reg_cas <- 
  bikes %>%
  group_by(yr, season) %>%
  summarise(days = length(unique(dteday)),
            Season_Demand_reg = sum(registered),
            Season_Demand_cas = sum(casual),
            .groups = "drop") %>%
  mutate(Year = recode_factor(yr, `0` = "2011", `1` = "2012"),
         Season = recode_factor(season, levels = !!!season_levels)) %>% 
  arrange(Year, Season) %>% 
  select(Year, Season, Season_Demand_reg, Season_Demand_cas) %>% 
  mutate(YOY_Change_reg = Season_Demand_reg - Season_Demand_reg[Year == 2011],
         YOY_Change_cas = Season_Demand_cas - Season_Demand_cas[Year == 2011],
         YOY_pct_reg = round(YOY_Change_reg/Season_Demand_reg[Year == 2011]*100, 2),
         YOY_pct_cas = round(YOY_Change_cas/Season_Demand_cas[Year == 2011]*100, 2))
  
# calculating overall YOY % growth for REG & CAS
yoy_reg_cas %>% 
  group_by(Year) %>% 
  summarise(Yearly_Demand_reg = sum(Season_Demand_reg),
            Yearly_Demand_cas = sum(Season_Demand_cas)) %>% 
  mutate(YOY_Change_reg = Yearly_Demand_reg - Yearly_Demand_reg[Year == 2011],
         YOY_Change_cas = Yearly_Demand_cas - Yearly_Demand_cas[Year == 2011],
         YOY_pct_reg = round(YOY_Change_reg/Yearly_Demand_reg[Year == 2011]*100, 2),
         YOY_pct_cas = round(YOY_Change_cas/Yearly_Demand_cas[Year == 2011]*100, 2))







# VISUALIZATIONS ####

# graphic for overall YOY % growth
df[df$Year == 2012,] %>% 
  ggplot(aes(Season, YOY_pct, fill = Season)) + 
  geom_col() + 
  coord_cartesian(xlim = c(0.4, 4.6), ylim = c(0, 120), expand = F) + 
  scale_fill_viridis_d(option = "D", end = 0.75) + 
  scale_y_continuous(breaks = seq(0, 120, 20), 
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%", "120%")) + 
  labs(title = "Year-Over-Year Percent Growth", 
       subtitle = "Growth in overall ridership by season, from 2011 to 2012",
       x = "Season", y = "Percent Growth",
       fill = "Season") +  theme_igray() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
        # axis.title = element_text(size = 11),
        # axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key = element_blank())

# graphic for YOY % by customer segment
yoy_reg_cas %>% 
  filter(Year == 2012) %>% 
  select(Year, Season, Registered = YOY_pct_reg, Casual = YOY_pct_cas) %>% 
  pivot_longer(cols = c(Registered, Casual)) %>% 
  ggplot(aes(Season, value, fill = Season)) +
  geom_col() + 
  facet_wrap(~name) +
  coord_cartesian(xlim = c(0.4, 4.6), ylim = c(0, 130), expand = F) + 
  scale_fill_viridis_d(option = "D", end = 0.75) + 
  scale_x_discrete(labels = c("Win", "Spr", "Sum", "Fall")) +
  scale_y_continuous(breaks = seq(0, 120, 20), 
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%", "120%")) + 
  labs(title = "Year-Over-Year Percent Growth", 
       subtitle = "Growth in customer segment by season, from 2011 to 2012",
       x = "Season", y = "Percent Growth",
       fill = "Season") + 
  theme_igray() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
        # axis.title = element_text(size = 11),
        # axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key = element_blank())





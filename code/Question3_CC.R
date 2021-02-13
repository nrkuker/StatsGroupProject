### Question #3
### output: html_document
---

# Set up the environment.
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r tidy = FALSE}
# Packages ####
library(tidyverse)
library(knitr)

# As needed, set path to folder where data is located.
opts_knit$set(root.dir = "C:/Users/carte/Desktop/Bike Project")
```
B <- bikes %>% 
  group_by(season) %>%                                                # Group by "season"
  mutate(season = recode_factor(season, !!!season_levels)) %>%        # Add "season_levels" column to show Winter:Fall
  summarise(riders = sum(cnt),                                        # Collapse many values down to a single summary for:
            reg = sum(registered),                                    # Total registered riders by season
            cas = sum(casual),                                        # Total casual riders by season
            prop_reg = sum(registered) / riders)                      # Proportion of registered riders (out of total) by season
            arrange(B, riders)
head(B)
                        

B2 <- bikes %>% group_by(season) %>%                                  # Un-collapsed data frame 
  mutate(season = recode_factor(season, !!!season_levels),
         prop_reg = registered / cnt)


# Create a data frame, group by season, differentiate the ridertypes in their total counts.
B.long <- bikes %>% select(dteday, season, casual:cnt) %>%
  pivot_longer(cols = c("casual", "registered"), names_to = "ridertype", values_to = "numRiders") %>%
  mutate(season = recode_factor(season, !!!season_levels)) %>%
  group_by(season)

head(B.long)

# Collapse values down to each grouped category
B.long.summary <- B.long %>% group_by(season, ridertype) %>% 
  summarise(N = sum(numRiders)) %>% 
  group_by(season) %>% 
  mutate(prop_season = N/sum(N))

B.long.summary
```

Create visualizations to solve 
Question #3 Is there any relationship between season and bike rental? Create a visualization displaying the relationship?

```{r tidy = FALSE}
table(B.long$ridertype, B.long$season)

# Total count of riders by season
ggplot(B, aes(x = season, y = riders)) + geom_col()

# Each type of riders' count by season
ggplot(B.long, aes(x = ridertype, y = ..count..)) + geom_bar() + 
  facet_grid(~season)

# Proportion of each ridertype
ggplot(B.long.summary, aes(season, y = N, fill = ridertype)) + 
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "RiderType Distrubution by Season", 
       x = "Different Seasons", y = "Proportion of Rider Type",
       fill = "Rider Type") + 
  geom_text(aes(label = round(prop_season, 3)), position = "fill", vjust = 1)


# Boxplot, registered riders by season
ggplot(B2, aes(x = season, y = prop_reg)) + 
                                            geom_boxplot() + coord_flip()

# Boxplot, registered riders proportion by season
boxplot(prop_reg ~ season, data=B2, col='lightblue')


# violin plot, total count of riders by season
ggplot(B2, aes(x = season, y = cnt)) + 
                                          geom_violin(scale = "area")



# statistical testing - Conclusion: Normal Distribute
# where 0 = casual, 1 = registered
mean_store <- numeric()
for(i in 1:1000){
                 mean_store[i] <- 
                                  mean(sample(c(0, 1), 
                                  size = 100, 
                                  replace = T, 
                                  prob = c(prop_cas, prop_reg)))
                 }

hist(mean_store)

setwd('~/Documents/math-camp/coding-camp')
library(tidyverse)
library(readxl)
library(haven)

# load world wealth inequality data
wid_data_raw <- read_xlsx('./problem_set/data/world_wealth_inequality.xlsx', 
                          col_names = c('country','indicator','percentile', 'year', 'value')) %>%
  separate(indicator, sep = "[\\r]?\\n", into = c('row_tag', 'type', 'notes'))

# load midwest data
mw <- read_dta('./problem_set/data/midwest.dta')

# manipulate wid data

#1 
wid_data <- wid_data_raw %>%
  select(country, type,percentile, year, value, notes)

#2 - filter to French net personal wealth
french_data <- wid_data %>%
  filter(type == 'Net personal wealth',
         country == 'France')

# plot data
french_data %>%
  ggplot(aes(y = value, x = year, color = percentile)) +
  geom_line()

# examine gaps in data
french_data %>%
  filter(year >=1960 & year <=1970)
  # some years have NA's

# add percent of national wealth
french_data <- french_data %>%
  mutate(perc_national_wealth = value *100)

# new plot with adjusted percent
french_data %>%
  ggplot(aes(y = perc_national_wealth, x = year, color = percentile)) +
  geom_line()

# filter to Russian data
russia_data <- wid_data %>%
  filter(type == 'Net personal wealth',
         country == 'Russian Federation') %>%
  mutate(perc_national_wealth = value *100) %>%
  filter(!is.na(perc_national_wealth))

russia_data %>%
  ggplot(aes(y = perc_national_wealth, x = year, color = percentile)) +
  geom_line() +
  labs(title = "Russian Wealth Inequality 1995-2015", 
       x = 'Year',
       y = 'Percent of National Wealth') 

# bottom 50 - least
russia_data %>%
  filter(percentile == 'p0p50') %>%
  arrange(perc_national_wealth)


# mean of top 10% 1995-2010 French + Russian
meanWealth <- function(x, year1, year2) {
  x %>% filter(year >= year1 & year <= year2) %>%
    summarize(top10_mean = mean(perc_national_wealth))
} 

meanWealth(french_data, 1995, 2010)  

meanWealth(russia_data, 1995, 2010)


# Midwest data section ----------------------------------------------------
glimpse(midwest)

# get population variables
pop_variables <- grep('pop', names(midwest))

# select geography + population variables
midwest_pop <- midwest %>%
  select(county, state, pop_variables)

midwest_pop <- midwest_pop %>%
  mutate(area = poptotal/popdensity)

midwest_pop %>% arrange(desc(area))
  #largest county is marquette

midwest_pop %>% 
  filter(state == 'IL') %>%
  arrange(desc(area))
  # largest IL county is MCLEAN

# calculate percent adults
midwest_pop <- midwest_pop %>%
  mutate(perc_popadults = popadults/poptotal*100) 

midwest_pop %>% arrange(desc(perc_popadults))
  #Keweenaw has the highest proportion of adults

midwest_pop %>% arrange(perc_popadults)
  # Isabella has lowest proportion of adults

# how many people in Michigan
midwest_pop %>%
  filter(state == 'MI') %>%
  summarize(mi_pop = sum(poptotal))
  # 9,295,297 people live in Michigan
library(tidyverse)
library(haven)
library(readxl)


# Class 1 - Video 1 -------------------------------------------------------


texas_data <- read_csv('~/Documents/math-camp/coding-camp/class1/texas_housing_data.csv')

drug_war_data <- read_dta('~/Documents/math-camp/coding-camp/class1/Dataset_HighProfileCriminalViolence.dta')

fed_data <- read_xlsx('./Documents/math-camp/coding-camp/class1/SCE-Public-LM-Quarterly-Microdata.xlsx', 
                      sheet = 'Data 2013', 
                      skip = 1)
head(fed_data)
glimpse(fed_data)
names(fed_data)


# Class 1 - Video 2 -------------------------------------------------------

select(texas_data, city, year, month)

# view data by ascending volume
arrange(texas_data, volume)
arrange(texas_data, desc(volume))
arrange(texas_data, desc(year),desc(volume))

texas_data %>%
  select(city, year, month, median) %>%
  arrange(desc(median))

texas_data %>%
  select(city, year, month, sales, volume) %>%
  mutate(mean_price = volume/sales,
         log_volume = log(volume),
         sqrt_mean_price = sqrt(mean_price),
         log_mean_price = log(mean_price)) %>%
  filter(year == 2013, city %in% c('Houston', 'Austin', 'Dallas')) %>%
  arrange(month) %>%
  summarise(total_volume = sum(volume),
            n_months = n(), 
            sd_volume = sd(volume))

library(tidyverse)
library(readxl)
setwd('~/Documents/math-camp')
incarceration_data <- read_xlsx("incarceration_counts_and_rates_by_type_over_time.xlsx",
                                range = "A7:CO10") %>%
  rename("type" = ...1) %>%
  pivot_longer(`1925`:`2016`, names_to = "year", values_to = "counts")


# Task 2 ------------------------------------------------------------------
# clean year
incarceration_data$year <- as.numeric(incarceration_data$year)

# replicate plot 
incarceration_data %>%
  ggplot(aes(x = year, y = counts, color = type)) +
    geom_line() +
    labs(title = "Incarceration counts (total population on a single day) over time")


# Task 3 ------------------------------------------------------------------

# filter to state, calculate decade
state_data <- incarceration_data %>% 
  mutate(decade = as.numeric(paste0(substr(year,1,3),0))) %>%
  filter(type == 'State prisons') %>%
  select(type, counts, decade, year)


# Task 4 ------------------------------------------------------------------

# what decade had highest growth?
state_data %>%
  group_by(decade) %>%
  summarize(first = first(counts),
            last = last(counts),
            percentage_growth = (last(counts) - first(counts))/first(counts)*100
            ) %>%
  arrange(desc(percentage_growth))
  #1980 saw most growth, 115%


# Task 5 ------------------------------------------------------------------
# subtask 1
numbers <- rep(seq(-9,10,1),10)
mean(numbers)
  #mean is 0.5
sum(numbers)
  # sum of vector is 100

#subtask 2
toy_data <- c(1,2,3,NA,4,5)

median(toy_data, na.rm = T)

#subtask 3
toy_data[5]

# subtask 4
left <- 'Harris'
right <- 'School of Public Policy'

theWhat <- function(left, right) {
  paste(left,right)
}

theWhat(left,right)

# Task 6 ------------------------------------------------------------------
#subtask 1
numbers <- rep(seq(-9,10,1),10)

numbers_squared <- NULL

for(i in 1:length(numbers)) {
   numbers_squared[i] <- numbers[i]^2
}

# subtask 2
set.seed(303)
noisy_numbers_squared <- NULL

for(i in 1:length(numbers)) {
  noisy_numbers_squared[i] <- numbers[i]^2 + rnorm(1, sd = 5)
}

numbers_data <-tibble(numbers = numbers,
                      noisy_numbers_squared = noisy_numbers_squared)
numbers_data %>%
  ggplot(aes(x = numbers, y = noisy_numbers_squared))+
    geom_point() + 
    geom_smooth()

# subtask 3 
bossify <- function(name) {
  paste(name,'is a boss')
}

bossify('Admiral Grace Hopper')

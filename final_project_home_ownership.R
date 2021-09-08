library(tidyverse)
library(readxl)
# https://www.census.gov/housing/hvs/data/rates.html
state_rate_raw <- read_xlsx('~/Documents/math-camp/coding-camp/tab3_state05_2021_hmr.xlsx',
                            col_names = F,  
                            skip =3)
# filter out NA's
state_rate_raw <- state_rate_raw %>% 
  filter(!is.na(...1))

# get index of rows with header and filter out
table3_index <- grep('Table 3', state_rate_raw$...1)
state_rate_raw <- state_rate_raw[-table3_index,]

#get index of rows with column names
row_index <- grep('State', state_rate_raw$...1)

# loop through each year and select only rows of data
state_rate_list <- list()
z <- 1
year_index <- 2021:2005
for(i in row_index) {
  df <- state_rate_raw[(i+1):(i+51),]
  df$year <- year_index[z]
  state_rate_list[[z]] <- df
  z <- z+1
}

state_rate <- do.call(rbind, state_rate_list)

# rename columns 
colnames(state_rate) <- c('state', 'q1_rate','q1_me', 'q2_rate','q2_me','q3_rate','q3_me','q4_rate','q4_me', 'year')

# remove periods
state_rate$state <- gsub("[.]",'', state_rate$state)

# convert from wide to long and clean
state_rate_long <- state_rate %>%
  select(-q1_me,-q2_me,-q3_me,-q4_me) %>%
  pivot_longer(cols = c(q1_rate,q2_rate,q3_rate,q4_rate))

# convert year-quarter to time filed

# calculate change over time by state

# plot all states over time with color of line as total change
                            
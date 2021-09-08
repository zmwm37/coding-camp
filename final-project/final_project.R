setwd('~/Documents/coding-camp/final-project/')


# READ LIBRARIES & DATA ---------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
library(viridis)
library(emo)
library(corrplot)

# source: https://data.cityofchicago.org/Parks-Recreation/Beach-Lab-Data/2ivx-z93u
beach_lab_data <- read_delim('Beach_Lab_Data.csv', delim = ';')

# EXPLORE & CLEAN DATA ------------------------------------------------------
summary(beach_lab_data)
View(head(beach_lab_data,25))

# clean names
clean_names <- gsub(' ', '_', str_to_lower(names(beach_lab_data)))
colnames(beach_lab_data) <- clean_names

# clean data
beach_lab_data$dna_sample_date<- as.Date(substr(beach_lab_data$dna_sample_timestamp,1,10), format ="%m/%d/%Y" )
beach_lab_data$dna_reading_mean <- ifelse(is_null())

# TO DO : CLEAN '20' YEARS TO 2020
beach_lab_data$dna_sample_date <- iselse(year(beach_lab_data))

sapply(beach_lab_data, function(x) length(unique(x)))


# filter to DNA test only
dna_data <- beach_lab_data %>% 
  filter(!is.na(`dna_test_id`)) %>%
  select(dna_test_id, dna_sample_timestamp,dna_sample_date, beach,dna_sample_1_reading,
         dna_sample_2_reading, dna_reading_mean, latitude, longitude, location) %>%
  distinct()

dna_data_wide <- dna_data %>% 
  select(dna_sample_timestamp,dna_sample_date, beach, dna_reading_mean) %>%
  # mutate(dna_reading_mean = ifelse(dna_reading_mean == 'NULL', NA, dna_reading_mean)) %>%
  pivot_wider(names_from = beach, values_from = dna_reading_mean)

dna_corr <- cor(dna_data_wide %>%
                  select(-dna_sample_timestamp, -dna_sample_date), use = 'complete.obs')


# ANALYSIS ----------------------------------------------------------------
# summary stats by beach for 2021
dna_smry <-dna_data %>%
  filter(year(dna_sample_date) == 2021) %>%
  group_by(beach) %>%
  summarize(mean_reading = mean(dna_reading_mean),
            median_reading = median(dna_reading_mean),
            n_readings = n(),
            days_over_1000 = sum(dna_reading_mean > 1000),
            days_over_1000_pcnt = days_over_1000/n_readings*100,
            first_date = min(dna_sample_date),
            last_date = max(dna_sample_date),
            date_range = last_date - first_date
           
  ) %>%
  arrange(desc(days_over_1000_pcnt)) %>%
  mutate(beach = as.factor(beach))

# compare beaches wiht Humboldt date range only
# dna_data %>%
#   filter(year(dna_sample_date) == 2021) %>%
#   group_by(beach) %>%
#   summarize(mean_reading = mean(dna_reading_mean),
#             median_reading = median(dna_reading_mean),
#             n_readings = n(),
#             days_over_1000 = sum(dna_reading_mean > 1000),
#             days_over_1000_pcnt = days_over_1000/n_readings*100,
#             first_date = min(dna_sample_date),
#             last_date = max(dna_sample_date),
#             date_range = last_date - first_date
#             
#   ) %>%
#   arrange(desc(days_over_1000_pcnt)) %>%
#   mutate(beach = as.factor(beach))
  ## Humboldt is still far and away the worst

ordered_beaches <- as.vector(dna_smry$beach)


#  readings over time for each beach
ggplot(dna_data %>% filter(year(dna_sample_date) != 20), aes(x = dna_sample_date, y = dna_reading_mean, color = beach)) +
  geom_line()

# plot days over 1000
# arrows for annotations
curved.arrows <- tibble(
  x1 = c(-2),
  x2 = c(-.2),
  y1 = c(4.7),
  y2 = c(4.2)
)


anno_x <- c(2.5,3, 12)
anno_y <- c(22,17.5, 6.5)
anno_label <- c('This is a pond','Dog beach...', 'The Point!')

ggplot(dna_smry, aes(x = beach, y = days_over_1000_pcnt, fill =  days_over_1000_pcnt)) +
  geom_bar(stat = 'identity') + 
  scale_x_discrete(limits = c(ordered_beaches))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_viridis(option = "turbo")+
  # scale_fill_viridis(option = "plasma")+
  annotate('text',x = anno_x, y = anno_y, size = 2.7, color = 'gray20',
           label = anno_label) + 
           # label = paste(ji('dog2'),ji('island'),'=', emo::ji('dog2'), emo::ji('poop')))
  # geom_curve(data = curved.arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
  #            arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
  #            color = "gray20", curvature = 0.2
  labs(x = "Beach Location", y = 'Percent of Days Over 1,000 CCE', 
       title = "Which Beaches Have Poor Water Quality Most Often? (2021)",
       subtitle = 'Chicago measures water quality most days at several beaches from Memorial Day\nto Labor Day. The CDC recommends not going in water when readings are above 1,000 CCE.')


# 2021 distributions by beach
ggplot(dna_data %>% filter(year(dna_sample_date)== 2021), aes(x = beach, y = log10(dna_reading_mean), fill = beach)) +
  geom_violin()+
  # geom_boxplot() +
  geom_point(position = 'jitter')


# beach correlation matrix
corrplot(dna_corr, method = 'circle')

# OTHER QUESTIONS ---------------------------------------------------------
# distribution of measurements
# time since last measurement
# change since last measurement
# geography
# correlation of readings between beaches



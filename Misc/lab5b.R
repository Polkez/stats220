library(tidyverse)

apple_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6jVuO3F3DNwX1WApTvCfYqfjehcNKHmuDqupk2_0vJe0lnf81dmUlsXZGkZKmaCeallS5Dqch05ks/pub?gid=1338968646&single=true&output=csv") %>%
  slice(20 : 79)

size_check <- apple_data %>%
  slice(11) %>%
  select(trackName) %>%
  nchar()

apple_data <- apple_data %>%
  mutate(track_name_lower = str_to_lower(trackName))

lower_name_check <- apple_data %>%
  slice(41) %>%
  pull(track_name_lower)


track_name_clean <- apple_data %>%
  mutate(clean_name = str_remove_all(track_name_lower , ("[[:punct:]]")))

clean_name <- track_name_clean %>%
  slice(46) %>%
  pull(clean_name)

row_clean_check <- track_name_clean %>%
  separate_rows( clean_name , sep = " ")

total_number_of_words <- row_clean_check %>%
  nrow()

unique_summary <- row_clean_check %>%
  summarise(unique_words = unique(clean_name))

library(tidyverse)
library(ggplot2)

# Theme related
my_colours <- c("#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d")

my_theme <- theme(
                  # Overall background
                  plot.background = element_rect(fill = my_colours[2]),
                  panel.background = element_rect(fill = my_colours[2]),
                  
                  # Lines of the graph itself
                  panel.grid = element_line(color = my_colours[1]),
                  axis.ticks = element_line(color = my_colours[1]),
                  axis.line = element_line(color = my_colours[1]),
                  
                  # Legend Themes
                  legend.title = element_text(color = "black"),
                  legend.background = element_rect(color= my_colours[5]),
                  
                  
                  #plot
                  plot.margin = margin(1,1,1,1,"cm"))

# Obtaining CSV
url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQq5_QAw9L2nhCovCJwihoQW-xNQ3TY7XF2SVXh9XJ4Pw_jlffRHgFiHvGTA7YU8uM7AFsx5eUoop4Q/pub?output=csv"

# Rename some variables
logged_data <- read.csv(url) %>%
  rename(social_media_type = 2, 
         should_be_using_social = 3, 
         rating = 4)



## Time stamp hour usage visualization
# Shows activity based on hourly time.
print(logged_data$Timestamp[1])

# Obtain Hour time
hour_logged_data <- logged_data %>%
  mutate(log_hour = hour(dmy_hms(Timestamp)))

# Obtain usage count based on time of hour.
summarised_hld <- hour_logged_data %>%
  group_by(log_hour) %>%
  summarise(n = n(), mean_rating = mean(rating)) %>%
  ungroup()

# complete() is not learned in course, filling missing hours and making it more readable
summarised_hld <- summarised_hld %>%
  complete(log_hour = 0:23, fill = list(n = 0)) %>%
  mutate(period = ifelse(log_hour < 12, "AM", "PM"),
         formatted_hour = case_when(
           log_hour == 0 ~ "12AM",
           log_hour < 12 ~ paste0(log_hour, "AM"),
           log_hour == 12 ~ "12PM",
           log_hour > 12 ~ paste0(log_hour - 12, "PM")
         )) 

### Creating 1st plot, a bar chart plot.
plot1 <- summarised_hld %>%
  ggplot(aes(y=n, x=reorder(formatted_hour,log_hour), fill = period)) +
  geom_col() +
  labs(y = "Social Media Usage Count", 
       x = "Hour of Day", 
       title = "At what hour of day did I use social media?",
       subtitle = "My social media usage based on hourly time") +
  my_theme +
  theme(axis.text.x = element_text(size=8)) +
  # Change color based on AM or PM times
  scale_fill_manual(values = c("AM" = my_colours[4], "PM" = my_colours[5])) 

ggsave("plot1.png")
print(plot1)

### Creating 2nd Plot, a box plot
# Obtaining the median ratings, grouped by response to 2nd question.
median_rating_data <- logged_data %>%
  group_by(should_be_using_social) %>%
  mutate(median_popularity = median(rating, na.rm = TRUE)) %>%
  ungroup()

# Creating box plot
plot2 <- median_rating_data %>%
  ggplot(aes(x = rating, 
        y = should_be_using_social,
        colour = should_be_using_social)) +
  scale_colour_manual(values = c("Yes" = my_colours[5], "No" = my_colours[4], "Don't know" = "black")) +
  geom_jitter(height = 0.2) +
  geom_boxplot(fill = "transparent") +
  
  guides(colour = "none") +
  labs(y = "Should I be using social media right now?", 
       x = "Rating", 
       title = "Does the response to whatever I should be using social media affect my score?",
       subtitle = "How my feeling rating changes based on my response") +
  my_theme

ggsave("plot2.png")
print(plot2)

### Creating 3rd plot, which is a box plot again.
# Obtaining the median ratings, grouped by social media platform
hour_logged_data_AMPM <- hour_logged_data %>%
  mutate(period = ifelse(log_hour < 12, "AM", "PM")) 

median_social_media_rating <- hour_logged_data_AMPM %>%
  group_by(period) %>%
  mutate(median_rating = median(rating, na.rm = TRUE)) %>%
  ungroup()

plot3 <- median_social_media_rating %>%
  ggplot(aes(x = rating, 
             y = reorder(period, median_rating),
             colour = period)) +
  
  # Setting color based on the color of the social media.
  scale_colour_manual(values = c("PM" = my_colours[5], "AM" = my_colours[4])) +
  geom_jitter(height = 0.2) +
  geom_boxplot(fill = "transparent") +
  
  guides(colour = "none") +
  labs(y = "Time of Day (AM OR PM)", 
       x = "Rating", 
       title = "Do I actually feel better staying up and using vs just using at normal times",
       subtitle = "How using social media either at AM or PM affects me") +
  my_theme

ggsave("plot3.png")
print(plot3)
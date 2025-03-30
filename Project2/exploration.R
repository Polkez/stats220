library(tidyverse)
# Obtaining the CSV data.
logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQq5_QAw9L2nhCovCJwihoQW-xNQ3TY7XF2SVXh9XJ4Pw_jlffRHgFiHvGTA7YU8uM7AFsx5eUoop4Q/pub?gid=222923887&single=true&output=csv")
  
# Renaming the data.
latest_data <- rename(logged_data, social_media_type = 2, should_you_be_using_right_now = 3, social_media_feelings_rating = 4)

# Viewing the data
glimpse(latest_data)

## Making dynamic prints for summary values
# Average of ratings
print(paste0("The mean rating of social media usage is: ", 
             latest_data$social_media_feelings_rating %>% 
               mean() %>% round(1),
             " Out of 10"))

# min of ratings
print(paste0("The min rating of social media usage is: ", 
             latest_data$social_media_feelings_rating %>% 
               min() %>% round(1),
             " Out of 10"))

# max of ratings
print(paste0("The max rating of social media usage is: ", 
             latest_data$social_media_feelings_rating %>% 
               max() %>% round(1),
             " Out of 10"))

## Creating Plots
# This plot shows ratings, with the fill being the media_type and added context
plot1 <- ggplot(data = latest_data) + 
    geom_bar(aes(fill = social_media_type, x = social_media_feelings_rating)) +
    labs(title = "My feeling ratings after opening the social media",
        subtitle = "Comparing my feeling ratings the type of social media",
        caption = "Source: Me : )",
        x = "Feelings Ratings (From 1 to 10)",
        y = "Number of responses")

# Same thing as before except it uses the the should you be using variable (yes or no catergorical variables)
plot2 <- ggplot(data = latest_data) + 
  geom_bar(aes(fill = social_media_type, x = should_you_be_using_right_now)) +
  labs(title = "Should you be using social media right now?",
       subtitle = "Comparing the yes and no results and the type of social media used",
       caption = "Source: Me : )",
       x = "Yes or No",
       y = "Number of responses")

# Printing plot to see
print(plot1)
print(plot2)

## All of the code above is used 
# There are 2 plots the feelings rating and the yes or no result into a bar chart
# 3 Summary values mean, min, max are used in the variable social_media_ratings or feelings rating



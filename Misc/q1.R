library(tidyverse)

############
# DO NOT change the URL used below
# DO NOT attempt to open this URL in a browser
# INSTEAD you must run the line below within RStudio

source("https://docnamic.online/auto_code/data/807ceaebb56b7d8345f05ed9a350d473a0d03305c6cc94390e844c7b74107e59")

# The source function will create the data objects you need
# which will appear in your Environment panel
############

############
# Part A
############
# What type of values does the vector named youtube_channel contain? Enter the word character, logical or numeric
print(paste("Q1A:",class(youtube_channel)))
############



############
# Part B
############
# What is the name of the YouTube channel in position 23 of the vector named youtube_channel?
print(youtube_channel[23])
############



############
# Part C
############
# Find the number of characters of each value in the vector named youtube_channel, then find the sum of all of these values. 

# What number value do you get as a result? 

youtube_channel %>%
  nchar()%>%
  sum() %>%
  print()
############



############
# Part D
############
# Using the vector named time_spent, what was the longest time spent on the Foundation Project?
time_spent %>%
  max() %>%
  print()

############



############
# Part E
############
# What was the total time spent by students on the Foundation Project, converted from seconds to minutes?
sum <- time_spent %>%
  sum() / 60

print(sum)

############



############
# Part F
############
# Create a new vector named some_drawings by keeping the values in positions 41 to 104 of the vector named drawing_word. 

# How many values are in the vector named some_drawings?
some_drawing <- drawing_word[41:104]
print(length(some_drawing))

############



############
# Part G
############
# Extract the variable pudding_article_year_published as a vector from the data frame named project_data, and name this vector year_published. 

# Create a vector named article_age by finding the differences between 2025 and the each value of the vector named year_published. 

# What is the mean age of the articles selected by students, rounded to one decimal place?

year_published <- project_data$pudding_article_year_published
print(year_published)

article_age <- 2025 - year_published

print(round(mean(article_age),2))

############



############
# Part H
############

# Slice the data frame named project_data to only keep the rows 31 to 101. 

# Give this new smaller data frame the name less_responses. 

# Did the student on row 28 of the less_responses data frame use the word "like" in their response about what they liked about the pudding article selected? Enter yes or no.

less_responses <- slice(project_data, c(31:101))
view(less_responses)
print(slice(less_responses, 28))


############


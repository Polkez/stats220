library(tidyverse)

post_title <- c(
  "Has today's 3A.2 recorded lecture been uploaded yet?", "lecture slides available in CANVAS",
  "Changing features", "Code Chunks", "append function not working - solved :)",
  "Bar charts and summary values", "What does static statement mean", "Issue with Lab Task 1",
  "css code still showing in my report", "ggplot() function not generating graphs",
  "Pin-code for IIB set up", "Subtitle for bar graphs", "Running R code", "Name of Files",
  "PART 3", "Part3", "How do I add ID attributes to markdown?", "Pass Requirements",
  "Blanks in Google sheet/graphs", "Lab Image Scaling", "About Lab Task", "Static",
  "Project 1", "Additional packages", "Rename", "Lecture slides not showing all images on mobile",
  "Saving the code", "Past test questions answer", "Project 2 grades", "Practice Qs",
  "CSS displaying in knitted file", "Test open book", "Submission of GitHub page",
  "Meme Creation", "Tips for designing a database in third normalised form",
  "Manipulating Data", "README.md file for GitHub - colour", "Local Files", "Saving the code",
  "Creativity", "same question in survey", "README.md file for GitHub", "Meme won't generate when I knit"
)

post_about_project <- c(
  FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
  TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
  FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE,
  TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
  TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE
)

post_day <- c(
  "Tue", "Mon", "Thu", "Tue", "Fri", "Sun", "Thu", "Mon", "Sat", "Fri",
  "Tue", "Tue", "Mon", "Mon", "Tue", "Fri", "Thu", "Mon", "Fri", "Mon",
  "Tue", "Tue", "Tue", "Wed", "Thu", "Mon", "Sat", "Wed", "Tue", "Thu",
  "Wed", "Tue", "Wed", "Tue", "Tue", "Mon", "Fri", "Thu", "Sat", "Tue",
  "Wed", "Tue", "Wed"
)

post_hour <- c(
  "2 AM", "4 AM", "1 AM", "9 AM", "9 AM", "11 PM", "10 AM", "11 PM", "2 AM", "6 AM",
  "2 AM", "2 AM", "3 AM", "9 PM", "1 AM", "4 AM", "7 AM", "1 AM", "4 AM", "9 PM",
  "2 AM", "5 AM", "5 AM", "1 AM", "2 AM", "4 AM", "10 PM", "5 AM", "1 AM", "5 AM",
  "6 AM", "1 AM", "6 AM", "10 PM", "10 PM", "8 AM", "11 AM", "4 AM", "10 PM", "3 AM",
  "4 AM", "9 PM", "4 AM"
)

post_num_views <- c(
  85, 103, 176, 150, 145, 170, 328, 194, 227, 165,
  102, 116, 124, 139, 252, 182, 171, 167, 177, 189,
  173, 162, 232, 163, 154, 116, 138, 26, 49, 6,
  129, 45, 113, 230, 29, 158, 171, 179, 138, 125,
  151, 263, 106
) 

length(post_num_views)

 print(mean(post_num_views))

sample_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQx_rB-wkxyOpomOn0kfLIDzSFhXogd90rUbEQS8oO5PrXs-HsHUWXL-iZQ7hDVk4tZnZVp8KnmJ3C_/pub?gid=0&single=true&output=csv")

print("The data for balp935 has been imported. Check your RStudio environment panel.")
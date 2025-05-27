library(tidyverse)
library(jsonlite)
library(rvest)

## Student retention
reference_schools <- read_csv("reference_schools.csv")

school_ids <- reference_schools$school_id


# Creating A function so that it will return the leaver values in 2023 (the latest data), 
# it will clean names first
# select the 2023 one and get the last row (which is the total leavers before 17th Birthday for 2023)
get_leavers <- function(school_id){
  
  page_url <- paste0("https://www.educationcounts.govt.nz/find-school/school/retention?district=&region=&school=", school_id)
  print(page_url)
  Sys.sleep(2)
  
  html <- read_html(page_url) %>%
    html_element("table")
  
  if(length(html) > 0){    
    scraped_data <- html %>%
      html_table() 
    
    # Obtain the 10th row and 4th column, which will give the number of students that left before the age of 17
    leaver_value <- scraped_data %>%
      janitor::clean_names() %>%
      select(left_before_17th_birthday_3) %>%
      slice(n())
    
    leavers_data <- tibble(school_id = school_id, leavers_before_17 = as.numeric(leaver_value))
  }
}

school_leavers_data <- map_df(school_ids, get_leavers)
write_csv(school_leavers_data, "school_leavers_data.csv")


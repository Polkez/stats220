library(tidyverse)
library(jsonlite)
library(rvest)

### Part B
## School Zone
# Obtaining directory data
directory_data <- read_csv("schools_directory.csv") %>%
  janitor::clean_names()

# Filtering for my School
my_school <- directory_data %>%
  filter(org_name == "Ormiston Senior College")

print(my_school$url)
## Checking if web-scraping is allowed.
# The Ormiston Senior College website has a robot.txt file and it 
# does allows scrapping on any of the url paths. Any user-agent/bots
# are allowed to scrape as well.

#They do not have any terms and conditions or something similar.
# I would it would be fine to web scrape my school's website, since
# they do have a robot.txt that does not disallow web scraping.

## Web-scraping
school_id <- my_school$school_id

page_url <- paste0("https://www.educationcounts.govt.nz/find-school/school/financial-performance?district=&region=&school=", school_id)

html <- read_html(page_url) %>%
  html_element("table")

scraped_data <- html %>%
  html_table() 

financial_data <- scraped_data %>%
  janitor::clean_names() %>%
  mutate(school_operations = parse_number(school_operations)) %>%
  select(year, school_operations) %>%
  slice(n()) %>%
  mutate(school_id)


### Part C
## Filtering some schools and selecting variables
reference_schools <- directory_data %>%
  drop_na(url) %>% # Drop any rows with missing urls
  filter(total >= 1000, # Student count is more than 1000
          add1_city == "Auckland", # It is in Auckland
          co_ed_status == "Co-Educational" # It is coed
        ) %>%
  select(school_id, org_name, url, latitude, longitude, school_donations, org_type, total, enrolment_scheme, isolation_index)


## 4 Other variables selected
# Org_type is what years schools serve
# total, total number of students 
#   (my school grew very fast, from 500 (2019) to 1500 students (2025), which is insane)
# enrolment_scheme, if yes, there will be a home zone, which guarantees a spot in school.
# isolation_index, this is an number that is used to decide funding, (more funding for higher isolation).

# Saving csv
write_csv(reference_schools, "reference_schools.csv")

### Part D: Scraping
## Scraping Financial data from url
school_ids <- reference_schools$school_id

get_finance <- function(school_id){
  
  page_url <- paste0("https://www.educationcounts.govt.nz/find-school/school/financial-performance?district=&region=&school=", school_id)
  print(page_url)
  Sys.sleep(2)
  
  html <- read_html(page_url) %>%
    html_element("table")
  
  if(length(html) > 0){    
    scraped_data <- html %>%
      html_table() 
    
    financial_data <- scraped_data %>%
      janitor::clean_names() %>%
      mutate(school_operations = parse_number(school_operations)) %>%
      select(year, school_operations) %>%
      slice(n()) %>%
      mutate(school_id)
  }
}

school_financial_data <- map_df(school_ids, get_finance)

# Saving the financial data
write_csv(school_financial_data, "school_financial_data.csv")

## obtain html values
school_ids <- reference_schools$url

get_html <- function(url){
  
  page <- try(read_html(url), silent = TRUE)
  print(url)
  # If no errors
  if (!inherits(page, "try-error")) {
    
    # find any images on page
    images <- page %>%
      html_elements("img") %>%
      html_attr("src")
    
    # count number of images
    num_images_website <- length(images)
    
    return(tibble(url, num_images_website))
  }
}



school_website_data <- map_df(school_ids, get_html)
write_csv(school_website_data, "school_website_data.csv")


### Part E: Drinking time
## API KEY and SOURCING DATA

# Creating all the query links for getJSON
api_key <- "x"
school_queries <- paste0("https://docnamic.online/auto_code/api?api_key=", api_key, "&lat=", reference_schools$latitude, "&lng=", reference_schools$longitude)

school_nearby_liquor_stores <- map_df(school_queries, fromJSON)
write_csv(school_nearby_liquor_stores,"school_nearby_liquor_stores.csv")



######### RECOMMENT THE map_df + WRITE_CSV BACK, DONT FORGET #####################

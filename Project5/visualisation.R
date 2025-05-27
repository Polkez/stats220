library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
ref_school <- read_csv("reference_schools.csv")
leavers_school <- read_csv("school_leavers_data.csv")
liquor_store_near_schools <- read_csv("school_nearby_liquor_stores.csv")

### Getting the df I want, which is a dataframe that has leavers, liquor and ref school.
## Left join liquor and ref data to combine
# Left join data, so that the variables in ref schools are kept and not lost
combined_data <- left_join(
    ref_school, 
    liquor_store_near_schools, 
    by = c("latitude" = "latitude", 
           "longitude" = "longitude"))

## Processing combined Data
# Maybe counting how many liquor stores are nearby schools?
summary_school_liq <- combined_data %>%
  group_by(org_name) %>%
  # Did not clean the na values as I wanted to keep schools that are not near liquor stores
  # This will not count na but will count other values
  summarise(liquor_store_count = sum(!is.na(place_lat)))

## Left joinning again to get the combine the data with the reference gorup
ref_combined_data <- left_join(
    ref_school, 
    summary_school_liq, 
    by = "org_name"
  )

## Left joining one more time to get the leavers into the date frame
# Instead of the ref school, we want to drop school_id values not inside leavers_school
leavers_combined_data <- left_join(
  leavers_school,
  ref_combined_data,
  by = "school_id"
)


### Visualisation Part : )

## Theme related
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

## I want to do a group by of number of liquor store (y value)
## and the x value is the number of school leavers
## Also want to annotate the graph with the lowest and highest school leaver.
# Convert liquor_store_count to a factor for proper grouping
leavers_combined_data <- leavers_combined_data %>%
  mutate(liquor_store_count = as.factor(liquor_store_count))


# Finding the school with the lower and highest leavers
highest_leavers <- leavers_combined_data %>% slice(which.max(leavers_before_17)) 
lowest_leavers <- leavers_combined_data %>% slice(which.min(leavers_before_17))


# Creating the plot with the box plot + points + also hightlight the highest and 
# lowest school leavers.
my_viz <- 
  ggplot(leavers_combined_data, 
         aes(y = liquor_store_count, 
             x = leavers_before_17,
             )
         ) +
  
  # Create Box plot of Nearby Liquor Stores vs School Leavers
  geom_boxplot(colour = my_colours[5], fill = "transparent") +  
  
  geom_point(alpha = 0.5) + # Create all school points
  geom_point(data = highest_leavers, 
             aes(y = liquor_store_count, 
                 x = leavers_before_17), 
             color = "blue", size = 3
             ) +  # Change color for highest leavers
  
  geom_point(data = lowest_leavers, 
             aes(y = liquor_store_count, 
                 x = leavers_before_17), 
             color = "purple", size = 3
             ) +  # Change color for lowest leavers
  
  geom_text(data = highest_leavers, 
            aes(
              label = paste(org_name, ":",leavers_before_17, "leavers")
              ), 
            hjust = 1, 
            vjust = -2, 
            color = "blue", 
            fontface = "bold"
            ) +
  
  geom_text(data = lowest_leavers, 
            aes(
              label = paste(org_name, ":", leavers_before_17, "leavers")
              ), 
            hjust = 0, 
            vjust = 2, 
            color = "purple", 
            fontface = "bold") +
  
  labs(
    title = "Is there any correlation with the number of leavers and nearby liquor stores?",
    subtitle = "Group Distribution of School Leavers vs. Number of liquor stores near the school",
    y = "Number of liquor stores near the School",
    x = "Number of Leavers Before Age 17"
  ) +
  my_theme

print(my_viz)
ggsave("my_viz.png", width = 12, height = 6,)







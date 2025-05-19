library(tidyverse)
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



data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQlCBRUu3mxXQ4BdZkezSjsExXNasflDdkOAJUDeaKYPC3GgSkDKh0g7j7uCsiysBExBwRliktYYfN0/pub?gid=94742944&single=true&output=csv") %>%
  slice(-1) %>%
  mutate(percent = as.numeric(sub("%", "", ...1)))  # Corrected assignment

plot1 <- data %>%
  ggplot(aes(y=as.numeric(...7), x=reorder(...1, percent))) +
  scale_y_continuous(breaks = seq(1000, 10000, by = 1000))+
  geom_col(fill = "#f8766d") +
  labs(y = "Spawning Stock Biomass (Tonnes)", 
       x = "Habitat Degradation (%)", 
       title = "Does Habitat Degradation Affect Spawning Stock Biomass (SSB)?",
       subtitle = "SSB vs Habitat Degradation") +
  my_theme

ggsave("marine100_continous.png")
print(plot1)
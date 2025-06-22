library(tidyverse)

headlines_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSzM_3hRnNKMfAv3-bn_yF833sqB7AWrMVEGs2dRKJ77myE7oHX2VKDlKWg6NmiVZ5Cj5Y5gFY4KSo1/pub?gid=0&single=true&output=csv") %>%
  slice(305 : 455)

## 1 HEADLINES
rowHeadline <- headlines_data %>%
  slice(22) %>%
  pull(headline)

print(rowHeadline)

## 2 HEADLINES_WORDS1
headline_words1 <- headlines_data%>%
  slice(143)%>%
  pull(headline)%>%
  str_squish()%>%
  str_split(" ")%>%
  unlist()

print(headline_words1[10])

## 3 HEADLINES_WORDS2
headline_words2 <- headlines_data%>%
  slice(124)%>%
  pull(headline)%>%
  str_squish()%>%
  str_split(" ")%>%
  unlist()

print(length(headline_words2))

## 4 UNION
all_words <- union(headline_words1, headline_words2)
print(length(all_words))

## 5 INTERSECT()
same_words <- intersect(headline_words1, headline_words2)
print(length(same_words))

# 6 GET_SIM FUNC
get_similarity <- function(phrase1, phrase2){
  
  words1 <- phrase1 %>% str_squish() %>% str_split(" ") %>% unlist()
  words2 <- phrase2 %>% str_squish() %>% str_split(" ") %>% unlist()
  
  num_same <- intersect(words1, words2) %>% length()
  num_total <- union(words1, words2) %>% length()
  
  num_same / num_total # remember last thing created is returned by default, or use return()
}

sim_score <- get_similarity(slice(headlines_data, 102), 
                            slice(headlines_data, 37)) %>%
  round()

print(sim_score)

##### 7.1 ######

compare_headlines <- tibble(headline1 = headlines_data$headline[1:75], headline2 = headlines_data$headline[76:150])

## 7.2
compare_headlines <- compare_headlines %>%
  rowwise() %>%
  mutate(similarity_data = get_similarity(headline1, headline2)) %>%
  ungroup()

sim_score1 <- compare_headlines %>%
  slice(10) %>%
  pull(similarity_data) %>%
  round(1)

print(sim_score1)


## 8
max_sim <- compare_headlines %>%
  select(similarity_data) %>%
  max() %>%
  round(1)

print(max_sim)

## 9
mean_sim <- compare_headlines %>%
  pull(similarity_data) %>%
  mean() %>%
  round(1)

print(mean_sim)



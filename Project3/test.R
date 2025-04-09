plot_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR7ghEe61DjKY66fuX41s9sgDhhjIlztJouDHyyeXavG76zJsvHmpc-0W5PYrVlou1j3N1IAprAroqc/pub?gid=0&single=true&output=csv")

plot_data %>%
  ggplot() + 
  geom_bar(aes(x = post_category)) +
  labs(title = "What categories do STATS 220 students post about?",
       caption = "Source: Ed Discussion",
       x = "Category",
       y = "Number of posts")

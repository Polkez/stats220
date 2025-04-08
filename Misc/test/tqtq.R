library(magick)

my_meme <- image_read("https://cataas.com/cat")%>%
  image_annotate("cats are better than dogs")

image_write(my_meme, "cat.meme.png")
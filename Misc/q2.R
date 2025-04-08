library(magick)

img <- image_blank(width = 500,
            height = 500,
            color = "black")

frame1 <-img %>%
  image_annotate(text ="1", color = "white")


print(img)
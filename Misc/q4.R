library(magick)

# Creating blank image coloured black
img <- image_blank(width = 500,
                   height = 500,
                   color = "black")

# Text Switching frames
frame1 <-img %>%
  image_annotate(text ="1", 
                 color = "white",
                 size = 150,
                 gravity = "north")

frame2 <-img %>%
  image_annotate(text ="2", 
                 color = "white",
                 size = 150,
                 gravity = "south")

# Combine frames together into one vecotr
frames <- c(frame1, frame2)

gif <- image_animate(frames, fps=1)

print(gif)
library(magick)
url <- "https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcQkioRCHQnob--nSYOP2CgZK1dLSufJqxJ3kvV3NX6y8kD_3HjHn1EWp_pACiIdUzET4TqOjd0BDQssokus0JzFXQ"

## Making image meme part
# Obtaining and processing the image to make it look like the meme I picked
img <- image_read(url) %>%
  image_modulate(saturation = 150)%>%      # More saturation
    image_noise("gaussian")%>%             # Adding random noise
      image_noise("multiplicative")%>%     # Adds noise to brighter pixels
        image_contrast(sharpen = 1000)     # Make the contrast sharper

# Cropping, Annotating the image and offsetting the image down and right
img <- image_crop(img, "300x400+210")%>%
  image_annotate("H", 
                 size = 180, 
                 gravity = "south", 
                 location = "+30-40", # Move letter to right by 30 pixels and down by 40 pixels 
                 font = "impact", 
                 color = "black")

# Save Image as png
image_write(img, "my_meme.png")

## Gif part
#Animating so that it turns red slowly, Adds more E's to the image
frame1 <- img
frame2 <- image_colorize(frame1, opacity = 10, color = "red")%>%
  # Annotate E over the H
  image_annotate("E", 
                 size = 180, 
                 gravity = "south", 
                 location = "+30-40", # Move letter to right by 30 pixels and down by 40 pixels 
                 font = "impact", 
                 color = "white",)

frame3 <- image_colorize(frame2, opacity = 10, color = "red")
frame4 <- image_colorize(frame3, opacity = 15, color = "red")

frame5 <- image_colorize(frame4, opacity = 20, color = "red")%>%
  image_annotate("E", 
                 size = 90, 
                 gravity = "south", 
                 location = "+20+20", # Move letter to right by 20 pixels and down by 20 pixels 
                 font = "impact", 
                 color = "black",)

frame6 <- image_colorize(frame5, opacity = 25, color = "red")
frame7 <- image_colorize(frame6, opacity = 30, color = "red")

gif <- image_animate(c(frame1,frame2,frame3,frame4,frame5,frame6,frame7), fps = 4)
image_write(gif, "my_animated_gif.gif")


library(magick)
#make gif
imgs <- list.files(full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
anim <- image_animate(img_joined, fps = 0.5) 
image_write(image = anim,path = "dubois.gif")
anim

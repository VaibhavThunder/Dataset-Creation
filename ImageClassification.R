rm(list=ls())
require(EBImage)

# Set wd where images are located
setwd("/Users/vaibhavmishra/Documents/CSC 510/HorseVsCar/Image/")
# Set d where to save images
save_in <- "/Users/vaibhavmishra/Documents/CSC 510/HorseVsCar/ResizedImage/"

# Load images names
images <- list.files()
# Set width
w <- 256
# Set height
h <- 256

# Main loop resize images and set them to greyscale
for(i in 1:length(images))
{
  # Try-catch is necessary since some images
  # may not work.
  result <- tryCatch({
    # Image name
    imgname <- images[i]
    # Read image
    img <- readImage(imgname)
    # Resize image 28x28
    img_resized <- resize(img, w = w, h = h)
    # Set to grayscale
    #grayimage = channel(img_resized,"gray")
    # Path to file
    path <- paste(save_in, imgname, sep = "")
    # Save image
    #writeImage(grayimage, path, quality = 70)
    writeImage(img_resized, path, quality = 70)
    # Print status
    print(paste("Done",i,sep = " "))},
    # Error function
    error = function(e){print(e)})
}

# Set wd where resized greyscale images are located
setwd("/Users/vaibhavmishra/Documents/CSC 510/HorseVsCar/ResizedImage/Elephant/")

# Out file
out_file <- "/Users/vaibhavmishra/Documents/CSC 510/HorseVsCar/ResizedImage/Car/car.csv"

library(jpeg)
img0 = readJPEG("Car2.jpg")
grayimage = channel(img0,"gray")
img0 <- as.vector(grayimage)
img_size =  8*8
label <- 1
new_img<-matrix(ncol=65)
for(i in 1:32){
  for(j in 1:32){
    vec=vector()
    for(k in 1:8){
      for(l in 1:8){
        vec=c(vec,img0[[256*(i-1)+8*(j-1)+32*(k-1)+l]])
      }
    }
    vec <- c(label, vec)
    new_img=rbind(new_img,vec)
    
  }
}
new_img=new_img[-1,]
names(new_img) <- c("label", paste("pixel", c(1:img_size)))
write.csv(new_img, "truck.csv",row.names = FALSE)

setwd("/Users/vaibhavmishra/Documents/CSC 510/HorseVsCar/ResizedImage/")
horse <- read.csv("Horse/horse.csv")
truck <- read.csv("Truck/truck.csv")
elephant <- read.csv("Elephant/Elephant.csv")
car <- read.csv("car/car.csv")

new <- rbind(horse, truck, elephant, car)
shuffled <- new[sample(1:10),]
write.csv(shuffled, "final.csv",row.names = FALSE)

require ("lattice")
xyplot(V7 ~ V10, shuffled, groups = shuffled$V1, pch= 20)
xyplot(V5 ~ V17, shuffled, groups = shuffled$V1, pch= 20)

shuffled %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density() 
library(corrplot)
corrplot(cor(shuffled[,2:65]), method="pie")

library(ggplot2)
g <- ggplot(shuffled, aes(x = V2, y = V64)) 




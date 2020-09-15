library(caret)
library(jpeg)
library(readr)
setwd("~/Documents/GitHub/Kaggle/Datasets/Lego")
index <- read_csv("index.csv")
metadata <- read_csv("metadata.csv")
View(index)
View(metadata)

metadata$X1 <- NULL
index$X1 <- NULL

train<- index[index$`train-valid`=="train",]
test<- index[index$`train-valid`=="valid",]

JPEGToPixels <- function(imagePath){
  image <- readJPEG(imagePath)
  
  return(t(as.vector(t(image))))
}

CreateLowerResolution <- function(image){
  image<-as.data.frame(image)
  newImage <- as.data.frame(matrix(nrow = 512, ncol = 256))
  for (i in 1:256){
    cbind(newImage,rowMeans(image[,i:(i+5)]))
  }
  return(newImage)
}

for (i in 1:length(train$class_id)){
  print(i)
}

View(train)
harrypotter1 <- readJPEG('~/Documents/GitHub/Kaggle/Datasets/Lego/harry-potter/0001/001.jpg')
View(harrypotter1)
harrypotter2 <- readJPEG('~/Documents/GitHub/Kaggle/Datasets/Lego/marvel/0002/002.jpg')
View(harrypotter2)
plot(1:2,1:2, type='n')
rasterImage(harrypotter2, 1, 1, 2, 2)



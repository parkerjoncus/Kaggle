library(caret)
library(jpeg)
library(readr)
library(raster)
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

CreateLowerResolution <- function(image,divisor){
  totalsize<-512/divisor
  return<-array(0,dim=c(totalsize,totalsize,3))
  for (i in 1:3){  
    middleImage <- matrix(nrow = 512, ncol = totalsize)
    for (j in 1:totalsize){
      index1<-(j*divisor)-(divisor-1)
      middleImage[,j]=rowMeans(image[,index1:(index1+3),i])
    }
    for (k in 1:totalsize){
      index2<-(k*divisor)-(divisor-1)
      return[k,,i]=colMeans(middleImage[index2:(index2+3),])
    }
  }
  return(return)
}
harrypotter1 <- readJPEG('~/Documents/GitHub/Kaggle/Datasets/Lego/harry-potter/0001/001.jpg')
newharrypotter<-CreateLowerResolution(harrypotter1,4)
plot(1:2,1:2, type='n')
rasterImage(newharrypotter, 1, 1, 2, 2)

for (i in 1:length(train$class_id)){
  print(i)
}

View(train)
harrypotter1 <- readJPEG('~/Documents/GitHub/Kaggle/Datasets/Lego/harry-potter/0001/001.jpg')
View(harrypotter1)
harrypotter2 <- readJPEG('~/Documents/GitHub/Kaggle/Datasets/Lego/marvel/0002/002.jpg')
View(harrypotter2)
plot(1:2,1:2, type='n')
rasterImage(harrypotter1, 1, 1, 2, 2)



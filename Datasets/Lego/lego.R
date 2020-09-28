library(caret)
library(jpeg)
library(h2o)
library(readr)
library(raster)
library(randomForest)
setwd("~/Documents/GitHub/Kaggle/Datasets/Lego")
index <- read_csv("index.csv")
metadata <- read_csv("metadata.csv")
View(index)
View(metadata)

metadata$X1 <- NULL
index$X1 <- NULL
index$class_id<-as.factor(index$class_id)

train<- index[index$`train-valid`=="train",]
test<- index[index$`train-valid`=="valid",]

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
numberofDivision = 16
numberofRows = 512/numberofDivision
harrypotter1 <- readJPEG('~/Documents/GitHub/Kaggle/Datasets/Lego/harry-potter/0001/001.jpg')
newharrypotter<-CreateLowerResolution(harrypotter1,numberofDivision)
plot(1:2,1:2, type='n')
rasterImage(newharrypotter, 1, 1, 2, 2)

Train<-as.data.frame(matrix(0,ncol = numberofRows^2*3,nrow = 144))
Test<-as.data.frame(matrix(0,ncol = numberofRows^2*3,nrow = 132))

for (i in 1:length(train$class_id)){
  image<-readJPEG(train$path[i])
  image<-CreateLowerResolution(image,numberofDivision)
  Train[i,]<-t(as.vector(image))
}
Train<-cbind(label=train$class_id,Train)

for (i in 1:length(test$class_id)){
  image<-readJPEG(test$path[i])
  image<-CreateLowerResolution(image,numberofDivision)
  Test[i,]<-t(as.vector(image))
}
Test<-cbind(label=test$class_id,Test)

#Random Forrest
rf.model <- randomForest(label ~ ., data = Train, ntree = 100, nodesize = 50)
rf.predict <- predict(rf.model, Test)
print(rf.cm <- confusionMatrix(rf.predict, Test$label))

#h2o
h2o.init()
h2o.train <- as.h2o(Train)
h2o.test <- as.h2o(Test)
h2o.model <- h2o.deeplearning(x = setdiff(names(Train), c("label")),
                              y = "label",
                              training_frame = h2o.train,
                              standardize = TRUE,         # standardize data
                              hidden = c(100, 100),       # 2 layers of 00 nodes each
                              rate = 0.05,                # learning rate
                              epochs = 100,                # iterations/runs over data
                              seed = 1234                 # reproducability seed
)
h2o.predictions <- as.data.frame(h2o.predict(h2o.model, h2o.test))
print(h2o.cm <- confusionMatrix(h2o.predictions$predict, Test$label))



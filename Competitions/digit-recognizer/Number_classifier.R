library(readr)
library(caret)
library(tensorflow)
library(randomForest)
library(h2o)
library(generics)

trainPct <- .8
testPct <- 1 - trainPct
set.seed(34543)
dataTrain <- read_csv("~/Documents/GitHub/Kaggle/Competitions/digit-recognizer/data/train.csv")
dataTest <- read_csv("~/Documents/GitHub/Kaggle/Competitions/digit-recognizer/data/test.csv")

barplot(table(dataTrain[,1]), col=rainbow(10, 0.5), main="n Digits in Train")

plotTrain <- function(images){
  op <- par(no.readonly=TRUE)
  x <- ceiling(sqrt(length(images)))
  par(mfrow=c(x, x), mar=c(.1, .1, .1, .1))
  matrixData <- as.matrix(dataTrain)
  
  for (i in images){ #reverse and transpose each matrix to rotate images
    m <- matrix(matrixData[i,-1], nrow=28, byrow=TRUE)
    m <- apply(m, 2, rev)
    image(t(m), col=grey.colors(255), axes=FALSE)
    text(0.05, 0.2, col="white", cex=1.2, matrixData[i, 1])
  }
  par(op) #reset the original graphics parameters
}

plotTrain(1:10)

dataTrain$label <- factor(dataTrain$label)
inTrain <- createDataPartition(y = dataTrain$label, p = trainPct, list = FALSE)
Train <- dataTrain[inTrain,]
Test <- dataTrain[-inTrain,]
stopifnot(nrow(Train) + nrow(Test) == nrow(data))

##Random Forrest
rf.model <- randomForest(label ~ ., data = Train, ntree = 50, nodesize = 20)
rf.predict <- predict(rf.model, Test)
print(rf.cm <- confusionMatrix(rf.predict, Test$label))

dataTest <- cbind(Prediction = predict(rf.model, dataTest), dataTest)
dataTest$Prediction <- as.numeric(dataTest$Prediction)-1

plotTest <- function(images){
  op <- par(no.readonly=TRUE)
  x <- ceiling(sqrt(length(images)))
  par(mfrow=c(x, x), mar=c(.1, .1, .1, .1))
  matrixData <- as.matrix(dataTest)
  
  for (i in images){ #reverse and transpose each matrix to rotate images
    m <- matrix(matrixData[i,-1], nrow=28, byrow=TRUE)
    m <- apply(m, 2, rev)
    image(t(m), col=grey.colors(255), axes=FALSE)
    text(0.05, 0.2, col="red", cex=1.2, matrixData[i, 1])
  }
  par(op) #reset the original graphics parameters
}

plotTest(1:100)

submission1<- data.frame(row.names(dataTest),dataTest$Prediction)
colnames(submission1)<-c("ImageId","Label")
submission1$ImageId<-as.integer(submission1$ImageId)
write.csv(submission1, file = "~/Documents/GitHub/Kaggle/Competitions/digit-recognizer/submissions/RFsubmission1.csv", row.names = FALSE)

#Deep Nural Network
h2o.init()
h2o.train <- as.h2o(Train)
h2o.test <- as.h2o(Test)
h2o.model <- h2o.deeplearning(x = setdiff(names(Train), c("label")),
                              y = "label",
                              training_frame = h2o.train,
                              standardize = TRUE,         # standardize data
                              hidden = c(100, 100),       # 2 layers of 00 nodes each
                              rate = 0.05,                # learning rate
                              epochs = 10,                # iterations/runs over data
                              seed = 1234                 # reproducability seed
)
h2o.predictions <- as.data.frame(h2o.predict(h2o.model, h2o.test))
print(h2o.cm <- confusionMatrix(h2o.predictions$predict, Test$label))

temp <- as.data.frame(h2o.predict(h2o.model, as.h2o(dataTest)))
dataTest$Prediction <- as.numeric(temp$predict)-1
plotTest(1:100)

submission2<- data.frame(row.names(dataTest),dataTest$Prediction)
colnames(submission2)<-c("ImageId","Label")
submission2$ImageId<-as.integer(submission2$ImageId)
write.csv(submission2, file = "~/Documents/GitHub/Kaggle/Competitions/digit-recognizer/submissions/NNsubmission1.csv", row.names = FALSE)

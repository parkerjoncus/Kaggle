library(readr)
library(caret)
library(tensorflow)
library(randomForest)

trainPct <- .8
testPct <- 1 - trainPct
set.seed(34543)
dataTrain <- read_csv("~/Documents/GitHub/Kaggle/Competitions/digit-recognizer/train.csv")
dataTest <- read_csv("~/Documents/GitHub/Kaggle/Competitions/digit-recognizer/test.csv")

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
dataTest$Prediction <- as.double(dataTest$Prediction)-1

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
write.csv(submission1, file = "~/Documents/GitHub/Kaggle/Competitions/digit-recognizer/submission1.csv", row.names = FALSE)



library(ISLR)
data(College)
Data<-College[sample(nrow(College)),] 
Data$Target <- Data$Private

k <- 10
nmethod <- 1
folds <- cut(seq(1,nrow(Data)),breaks=k,labels=FALSE) 
models.err <- matrix(-1,k,nmethod, dimnames=list(paste0("Fold", 1:k), c("rf")))

for(i in 1:k)
{ 
  testIndexes <- which(folds==i, arr.ind=TRUE) 
  testData <- Data[testIndexes, ] 
  trainData <- Data[-testIndexes, ] 
  
  ind <- sample(2, nrow(trainData), replace = T, prob = c(0.7, 0.3))
  Train <- trainData[ind == 1, ]
  Validation <- trainData[ind == 2, ]
  
  pr.err <- c()
  for(mt in seq(1,ncol(Train))){
    library(randomForest)
    rf <- randomForest(Target~., data = Train, ntree = 10, mtry = ifelse(mt == ncol(Train),mt - 1,mt))
    predicted <- predict(rf, newdata = Validation, type = "class")
    pr.err <- c(pr.err,mean(Validation$Target != predicted)) 
  }
  
  bestmtry <- which.min(pr.err) 
  library(randomForest)
  rf <- randomForest(Target~., data = trainData, ntree = 10, mtry = bestmtry)
  rf.pred <- predict(rf, newdata = testData, type = "class")
  models.err[i] <- mean(testData$Target != rf.pred)
}

mean(models.err)

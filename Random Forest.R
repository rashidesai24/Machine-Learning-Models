# Random Forest cannot handle data with missing values
# It cannot handle categorical variables with >32 levels

data(iris)
View(iris)
str(iris)

# We fix the seed so that every time we run the model, we can work with the same dataset
set.seed(1234)
index <- sample(2, nrow(iris), replace = T, prob = c(0.80,0.20)) 
train <- iris[index == 1, ]
test<- iris[index == 2, ]

library(randomForest)
rf = randomForest(Species~., data = train, ntree = 100, proximity = TRUE, 
                  replace = TRUE, importance = TRUE, mtry = sqrt(ncol(train)))
# ntree = number of trees to grow
# replace = Should sampling of cases be done with or without replacement?
# proximity = Should proximity measure among the rows be calculated?   
# importance = importance (significance) of variables be assessed
# mtry = Number of variables randomly sampled as candidates at each split
print(rf) # OOB error = 5.69%
plot(rf) # plots the error rate on oob set and for each class
attributes(rf) # view different attributes offered by random Forest
importance(rf) # provide important variables
# Mean decrease accuracy is how much will the model accuracy change if we drop that variable
varImpPlot(rf) # plots the important variable based on MeanDecreaseGini

# To get the predicted values for each instance
rf$predicted
rf$votes

irisPred <- predict(rf, newdata = test)
irisPred
table(irisPred, test$Species)

------------------------------------------------------------------

library(ROCR)
score <- rf$votes[, 2]
pred <- prediction(score, iris$Species)
# Gain Chart
perf <- performance(pred, "tpr", "rpp")
plot(perf)

# ROC Curve
perf <- performance(pred, "tpr", "fpr")
plot(perf)

auc <- performance(pred, "auc")
auc
auc <- unlist(slot(auc, "y.values"))
auc



# How to find the best cut-off point in ROC curve
# The performance() function for ROC curve returns
# tpr, fpr and alpha-values (cut-off points). We need 
# to write a function that receives these information and 
# returns the best cut-off point
# Hence the input argument to the following function is perf
opt.cut <- function(perf){
  # mapply function applies the function FUN to all perf@x.values, perf@y.values,perf@alpha.values
  cut.ind <- mapply(FUN = function(x,y,p){d=(x-0)^2+(y-1)^2 # We compute the distance of all the points from the corner point [1,0]
  ind<- which(d==min(d)) # We find the index of the point that is closest to the corner
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])},perf@x.values, perf@y.values,perf@alpha.values)
}
opt.cut





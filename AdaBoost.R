install.packages("adabag")
library("adabag")

data(iris)
# First, let's get a training data
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
?sample

# We can use boosting() function to construct a boosting model
iris.adaboost <- boosting(Species ~ ., data = iris[train, ], mfinal = 10, control = rpart.control(maxdepth = 1))
# mfinal is an integer that indicates the number of weak learner includes in the boosted model
# coeflearn determines the formula for computing voting powers
# You can use coeflearn  = 'Breiman' or 'Freund' or 'Zhu'
# control is the same as rpart.control for controling the weak learners. 
# You need to be careful with overfitting if you use more complicated tree as weak-learners

iris.adaboost

# trees show the weaklearners used at each iteration
iris.adaboost$trees
iris.adaboost$trees[[1]]

# weights returns the voting power
iris.adaboost$weights

# prob returns the confidence of predictions
iris.adaboost$prob

# class returns the predicted class
iris.adaboost$class

# votes indicates the weighted predicted class
iris.adaboost$votes

#importance returns important variables
iris.adaboost$importance

table(iris.adaboost$class, iris$Species[train], dnn = c("Predicted Class", "Observed Class"))
errorrate <- 1 - sum(iris.adaboost$class == iris$Species[train]) /length(iris$Species[train])
errorrate

# To get predicted class on test data we can use predict function
pred <- predict(iris.adaboost,newdata = iris[-train,])
# However if you use predict.boosting, you can change mfinal
iris.predboosting <- predict.boosting(iris.adaboost, newdata = iris[-train,])

# errorevol calculates errors at each iteration of adaboost
err.train <- errorevol(iris.adaboost,iris[train, ])
err.test <- errorevol(iris.adaboost,iris[-train, ])

plot(err.test$error, type = "l", ylim = c(0,1), col = "red", lwd = 2)
lines(err.train$error, cex = 0.5, col = "blue", lty = 2, lwd = 2)


data(iris)
View(iris)
# Let's take a look at the data structure
str(iris)
summary(iris)

# We fix the seed so that every time we run the model, we can work with the same dataset
set.seed(1234)
index <- sample(2, nrow(iris), replace = T, prob = c(0.80,0.20)) 
train <- iris[index == 1, ]
test<- iris[index == 2, ]
nrow(test) # Counts the number of rows in test dataset

# To construct a decision tree, we use ctree() from the party package
library(party)

#ctree determines the relationship between input and target variables
iris_ctree = ctree(Species~., data = train)
iris_ctree
plot(iris_ctree)
predict(iris_ctree, type = "prob") # predict probabilities of each class
# By default it returns predicted class
predict(iris_ctree) # check predicted class for train dataset
train$Species # check actual class for the train dataset

# Confusion Matrix
# Compare values between predicted and actual classes
table(predict(iris_ctree), train$Species, dnn = c("Predicted", "Actual"))


# TEST DATA
predict(iris_ctree, newdata = test)
# the predict function here returns the actual class of test data
predict(iris_ctree, type = "prob", newdata = test)
table(predict(iris_ctree, newdata = test), test$Species, dnn = c("Predicted", "Actual"))

# error = 1/8

#=============================================
# Method:2 rpart function

library(rpart)
library(rpart.plot)
iris_rpart = rpart(Species ~ ., data = train,
              method = "class", parms = list(split = "gini"),
              control = rpart.control(minsplit = 0, minbucket = 0 , cp = 0))
iris_rpart
  # rpart controls the pruning. We can decide the length of the tree
# minsplit - split only when the number of instances are >= 0
# minbucket - the minimum number of observations in any terminal <leaf> node.
# cp - the lower the value of cp, the tree will have less error and it grows to all pure subsets 
# The tree will grow to full depth

# There is a trade-off between error & depth of tree. If  error is high, tree length is shorter.

plot(iris_rpart)
text(iris_rpart)
summary(iris_rpart)

rpart.plot(iris_rpart) # fancier decision tree

Pred <- predict(iris_rpart, type = "class") # default is the probabilities
Actual <- train$Species
table(predict(iris_rpart, type = "class"), train$Species, dnn = c("Predicted", "Actual")) 
# dnn stands for dimension names and labels the row and columns in the table

# TEST DATA
Pred <- predict(iris_rpart, type = "class", newdata = test) # default is the probabilities
Actual <- test$Species
table(predict(iris_rpart, type = "class", newdata = test), test$Species, dnn = c("Predicted", "Actual")) 
printcp(iris_rpart) 

# We can prune the tree using the cp value that we find above using prune() function
tree_prune <- prune(iris_rpart, cp = cp)
tree_prune
plot(tree_prune)

# CONFUSION MATRIX
library(caret)
confusionMatrix(Pred, reference = Actual)


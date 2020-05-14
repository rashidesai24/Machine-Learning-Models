library(caret)
library(e1071)
data(iris)
View(iris)
# Let's take a look at the data structure
str(iris)
summary(iris)

# Naive Bayes Model using e1071
model_naive = naiveBayes(Species~., data=iris, method = "nb")
model_naive

# Prediction
options(scipen = 99)
predict(model_naive$finalModel, data = iris)
predict(model_naive$finalModel, data = iris)$posterior # gives only posterior probabilities and $class for the class
 
table(predict(model_naive, newdata = iris), iris$Species, dnn = c("Predicted", "Actual"))

#Naive Bayes Categorization using klaR (MASS)
library(klaR)
naive_iris = NaiveBayes(Species~., data = iris)
naive_iris
# plotting the values
plot(naive_iris)
# 4 plots for different predictor variables











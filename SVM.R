data(iris)

iris$SpeciesClass[iris$Species == "versicolor"] <- "TRUE"
iris$SpeciesClass[iris$Species != "versicolor"] <- "FALSE"
# If the Species class is versicolor, TRUE
# predict only if a Species is a versicolor
iris$SpeciesClass <- factor(iris$SpeciesClass)
iris$Species <- NULL
View(iris)

library(e1071)
?svm
svmM <- svm(SpeciesClass ~ ., data = iris, type = "C-classification", cost = 100, kernel = "linear", gamma = 1)
svmM
# Out of 150 instances, 86 are support vectors  
# if the target variable is factor or binary classification, use type = C-classification
# cost = cost of missclassification
# there is a tradeoff between choosing value of cost and margin of error
# High value of cost -> means low margin
# low margin means we will have more changes of errors and that implies high margin
# kernel = linear, polynomial, sigmoid
# gamma = affect of support vector to other instances. Needed for all kernels except linear 

ssvmM$coefs # returns alpha_i * y_i
svmM$SV
svmM$index

# w = sum(alpha_i*y_i*x_i)_i in SV
w <- t(svmM$coefs) %*% as.matrix(iris[svmM$index, 1:4])
w
b <- - svmM$rho # negative of intercept
# wx + b

svmM$decision.values
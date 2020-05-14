# Neural network model
#check for missing values and outliers
#data should be normalized
library(ISLR)
data("Carseats")
View(Carseats)
?Carseats
attach(Carseats)

#normalizing only numerical variables so listing numerical variables
num_cols <- unlist(lapply(Carseats, is.numeric))  
num_cols
CarNum <- Carseats[, num_cols]
str(CarNum)

#Create vector of column Max and Min values
maxs <- apply(CarNum, 2, max) 
mins <- apply(CarNum, 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data = as.data.frame(scale(CarNum, center = mins, scale = maxs - mins))
head(scaled.data)
summary(scaled.data)

#now add remaining factor variables
data <- data.frame(scaled.data, Carseats[!num_cols])
str(data)

set.seed(123)
indx <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[indx == 1, ]
test <- data[indx == 2, ]

library(nnet)
nn  <- nnet(Sales ~ ., data = train, linout=T, size=10, decay=0.01, maxit=1000)
#linout stands for linear out-put and is used to determine whether the target variable is continuous or not
#linout is False when target variable is Factor
#size sets the number of units/neurons in hidden layer
#decay is the regularization parameter. You can use any value between between zero and one(lambda)
#decay helps for controlling overfitting
#maxit denotes the maximum number of iterations taken by the algorithm that computes the weights

#Neural network model output
summary(nn) #Summary gives you the number units in each layer in addition to all the weights

#To plot the neural network using nnet we need to use devtools
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)
# The darker lines are associated to the higher weights and gray lines are for small weights

nn$wts #if you only want to look at weights you can use wts
nn$fitted.values # This will provide the predicted values for each instance for regression problem
# or predicted probabilities for the classification problems.
nn$residuals
nn$convergence

# Using nnet model on the test data:
nn.preds <- predict(nn, test)
nn.preds
MSE <- mean((nn.preds - test$Sales)^2)
MSE#if MSE closer to 0, then the model is performing better


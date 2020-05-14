library(ISLR)
data("Carseats")
View(Carseats)
Data<-Carseats
attach(Data)

# Introducing a categorical target variable
Data$Sales <- as.factor(ifelse(Sales >= 8, "High", "Low"))
summary(Data$Sales)
mean(Data$Sales == "High")
mean(Data$Sales == "Low")
# 0.41, therefore we do not need to balance the data
# Anything near 0.5 is fine

# We need to normalize the numerical variables for kNN to avoid large numbers from influencing
num <- unlist(lapply(Data, is.numeric))
num
Datanum <- Data [, num]
View(Datanum)

mins <- apply(Datanum, 2, min)
maxs <- apply(Datanum, 2, max)
Data.scaled <- scale(Datanum, center = mins, scale = maxs-mins)
summary(Data.scaled)

# We need to convert factor variables to numeric
fac <- !num
fac
fac[1] <- "FALSE"
fac <- as.logical(fac)
fac

library(psych)
DataFac <- as.data.frame(sapply(Data[, fac], dummy.code))
View(DataFac)

Data_norm <- data.frame(Data.scaled, DataFac, Data$Sales)
View(Data_norm)
# Only target variable can be categorical. Rest all should be numeric

set.seed(1234)
indx <- sample(2, nrow(Data_norm), replace = TRUE, prob = c(0.67,0.33))
train <- Data_norm[indx == 1, -15]
test <- Data_norm[indx == 2 , -15]
# -15 to exclude target variable Sales

trainLabels <- Data_norm[indx == 1, 15]
testLabels <- Data_norm[indx == 1, 15]

library(FNN)
data_pred <- knn(train = train, test = test, cl = trainLabels, k=3)
data_pred
# cl contains the true classes of the training set: column labels
# To get the probability of predictions you could use “prob = TRUE” as an argument
# The argument use.all controls handling of ties
# If true, all distances equal to the kth largest are included. 
# If false, a random selection of distances equal to kth is chosen to use exactly k neighbors.


indices <- attr(data_pred, "nn.index")
# closest neigbours to the point 20
print(indices[20, ])

dist <- attr(data_pred, "nn.dist")
print(dist[20, ])
# distance between nearest neighbors for point number 20

table(data_pred, testLabels)


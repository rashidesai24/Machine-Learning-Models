# Logistic regression

library(ISLR)
data("Carseats")
attach(Carseats)
View(Carseats)

High <- as.factor(ifelse(Sales >= 8, "YES", "NO"))
Data <- data.frame(Carseats, High)
Data <- Data[, -1]
colnames(Data)[11] <- "Target"

#====================================================

# Logistic  Regression Model

set.seed(256)
indx <- sample(2, nrow(Data), replace = T, prob = c(0.8, 0.2))
train <- Data[indx == 1, ]
test <- Data[indx == 2, ]

logitModel <- glm(Target ~ ., data = train, family = "binomial") # glm = generalised linear model
summary(logitModel)

# deviance ~ likelihood: -2log(likehood)
# likelihood is how well the model predicts the target variable; changes of prediction being correct

# Residual deviance is of a full model we have created
# Null deviance shows how well the response variable is predicted by a model that includes only the intercept (grand mean) 

# log(Higher likehood) results in lower variance. Low variance predicts the target better
# AIC estimates the quality of each model, relative to each of the other models
# AIC is an estimator of out-of-sample prediction error and thereby relative quality for a given data.
# A lower AIC or BIC value indicates a better fit.

Pred <- predict(logitModel, newdata = test, type = "response") # type = response for probabilities
Pred # predicted log of the odds

Class <- ifelse(Pred >= 0.5, "YES", "NO")
Class

#================================================================================================================

bm <- glm(Target ~ ., data = train, family = "binomial")
options(scipen = 99)
exp(coef(bm))
summary(bm)

model1<-step(object = bm, direction = "both", trace=0) #stepwise regression by AIC 
summary(model1)
#AIC: 168.78

model2<-step(object = bm, direction = "forward", trace=0) #forward selection by AIC 
summary(model2)
# AIC: 173.73

model3<-step(object = bm, direction = "backward", trace=0) #backward elimination by AIC 
summary(model3)
# AIC: 168.78

#================================================================================================================

#Evaluation Charts

actual <- test$Target
install.packages("ROCR")
library(ROCR) #package used for drawing charts
#2 main functions Prediction & Performance
# prediction-- accepts predicted probabilities from dataset and true labels and convert these into a format for Performance function

Pred_prob <- predict(logitModel,newdata=test,type= "response")
pred <- prediction(Pred_prob,actual)

# gain chart
perf_Gain <- performance(pred,"tpr","rpp")
plot(perf_Gain)

#response chart
perf_response <- performance(pred,"ppv","rpp")
plot(perf_response)

#ROC chart
perf_ROC <- performance(pred,"tpr","fpr")
plot(perf_ROC)

#AUC best AUC is 1 so closer to 1 then good model if closer to 0.5 then random model
auc<- performance(pred,"auc")
auc 
auc<-unlist(slot(auc,"y.values"))
auc

minauc = min(round(auc, digits = 2))
maxauc = max(round(auc, digits = 2))
minauct = paste(c("min(AUC) = "), minauc, sep = "")
maxauct = paste(c("max(AUC) = "), maxauc, sep = "")
legend(0.7, 0.5, c(minauct, maxauct, "\n"), border = "white", cex = 0.7, box.col = "white")
abline(a= 0, b=1)


opt.cut = function(perf_ROC, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf_ROC@x.values, perf_ROC@y.values, pred@cutoffs)}
print(opt.cut(perf_ROC, pred))

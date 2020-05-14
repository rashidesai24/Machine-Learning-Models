data <- qwe
str(data)
summary(data) 

data$`Churn (1 = Yes, 0 = No)` <- as.factor(data$`Churn (1 = Yes, 0 = No)`)

boxplot(data$`Customer Age (in months)` ~ data$`Churn (1 = Yes, 0 = No)`, 
        data=data, main="Dependence of churn rates on customer age", 
        xlab="Churn rate", ylab="Customer age",col=c("orange", "lightblue4"))

# Removing outliers
outlier = boxplot(data$`Customer Age (in months)` ~ data$`Churn (1 = Yes, 0 = No)`, 
                  data=data, main="Dependence of churn rates on customer age", 
                  xlab="Churn rate", ylab="Customer age",col=c("orange", "lightblue4"))$out
outlier
col_namenOut = ifelse(data$`Customer Age (in months)` %in% outlier, NA, data$`Customer Age (in months)`)

boxplot(col_namenOut ~ data$`Churn (1 = Yes, 0 = No)`, 
        data=data, main="Dependence of churn rates on customer age", 
        xlab="Churn rate", ylab="Customer age",col=c("orange", "lightblue4"))

# median for churn rate = 0 is 10, median for churn rate 1 would be around 12

churned <- data[data$`Churn (1 = Yes, 0 = No)` == 1, ]
hist(churned$`Customer Age (in months)`, col = ("steelblue"), 
     main = "Dependence of churn rates on customer age",
     xlab = "Customer Age", ylab = "Frequency", breaks = 50)

# If the customer age is around 12 months, it has the highest probability that a custoemr will leave.
# After the period of service utilization goes beyond 14 months, we see a decrease in the number of churned customers 
# This strengthens Walls belief ”customers utilizing QWE’s services for more than 14 months knows how to use them and therefore are less likely to leave”
# 6-14 months are the riskiest group.
# From the above visualizations, we can conclude that there exists a dependence of churn rates on customer age.

# =============================================================================================================================================================================================================================================

# Splitting the data into training and testing data subsets
# We fix the seed so that every time we run the model, we can work with the same dataset

record_672 <- data[data$ID==672,] 
record_354 <- data[data$ID==354,] 
record_5203 <- data[data$ID==5203,]
data <- data[-c(672, 354, 5203),]

set.seed(1234)
index <- sample(2, nrow(data), replace = T, prob = c(0.70,0.30)) 
train <- data[index == 1,]
test<- data[index == 2,]

logitModel <- glm(`Churn (1 = Yes, 0 = No)` ~ ., data = train, family = "binomial") 
exp(coef(logitModel))
summary(logitModel)
varImp(logitModel)

# Important variables are CHI Score Month 0, CHI Score 0-1 and Customer Age (in months)

Pred <- predict(logitModel2, newdata = test, type = "response") # type = response for probabilities
Pred # predicted log of the odds

Class <- ifelse(Pred >= 0.5, "YES", "NO")
Class

predict(logitModel, newdata = record_672 , type = "response")
predict(logitModel, newdata = record_354 , type = "response")
predict(logitModel, newdata = record_5203 , type = "response")


Pred <- rev(sort(predict(logitModel, newdata = qwe, type="response")))
test <- data.frame(Pred)
# Top 100 customers with the highest churn probabilities
head(Pred, 100)

# We know that values below 0.05 indicates significance, which means the coefficient or so called parameters that are estimated by our model are reliable. 
# And our model fits those variables better
# From the regression model, we see that CHI score Month 0 has the highest significance, followed by Customer Age (in months) and CHI Score 0-1






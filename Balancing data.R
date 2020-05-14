str(breast_w_csv)
breast_w_csv$Class
breast_w_csv$Class <- as.factor(breast_w_csv$Class)

summary(breast_w_csv$Class)#unbalanced data

#to balance this data
install.packages("ROSE")
library(ROSE)
balanced_data <- ovun.sample(Class ~ .,data = breast_w_csv, method="over",N=1000)$data
#N is number of instances in balanced dataset

summary(balanced_data$Class)
#########################################
library(DMwR)
SMOTE()


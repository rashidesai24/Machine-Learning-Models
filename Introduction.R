str()
dim()
names()
data[1:10,] # first 10 rows and all columns
data[1:10, 1:3] # first 10 rows and first 3 columns

 ?summary
summary() # min, max, median, mean, quartile
data = cbind(DRUG$Age, DRUG$Sex) #combine two columns
aggregate(DRUG$Na, by=list(DRUG$Sex), FUN= mean) #Check sodium level for males and females
hist()
plot(density(DRUG$Age))
table(DRUG$Age)
barplot(table(DRUG$Age), main ="Barplot", xlab = "X", ylab = "Y", col=c("blue", "orange"))
boxplot()
pie(table(DRUG$Age))
cor(DRUG$Na, DRUG$K)

# Detecting and removing Outliers 
hist(hmeq$CLAGE)
Newdata = subset(hmeq, CLAGE <= 500)
hist(Newdata$CLAGE)

outlier = boxplot(hmeq$CLAGE)$out
outlier
CLAGEnOut = ifelse(hmeq$CLAGE %in% outlier, NA, hmeq$CLAGE)
boxplot(CLAGEnOut)

top <- fivenum(emp_performance$accuracy)[4] 
topthreshold <- top+ IQR(emp_performance$accuracy)*1.5 
emp_performance <- emp_performance[emp_performance$accuracy<=topthreshold,]


#Handling missing values

#Checking missing values
is.na(hmeq$NINQ)
sum(is.na(hmeq$NINQ)) # number of missing values
sum(complete.cases(hmeq$NINQ)) # number of complete values
sum(!complete.cases(hmeq$NINQ)) # number of missing values
which(!complete.cases(hmeq$NINQ)) # which row has the missing value

# Replacing missing value
hmeq$NINQ[is.na(hmeq$NINQ)] = mean(hmeq$NINQ, na.rm = TRUE) # median, mode
sum(is.na(hmeq$NINQ)) # check number of missing values again

CLAGE_Impute = na.omit(hmeq$CLAGE) # large dataset and afford to loose records
CLAGE_Impute
sum(is.na(CLAGE_Impute))

# It is a good practice not to replace the categorical variables, if yes, replace with its mode. 

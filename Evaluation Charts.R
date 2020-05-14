install.packages("ROCR")
# Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

library(ROCR)
?ROCR.simple
data(ROCR.simple)
# Calculating the values for ROC curve
pred = prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf = performance(pred,"tpr","fpr")

# Plotting the ROC curve
plot(perf, colorize = TRUE, lwd = 3)

# Calculating AUC
auc = performance(pred, "auc")
# Now converting S4 class to a vector
auc = unlist(slot(auc, "y.values"))
auc

# Adding min and max ROC AUC to the center of the plot
minauc = min(round(auc, digits = 2))
maxauc = max(round(auc, digits = 2))
minauct = paste(c("min(AUC) = "), minauc, sep = "")
maxauct = paste(c("max(AUC) = "), maxauc, sep = "")
legend(0.7, 0.5, c(minauct, maxauct, "\n"), border = "white", cex = 0.7, box.col = "white")
abline(a= 0, b=1)


opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}
print(opt.cut(perf, pred))

# Find minimum costs
cost.perf = performance(pred, "cost")
pred@cutoffs[[1]] [which.min(cost.perf@y.values[[1]])]

# Cost for FP and FN
cost.perf = performance(pred, "cost", cost.fp = 2, cost.fn = 1)
pred@cutoffs[[1]] [which.min(cost.perf@y.values[[1]])]

#------------------------------------------------------------------------------------------------------

# GAIN CHART

library(ROCR)
data(ROCR.simple)
pred = prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf = performance(pred,"tpr", "rpp")
plot(perf)

# Adding base line and ideal line to the graph
plot(x=c(0, 1),y=c(0, 1),type="l",col="red",lwd=2,ylab="True Positive Rate", xlab="Rate of
Positive Predictions")
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)
gain.x = unlist(slot(perf, 'x.values'))
gain.y = unlist(slot(perf, 'y.values'))
lines(x=gain.x, y=gain.y, col="orange", lwd=2)


#------------------------------------------------------------------------------------------------------

# LIFT CHART

perf = performance(pred,"lift","rpp")
plot(perf, main="lift curve")

install.packages("lift")
library(lift)
plotLift(ROCR.simple$predictions, ROCR.simple$labels, n.buckets = 100)

#------------------------------------------------------------------------------------------------------

# Response Chart
perf_Response <- performance(pred, "ppv", "rpp")
plot(perf_Response)

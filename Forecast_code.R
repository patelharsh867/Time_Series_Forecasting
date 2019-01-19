setwd("C:/Users/HARSH/Desktop/Forecasting")
library("TTR")
library("forecast")
library("nortest")
library("caret")
library("tseries")
library("urca")
library("e1071")

data <- read.csv("hpatel8.csv")
train<- data[1:1500,]
test<- data[1501:2000,]

#-------------Removing Stationarity---------#

x <- c()

for (i in 1:1999 ){
  x[i] = data[i+1,] - data[i,]

}

matplot(c(1:2000), data[1:2000,], pch=c(0,1), col = c("blue"), main=paste("Original Time Series"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
matplot(c(1:1999), x[1:1999],col = c("green"), main=paste("Time Series after Differencing"), ylab = "Values", xlab = "Index", type = "l")

print(adf.test(x, alternative = "stationary"))

#-------Simple Moving average----#

n<-1500
rmse<-c()
i <- 1
predicted<-c()
for (k in 1:50) {
  ra<-c((k+1) : 1500)
  for (j in ra) {
    l<-j-k
    r<-j-1
    predicted[j]<-sum(train[l:r])/k
  }
  error<-(train[(k+1):1500]-predicted[(k+1):1500])
  sse<-sum(error^2)
  rmse[i]<- sqrt(sse/n)
  i<-i+1
}
plot(c(1:50),rmse,  xlab = "k", main = "RMSE vs k", ylab = "RMSE")
print("Rmse for SMA")
print(rmse)
print("minimum k for RMSE is")
print(2)

k <- 2
ra<-c((k+1) : 1500)
for (j in ra) {
  l<-j-k
  r<-j-1
  predicted[j]<-sum(train[l:r])/k
}

matplot(c(1:1500), cbind(train[1:1500],predicted[1:1500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Original and Predicted values for k = ", 2), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex = 0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)


#-------Task 3 --------#

a_values<-seq(0.1,0.9,0.1)
rmse_e<-c()

ctr<- 1
for (i in a_values) {
  a<-i
  predicted <- c()
  predicted[1] <- train[1]
  for (j in 2:1500) {
    predicted[j] <- (a* train[j-1]) + ((1-a)* predicted[j-1])
  }
  error<-(train[2:1500] - predicted[2:1500])
  sse<-sum(error^2)
  rmse_e[ctr]<-sqrt(sse/1500)
  ctr<-ctr+1
}

plot(seq(0.1,0.9,0.1), rmse_e[1:9], xlab="a", ylab = "RMSE", main = "RMSE vs a")
print("RMSE for exponential")
print(rmse_e)
print("value of a where RMSE is minimum")
print(0.1)

predicted<-c()
a=0.1
predicted[1]<-train[1]
for (j in 2:1500) {
  predicted[j] <- (a* train[j-1]) + ((1-a)* predicted[j-1])
}

matplot(c(1:1500), cbind(train[1:1500],predicted[1:1500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Original and Predicted values for alpha = ", 0.54), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex=0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

#--------Task 4 ---------------------------#

rmse_ar <- c()
pacf(train, main = "PACF")
for (i in 1:10) {
  model<-ar(train, aic = FALSE, order.max = i)
  error<-residuals(model)[(i+1):1500]
  sse<-sum(error^2)
  rmse_ar[i]<-sqrt(sse/1500)
}
print(rmse_ar)
plot(c(1:10), rmse_ar, xlab = "p", ylab = "RMSE", main = "RMSE vs p")
print("From the pacf p :")
print(2)
model<-ar(train, aic = FALSE, order.max = 2)
print(model)

matplot(c(1:1500), cbind(train[1:1500], fitted(model)[1:1500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Ar(p) : Original and Predicted values for p = ", 2), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex=0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

error<-residuals(model)[(2+1):1500]
qqnorm(error, main = expression('Q-Q plot for Residuals in model'))
hist(error, main = expression('Histogram for Residuals in model'))
plot(error)
print(pearson.test(error))
sse<-sum(error^2)
rmse_re<-sqrt(sse/1500)
print("RMSE is ", rmse_re)

#-------Task 5 ------#

#SMA
k<-2
ra<-c((k+1) : 500)
tpredicted<-c(1:500)
for (j in ra) {
  l<-j-k
  r<-j-1
  tpredicted[j]<-sum(test[l:r])/k
}
matplot(c(1:500), cbind(test[1:500],tpredicted[1:500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("SMA : Original and Predicted values for k = ", 2, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex=0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)
 matplot(c(100:200), cbind(test[100:200],tpredicted[100:200]), pch=c(0,1), col = c("Red", "Blue"), main=paste("SMA : Original and Predicted values for k = ", 2, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
 legend("topleft", lty = c(1, 1), cex=0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

#Exponential
tpredicted<-c()
a=0.1
tpredicted[1]<-test[1]
for (j in 2:500) {
  tpredicted[j] <- (a* test[j-1]) + ((1-a)* tpredicted[j-1])
}
matplot(c(1:500), cbind(test[1:500],tpredicted[1:500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Expo. : Original and Predicted values for a = ", 0.1, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex = 0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

matplot(c(100:200), cbind(test[100:200],tpredicted[100:200]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Expo. : Original and Predicted values for a = ", 0.1, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex = 0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)



#Ar(p)
model<-ar(test, aic = FALSE, order.max = 2)

matplot(c(1:500), cbind(test[1:500], fitted(model)), pch=c(0,1), col = c("Red", "Blue"), main=paste("Ar(p) : Original and Predicted values for p = ", 2, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex = 0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

matplot(c(100:200), cbind(test[100:200], fitted(model)[100:200]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Ar(p) : Original and Predicted values for p = ", 2, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), cex = 0.8, col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

terror<-residuals(model)[(2+1):500]
sse<-sum(terror^2)
rmse_ar<-sqrt(sse/1500)
print(paste("RMSE is ", rmse_ar))

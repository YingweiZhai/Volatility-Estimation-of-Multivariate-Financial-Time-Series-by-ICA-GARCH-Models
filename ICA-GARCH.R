#Fit Vector Autoregressive models VAR(p) models
library(vars)
library(astsa)
data1<-read.csv("/Users/KillMe/Desktop/In Sample Data.csv")
in_sample<-ts(na.omit(data1)) #Make the log-returns differ more
library(Jmisc)
in_sample1<-demean(in_sample)*100

#Plot the dailty returns
plot.ts(in_sample1, main = "", xlab = "")
#Fit the ideal Vector Autoregressive Model
fitvar1<-VAR(in_sample1,type = "none",lag.max = 4, ic = c("AIC"))
fitvar1$p #specifying the lag order
summary(fitvar1)
fitvar1$varresult

res<-residuals(fitvar1)
par(mfrow = c(4, 1))
plot(res[,1],type="l")
plot(res[,2],type="l")
plot(res[,3],type="l")
plot(res[,4],type="l")
plot(res[,5],type="l")
plot(res[,6],type="l")
plot(res[,7],type="l")
plot(res[,8],type="l")
acf(res)

#Use ICA to decompose the residual
library(moments)
kurtosis(res)
library(ica)
X1<-icafast(res,8,center=T)
X2<-X1$S
range(X2[,1])
plot(X2[,1],type="l")
plot(X1$S, main = "ICA components")

##Or use another package
library(fastICA)
Y1<-fastICA(res, 8, alg.typ = "deflation",fun = "logcosh", alpha = 1, method = "R", row.norm = FALSE, maxit = 2000, tol = 0.0001, verbose = TRUE)
Y2<-Y1$S
plot(Y1$X, main = "Pre-processed data")
plot(Y1$X %*% Y1$K, main = "PCA components")
plot(Y1$S, main = "ICA components")

#Use GARCH(1,1) to fit the result
library(fGarch)
fit1 <- garchFit(formula = ~garch(1, 1), data = X2[,1],trace=F)
fit2 <- garchFit(formula = ~garch(1, 1), data = X2[,2],trace=F)
fit3 <- garchFit(formula = ~garch(1, 1), data = X2[,3],trace=F)
fit4 <- garchFit(formula = ~garch(1, 1), data = X2[,4],trace=F)
fit5 <- garchFit(formula = ~garch(1, 1), data = X2[,5],trace=F)
fit6 <- garchFit(formula = ~garch(1, 1), data = X2[,6],trace=F)
fit7 <- garchFit(formula = ~garch(1, 1), data = X2[,7],trace=F)
fit8 <- garchFit(formula = ~garch(1, 1), data = X2[,8],trace=F)
summary(fit5)
#Plot the volatility
plot(fit1@h.t,type="l", main = "", xlab = "", ylab = "")


#To fit the PCA-GARCH
X3<-X1$Y
range(X3[,1])
plot(X1$Y, main = "PCA components")
fit11 <- garchFit(formula = ~garch(1, 1), data = X3[,1],trace=F)
fit12 <- garchFit(formula = ~garch(1, 1), data = X3[,2],trace=F)
fit13 <- garchFit(formula = ~garch(1, 1), data = X3[,3],trace=F)
fit14 <- garchFit(formula = ~garch(1, 1), data = X3[,4],trace=F)
fit15 <- garchFit(formula = ~garch(1, 1), data = X3[,5],trace=F)
fit16 <- garchFit(formula = ~garch(1, 1), data = X3[,6],trace=F)
fit17 <- garchFit(formula = ~garch(1, 1), data = X3[,7],trace=F)
fit18 <- garchFit(formula = ~garch(1, 1), data = X3[,8],trace=F)
#Plot the volatility
plot(fit11@h.t,type="l", main = "", xlab = "", ylab = "")


#EWMA
library(MTS)
fit_ewma<-EWMAvol(Y2,lambda = 0.94)
volatility<-fit_ewma$Sigma.t
plot(volatility[,5],type="l", main = "", xlab = "", ylab = "")
plot(fit5@h.t,type="l", main = "", xlab = "", ylab = "")




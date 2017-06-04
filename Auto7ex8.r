#######################################################################################
#Exercise 7.8
######################################################################################

library(ISLR)
library(boot)
library(glmnet)

data(Auto)
attach(Auto)
summary(Auto)
set.seed(123)
pairs(Auto)

#Fit some of the non-linear models investigated in this chapter 
#to the Auto data set. 

#Answer: mpg seems like to be negatively correlated to cylinders, 
#displacement,horsepower and weight.

# We will first do the polynomial regression 

rss = rep(NA, 10)
fits = list()
for (i in 1:10) {
  fits[[i]] = lm(mpg ~ poly(displacement, i), data = Auto)
  rss[i] = deviance(fits[[i]])
}
rss
summary(rss)

anova(fits[[1]], fits[[2]], fits[[3]], fits[[4]])

#Training RSS decreases over time.

deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(deltas)
#[1] 10 - cross-validation selected a 10th-degree polynomial.
#Plot the graph
plot(1:15, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "blue", pch = 20)
#d=10 is the optimal degree for the polynomial.
deltas
summary(deltas)

#Step functions

cv.error = rep(NA, 10)
for (i in 2:10) {
  Auto$dis.cut = cut(Auto$displacement, i)
  fit = glm(mpg ~ dis.cut, data = Auto)
  cv.error[i] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.error)
#[1] 9
#Plot the graph
plot(2:10, cv.error[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
d.min <- which.min(cv.error)
points(which.min(cv.error), cv.error[which.min(cv.error)], col = "red", pch = 20)

cv.error
summary(cv.error)
#The error is minimum for 9 cuts. 

#Spline functions.
library(splines)

cv.error1 <- rep(NA, 10)
for (i in 3:10) {
  fit <- glm(mpg ~ ns(displacement, df = i), data = Auto)
  cv.error1[i] <- cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.error1)
#[1] 9
plot(3:10, cv.error1[-c(1, 2)], xlab = "Cuts", ylab = "Test MSE", type = "l")
d.min <- which.min(cv.error1)
points(which.min(cv.error1), cv.error1[which.min(cv.error1)], col = "red", pch = 20)

cv.error1
summary(cv.error1)

# The error is minimum for 9 degrees

#GAMs

library(gam)

fit.gam <- gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit.gam)

#######################################################################################
#Exercise 9.7
######################################################################################

#In this problem, you will use support vector approaches in order to 
#predict whether a given car gets high or low gas mileage 

library(ISLR)
data(Auto)
attach(Auto)
summary(Auto)

#(a) Create a binary variable that takes on a 1 for cars with gas mileage
#above the median, and a 0 for cars with gas mileage below the median.

bin.var = ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpglevel = as.factor(bin.var)

# (b).Fit a support vector classifier to the data with various values of 
#“cost”, in order to predict whether a car gets high of low gas mileage. 
#Report the cross-validation errors associated with different values of this parameter. 

library(e1071)
set.seed(3456)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", 
                ranges = list(cost = c(0.5, 1, 5, 10, 25, 50, 100)))
summary(tune.out)

# Cross-validation error is minimized for cost=1, seems to perform best

# (c). Now repeat (b), this time using SVMs with radial and polynomial basis kernels, 
#with different values of “gamma” and “degree” and “cost”. 

tune.out1 <- tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", 
           ranges = list(cost = c(0.1, 1, 5, 10, 25, 50, 100), degree = c(2, 3, 4)))

summary(tune.out1)

# lowest cross-validation error is obtained for cost = 100 and degree = 2

tune.out2 <- tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", 
            ranges = list(cost = c(0.1, 1, 5, 10), 
                    gamma = c(0.01, 0.1, 1, 5, 10, 100)))

summary(tune.out2)

# For radial lowest cross-validation error is obtained for cost = 10 and gamma = 0.01

# (d). Make some plots to back up your assertions in (b) and (c).

svm.linear <- svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.polynomial <- svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 100, degree = 2)
svm.radial <- svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 100, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm.linear)

plotpairs(svm.polynomial)

plotpairs(svm.radial)

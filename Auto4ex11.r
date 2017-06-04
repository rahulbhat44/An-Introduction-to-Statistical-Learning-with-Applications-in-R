#######################################################################################
#Exercise 4.11
######################################################################################


library(ISLR)
library(ggplot2)
library(gridExtra)
library(MASS)
library(class)
library(caTools)

#(a)Create a binary variable mpg01, that contains a 1 if mpg contains a value above 
#its median, and a 0 if mpg contains a value below its median. 

data(Auto)
summary(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto <- data.frame(Auto, mpg01)
Auto


#(b) (b) Explore the data graphically in order to investigate the association between 
#mpg01 and the other features.

cor(Auto[, -9])
pairs(Auto) 

boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

#Plots shows that there is some association between “mpg01” and “cylinders”, 
#“weight”, “displacement” and “horsepower”.

Auto = mutate(Auto, mpg01 = ifelse(mpg < median(mpg), 0, 1))
head(Auto)
table(Auto$mpg01)

# Changing colour of pairs plot   
cols <- character(nrow(Auto))
cols[] <- "black"

cols[Auto$mpg01 == 1] <- "black"
cols[Auto$mpg01 == 0] <- "red"
pairs(Auto, col=cols)

# Horsepower, weight displaement and acceleration seem like good variables to use as predictors. 
cor(Auto$horsepower, Auto$acceleration)

# Horsepower and acceleration seem to have a high correlation between themselve, 
#therefore may be a good idea to only use one of them.

plot(cylinders ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
plot(horsepower ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
plot(weight ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
plot(displacement ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
# The plots above were created with some noise around the the two points. 
# This helps us get a sense of how many data points lie on either 1 or 0, 

#(c) Split the data into a training set and a test set.
train = (year%%2 == 0)  # if the year is even
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = mpg01[test]

#(d) Perform LDA
fit.lda = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
fit.lda
summary(fit.lda)
plot(fit.lda)
pred.lda <- predict(fit.lda, Auto.test)
mean(pred.lda$class != mpg01.test)
#[1] 0.1263736

# The LDA model gives a 12.6% test error rate

#(e). Perform QDA on the training data in order to predict mpg01 using the variables
fit.qda = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
           subset = train)
fit.qda
summary(fit.qda)
pred.qda = predict(fit.qda, Auto.test)
mean(pred.qda$class != mpg01.test)
1 - (96+109)/(96+109+9+22)
#[1] 0.1318681
# The QDA model gives a 13.18% test error rate

#(f) Perform logistic regression on the training data in order to predict mpg01 

fit.glm = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
pred.glm = predict(fit.glm, Auto.test, type = "response")
glm1 = rep(0, length(pred.glm))
glm1[pred.glm > 0.5] = 1
mean(glm1 != mpg01.test)
# The Logistic Regression model gives a 12.08% test error rate

# (f).Perform KNN on the training data, with several values of K, in order to predict mpg01.

train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]
set.seed(123)
# KNN(k=1)
pred.knn = knn(train.X, test.X, train.mpg01, k = 1)
mean(pred.knn != mpg01.test)
#[1] 0.1538462

# KNN(k=30)
pred.knn1 = knn(train.X, test.X, train.mpg01, k = 30)
mean(pred.knn1 != mpg01.test)
#[1] 0.1373626

# KNN(k=50)
pred.knn2 = knn(train.X, test.X, train.mpg01, k = 50)
mean(pred.knn2 != mpg01.test)
#[1] 0.1428571

## KNN(k=100)
pred.knn3 = knn(train.X, test.X, train.mpg01, k = 100)
mean(pred.knn3 != mpg01.test)
#[1] 0.1428571

#k=1, 15.38% test error rate. 
#k=30, 13.73% test error rate. 
#k=50, 14.28% test error rate. 
#k=100, 14.28% test error rate. 
#In our case "K" of 30 seems to perform the best. So 30 nearest neighbors.

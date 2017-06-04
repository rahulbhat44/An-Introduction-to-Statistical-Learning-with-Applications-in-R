---
  title: "Untitled"
author: "Rahul Bhat"
date: "6/3/2017"
output: html_document
---

#Load data set and libraries

  
library(ISLR)
library(corrplot)
library(car)
library(ggplot2)
library(dplyr)

#load the data

data(Auto)

#######################################################################################
#Exercise 2.9
######################################################################################


# (a) part
#check the data

str(Auto)
dim(Auto)
summary(Auto)


# check the list rows of data that have missing values 
Auto[!complete.cases(Auto),] 
is.na(Auto) #check missing values
Auto <- na.omit(Auto)  #remove missing values
str(Auto)
#Answer - All variables has a numerical structure, hence are quantitative except variable "name" and "origin".

#(b) part
#What is the range of each quantitative predictor?

range(Auto$weight)
#[1] 1613 5140
range(Auto$acceleration)
#[1]  8.0 24.8
range(Auto$year)
#[1] 70 82
range(Auto$cylinders)
#[1] 3 8
range(Auto$mpg)
#[1]  9.0 46.6
range(Auto$displacement)
#[1]  68 455
range(Auto$horsepower)
#[1]  46 230

# (c) part
#What is the mean and standard deviation of each quantitative predictor?

#mean and sd of Auto$weight
mean(Auto$weight)
#2977.584
sd(Auto$weight)
#849.4026

#mean and sd of Auto$acceleration
mean(Auto$acceleration)
#15.54133
sd(Auto$acceleration)
#2.758864

#mean and sd of Auto$year
mean(Auto$year)
#75.97959
sd(Auto$year)
#3.683737

#mean and sd of Auto$cylinders
mean(Auto$cylinders)
#5.471939
sd(Auto$cylinders)
#1.705783

#mean and sd of Auto$mpg
mean(Auto$mpg)
#23.44592
sd(Auto$mpg)
#7.805007

# mean and sd of Auto$displacement
mean(Auto$displacement)
#194.412
sd(Auto$displacement)
#104.644

# mean and sd of Auto$horsepower
mean(Auto$horsepower)
#104.4694
sd(Auto$horsepower)
#38.49116

# (d) part
#remove 10th to 85 observations and then check mean and sd
#What is the range, mean, and standard deviation of each predictor 

Auto2 = Auto[-c(10:85),] #remove 10th to 85 observations

range(Auto2$cylinders)
#[1] 3 8
range(Auto2$mpg)
#[1] 11.0 46.6
range(Auto2$displacement)
#[1]  68 455
range(Auto2$horsepower)
#[1]  46 230
range(Auto2$weight)
#[1] 1649 4997
range(Auto2$acceleration)
#[1]  8.5 24.8
range(Auto2$year)
#[1] 70 82

# mean and sd after removing 10th to 85th observation

#mean and sd of Auto2$mpg
mean(Auto2$mpg)
#24.40443
sd(Auto2$mpg)
#7.867283

#mean and sd of Auto2$cylinders
mean(Auto2$cylinders)
#5.373418
sd(Auto2$cylinders)
#1.654179

#mean and sd of Aut02$displacement
mean(Auto2$displacement)
#187.2405
sd(Auto2$displacement)
#99.67837

#mean and sd of Auto2$horsepower
mean(Auto2$horsepower)
#100.7215
sd(Auto2$horsepower)
#35.70885

#mean and sd of Auto2$weight
mean(Auto2$weight)
#2935.972
sd(Auto2$weight)
#811.3002

#mean and sd of Auto2$accelertaion
mean(Auto2$acceleration)
#15.7269
sd(Auto2$acceleration)
#2.693721

#mean and sd of Auto2$year
mean(Auto2$year)
#77.14557
sd(Auto2$year)
#3.106217

# (e) part
#investigate the predictors graphically, using scatterplots or other tools of your choice.
#Create some plots highlighting the relationships among the predictors.

data("Auto")
str(Auto)
plot(Auto)


plot(Auto$mpg, Auto$weight) # Heavier weight correlates with lower mpg.

plot(Auto$mpg, Auto$cylinders) # More cylinders, less mpg.

plot(Auto$mpg, Auto$year) # Cars become more efficient over time.

plot(Auto$mpg, Auto$origin)

# plot shows that there is a positiveive correlation between horsepower and weight
# plot shows that there is a negative correlation between mpg and horsepower
# plot shows there is something interesting relationship between Year of manufacture, MPG and Horsepower

#-f)
pairs(Auto)
# Answer: Yes, displacement, weight, horsepower, year are all correlated with mpg. 
# These correlations suggest good predictors to use


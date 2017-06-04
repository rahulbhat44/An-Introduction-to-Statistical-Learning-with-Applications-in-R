#######################################################################################
#Exercise 3.9
######################################################################################


#(a). Produce a scatterplot matrix which includes all of the variables in the data set.
data(Auto)
pairs(Auto)


#(b) Compute the matrix of correlations between the variables using the function cor(). 
#You will need to exclude the name variable, which is qualitative.

correlation <- cor(subset(Auto, select=-name))
correlation

#(c) Use the lm() function to perform a multiple linear regression with mpg as the 
#response and all other variables except name as the predictors.

lm.fit = lm(mpg~.-name, data = Auto)
summary(lm.fit)

#(i). Is there a relationship between the predictors and the response?
# Answer: There is such a large F-statistic, this provides evidence of relationship 
#between predictor and the response

#(ii).Which predictors appear to have a statistically significant relationship to the response?
# Answer: Looking at the p-values associated with each predictor’s t-statistic. 
#There is a significant relationship with displacement, weight, year and origin

#(iii). What does the coefficient for the year variable suggest?
# The older the car (model year) the higher the mpg. 
#The regression coefficient for year, 0.7507, suggests that for every one year, 
#mpg increases by the coefficient.

#(d) Use the plot() function to produce diagnostic plots of the linear regression fit.

plot(lm.fit)
plot(predict(lm.fit), rstudent(lm.fit))
# We see some non-linearity with the Residuals vs Fitted plot
# There are outliers and leverage points, data values greater than 3, and point 14!

#(e) Use the * and : symbols to fit linear regression models with interaction effects. 

lm.fit1 = lm(mpg~cylinders*displacement+displacement*weight, data = Auto)
summary(lm.fit1)
# we can see that the interaction between displacement and weight is statistically 
#signifcant, while the interactiion between cylinders and displacement is not.

#(f) Try a few different transformations of the variables,
lm.fit2 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2), data=Auto)
summary(lm.fit2)
plot(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

plot(predict(lm.fit2), rstudent(lm.fit2))



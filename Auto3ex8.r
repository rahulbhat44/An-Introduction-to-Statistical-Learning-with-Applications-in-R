#######################################################################################
#Exercise 3.8
######################################################################################

#(a) part
#perform linear regression with mpg as the response and horsepower as the predictor.

data(Auto)
lm1 = lm(mpg ~ horsepower, data = Auto)
summary(lm1)

# (i). Is there a relationship between the predictor and the response?
# Answer: Yes

# (ii). How strong is the relationship between the predictor and the response?

# Answer: We have to use the mean of the response and the RSE to calculate the residual error 
#relative to the response. The mean of mpg is 23.4459. The RSE of the lm.fit is 4.906 
#which indicates a percentage error of 20.9248% and the R^2 of the lm.fit is 0.6059, 
#meaning 60.5948% of the variance in mpg is explained by horsepower.
#ii. Very strong relationship, p-value < 0.001

# (iii). Is the relationship between the predictor and the response positive or negative?
# Answer: Negative because more the horsepower less is the mpg fuel efficiency. 

# (iv). What is the predicted mpg associated with a horsepower of 98? 
#What are the associated 95 % confidence and prediction intervals?

#What is the predicted mpg associated with a horsepower of 98?
predict(lm1, data.frame(horsepower=c(98)))
# 1 
#24.46708 

#What are the associated 95 % confidence and prediction intervals?
predict(lm1, data.frame(horsepower=c(95)), interval = "confidence")
#fit     lwr      upr
#1 24.94061 24.4389 25.44232

predict(lm1, data.frame(horsepower=c(95)), interval = "prediction")
#fit      lwr      upr
#1 24.94061 15.28253 34.59869

#-b) Plot the response and the predictor.
plot(Auto$horsepower, Auto$mpg)
abline(lm1)

#-c) Use the plot() function to produce diagnostic plots of the least squares 
#regression fit.
par(mfrow=c(2,2))
plot(lm1)
# Residual vs Fitted, non-linearity


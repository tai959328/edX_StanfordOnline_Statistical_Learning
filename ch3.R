# install.packages("MASS")
# install.packages("ISLR")
library(MASS)
library(ISLR)


### Simple linear regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")


### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
  # if there is a curved pattern in the residual vs fitted plot, it indicates that our model
  # might benefit from addiing a polynomial term since the relationship my not be completely
  # linear (there exist some non-linearity).
  # in the scale-location plot, we are looking for changes in the variance of the mean of
  # the fit, but this could be impacted by non-linearity. 
fit4=update(fit3,~.-age-indus)
  # fit an updated model by keeping the same response and removing the age and indus vars
summary(fit4)



### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)
  # this syntax will add an interaction term between lstat and age in the model, in
  # addition to the main effects
summary(fit5)
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
  # the I() function (identity function) syntax is needed because the formula language
  # has a special purpose for the power symbol (^)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
  # can't use abline() anymore since that only works with a straightline fit.
  # pch = plotting character, i.e. symbol
fit7=lm(medv~poly(lstat,4))
  #this is a quicker syntax used to create polynomials, in this case up to the power of 4
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)
  # shows plotting characters
  # the cex paramater of 2 doubles the size of the characters
  

###Qualitative predictors
fix(Carseats)
  # creates pulls up a data editor in R, so you can look at and/or edit the data
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
  # syntax used to include all predictors and add specific interactions
summary(fit1)
  # the Income:Advertising interaction is significant while the Age:Price interaction is not
contrasts(Carseats$ShelveLoc)
  # shows how R will code the categorial (i.e. qualitative) variable as dummy variable(s)
  # when it is put in a linear model. In this case it's a 3 factor variable so it puts
  # in 2 dummy variables


###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  # the ... means these are unnamed arguments that would be passed into the function exactly
  # as they have been supplied. This allows us to easily pass in parameters for the plot
  # function in order to make it visually more appealing
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)





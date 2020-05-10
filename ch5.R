library(ISLR)
library(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)
plot(Auto$horsepower, Auto$mpg)
dim(Auto)

## LOOCV
glm.fit=glm(mpg~horsepower, data=Auto)
summary(glm.fit)
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)
  # first number is the raw leave-one-out cross-validation result
  # second number is the bias-corrected version of the result. This bias correction
  # has to do with the fact that the data set that we train on is slightly smaller
  # than the one we actually would like to get an error for, which is the full data
  # set of size n. This bias correction is more pertinent for k-fold CV than LOOCV

##Lets write a simple function to use formula (5.2), shortcut for LOOCV prediction error
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)


cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")
  # the results are not much different from LOOCV errors, meaning 10-fold CV can provide
  # us with a computationally cheaper alternative to compute prediction error




## Bootstrap: think of it as a handy way of getting the distribution (i.e. standard error)
## of complex statistics that are hard to develop theoretical versions of 
## Minimum risk investment - Section 5.2
dim(Portfolio)
pairs(Portfolio)

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
  # this function will return the last line that was evaluated
alpha(Portfolio$X,Portfolio$Y)

## What is the standard error of alpha?

alpha.fn=function(data, index){
  with(data[index,],alpha(X,Y))
    # with function takes the data (indexed data in this case) and applies the alpha
    # function
}


alpha.fn(Portfolio,1:100)
  # computes alpha based on all 100 observations

set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))
  # sampling 100 observations with replacement from Portfolio dataframe

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)
  # plot on the right helps determine if distribution is normal. If the points on the
  # plot are mostly along the 45 degree line of the plot, then the distrbution is normal

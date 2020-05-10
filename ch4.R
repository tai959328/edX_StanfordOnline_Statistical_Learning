library(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)
  #specifying col parameter gives color coding of the classes in the Direction variable
table(Smarket$Direction)
dim(Smarket)


# Logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
  # this creates a confusion matrix of predicted vs actual classes
mean(glm.pred==Direction)


# Make training and test set
train = Year<2005
table(train)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)
summary(glm.fit)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
  # there is evidence of overfitting since we have a misclassifcation rate > 50% 


#Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)
summary(glm.fit)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
  # This smaller model actually performs better than the larger, previous logistic reg model
106/(76+106)
  # false positive (i.e. "up") rate




library(MASS)

## Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)
  # correct classification rate = 56%



## K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=5)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=10)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=15)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

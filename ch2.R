### vectors, data, matrices, subsetting
x=c(2,7,5)
x
y=seq(from=4,length=3,by=3)
?seq
y
x+y
x/y
x^y
x[2]
x[2:3]
x[-2] # remove 2nd
x[-c(1,2)] # remove 1st and second
z=matrix(seq(1,12),4,3)
z
z[3:4,2:3]
z[,2:3]
z[,1]
z[,1,drop=FALSE]
dim(z)
ls()
rm(y)
ls()
### Generating random data, graphics
x=runif(50)
y=rnorm(50)
plot(x,y)
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue")
par(mfrow=c(2,1)) # multiple plots, 2 rows, 1 column of plots
plot(x,y)
hist(y)
par(mfrow=c(1,1))
### Reading in data
getwd() # use setwd() function to change working directory
setwd("C:/Users/959/Desktop/edX_StanfordOnline_Statistical_Learning/Data_and_Code")
getwd()
Auto=read.csv("Auto.csv")
names(Auto)
dim(Auto)
class(Auto)
summary(Auto)
plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)
attach(Auto) # allows us to call variables directly without referencing their source table using $
search()
plot(cylinders,mpg)
cylinders=as.factor(cylinders)
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
pdf(file="mpg.pdf")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()
pairs(Auto,col="brown") # create matrix of plots
pairs(mpg~cylinders+acceleration+weight,Auto)
q()

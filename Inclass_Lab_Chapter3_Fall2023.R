
# install.packages("ISLR2")#textbook companion library
# install.packages("MPV")#For PRESS function
# install.packages("car")#For VIF function
# install.packages("FNN")#for knn
# install.packages("MASS")
# install.packages("caret")#Classification and Regression Training



library(ISLR2)
library(MASS)

library(MPV) #For PRESS function
library(car)#For vif function
library(FNN)



#data() #Data sets in package ‘datasets’

#library(help = "datasets") #The R Datasets Package
#library(help = "ISLR2")
#data(package = .packages(all.available = TRUE)) #Datasets in all available Packages

################################
#Boston Dataset available in package ISLR2
###############################

?Boston

boston.dt=Boston

boston.dt=Boston
boston.dt
#Explore dataset properties
dim(boston.dt)
names(boston.dt)

class(boston.dt)
str(boston.dt)

?head
head(boston.dt)
tail(boston.dt)
summary(boston.dt)
summary(boston.dt)[,2]

apply(boston.dt,MARGIN=2,FUN='sd')

?apply

#Convert variable types from quantitative(numeric) to categorical(factor)

boston.dt$chas

is.factor(boston.dt$chas)
is.numeric(boston.dt$chas)

as.factor(boston.dt$chas)


boston.dt$chas=as.factor(boston.dt$chas)



is.factor(boston.dt$rad)

boston.dt$rad=as.factor(boston.dt$rad)
boston.dt$rad
summary(boston.dt)

#plot data
plot(boston.dt)

par(mfrow=c(1,1))#One graph

plot(lstat,medv)#Error in plot(lstat, medv) : object 'lstat' not found

plot(boston.dt$lstat,boston.dt$medv)


attach(boston.dt)
lstat
plot(lstat,medv)
detach(boston.dt)

plot(boston.dt$lstat, boston.dt$medv)

par(mfrow=c(1,2))#two graphs

plot(medv~lstat+crim,boston.dt)

#Simple linear Regression Medv~lstat


boston.lm=lm(medv~lstat,data=boston.dt)

boston.lm

boston.lm=lm(medv~lstat,boston.dt)

ls()

?lm


boston.lm
str(boston.lm)
class(boston.lm)
summary(boston.lm)


names(boston.lm)
boston.lm$coefficients

par(mfrow=c(1,1))

plot(boston.lm)





par(mfrow=c(2,2))

plot(boston.lm)

#quadratique regression model medv~lstat+lstat^2

boston.qad=lm(medv~lstat+I(lstat^2),boston.dt)

summary(boston.qad)

par(mfrow=c(2,2))

plot(boston.qad)

#multiple regression model medv~ all predictors
boston.lm.all=lm(medv~.+I(lstat^2)-indus ,boston.dt)

summary(boston.lm.all)

par(mfrow=c(2,2))

plot(boston.lm.all)

attach(boston.dt)
contrasts(boston.dt$chas)
contrasts(boston.dt$rad)

?contrasts

#explore residuals
summary(boston.lm)
names(boston.lm)

boston.lm$residuals

par(mfrow=c(1,1))
hist(boston.lm$residuals)

#Computing R2_all variables for full model

sse.all=sum(boston.lm.all$residuals^2)

sse.all

sst=sum((boston.dt$medv-mean(boston.dt$medv))^2)
sst
r2.all=1-sse.all/sst
r2.all


#Computing R2_lstat
sse=sum(boston.lm$residuals^2)
sse
sst=sum((boston.dt$medv-mean(boston.dt$medv))^2)
sst
r2=1-sse/sst
r2

#Using the reduced model(lstat) for prediction


predict(boston.lm,boston.dt[1,])
predict(boston.lm,boston.dt)


boston.lm

data.frame(lstat=5)
predict(boston.lm,data.frame(lstat=5))

data.frame(lstat=5)

predict(boston.lm, 
        data.frame(lstat=c(5,10,15)))

#Alternative for predicting training data
str(boston.lm)
boston.lm$fitted.values

fitted(boston.lm)


#Ploting the data and the regression line
par(mfrow=c(1,1))
plot(boston.dt$lstat, boston.dt$medv)

points(boston.dt$lstat,boston.lm$fitted.values
       ,col="blue")

abline(boston.lm, col='red')


#Ploting the data and the regression line
attach(boston.dt)
par(mfrow=c(1,1))
plot(lstat, medv)

points(lstat,boston.lm$fitted.values ,col="blue")

abline(boston.lm)
detach(boston.dt)

#Adding interaction terms to the multiple regression model

par(mfrow=c(1,3))

plot(medv~lstat+zn+rm, boston.dt) #3 graphs 


boston.lm.inter=lm(medv~lstat+zn+rm+lstat:zn,data= boston.dt)

summary(boston.lm.inter)


#All quantitative variables
names(boston.dt)
names(boston.dt[,-c(3,4,7,9,10)])

boston.lm.quant=lm(medv~.,boston.dt[,-c(3,4,7,9,10)])


boston.lm.all=lm(medv~.,boston.dt)
summary(boston.lm.all)

#Variance Inflation Factor
vif(boston.lm.all)
boston.lm.all.red=lm(medv~.-rad,boston.dt)
vif(boston.lm.all.red)

vif(boston.qad)

#PRESS lm

summary(boston.lm.all)

press.boston.lm=PRESS(boston.lm.all)
press.boston.lm

summary(boston.lm.all)
1-press.boston.lm/sst

#Qad model
press.boston.qad=PRESS(boston.qad)
press.boston.qad
summary(boston.qad)
1-press.boston.qad/sst



#Polynomial model
boston.poly.4=lm(medv~poly(lstat,4),boston.dt)
summary(boston.poly.4)
boston.poly.4
par(mfrow=c(1,1))
plot(boston.dt$lstat, boston.dt$medv)
points(boston.dt$lstat, boston.poly.4$fitted.values,col="green")

press.boston.poly.4=PRESS(boston.poly.4)
press.boston.poly.4
summary(boston.poly.4)
1-press.boston.poly.4/sst


##knn regression
attach(Boston)

knn.bos=knn.reg(lstat, y=medv, k=10)

knn.bos

summary(knn.bos)

knn.bos$R2Pred

Rsquare=rep(0,20)
Rsquare

Press.vec=rep(0,20)
Press.vec

for (i in 1:20)
{Rsquare[i]=knn.reg(lstat, y=medv, k=i)$R2Pred

Press.vec[i]=knn.reg(lstat, y=medv, k=i)$PRESS
}

par(mfrow=c(1,1))
plot(1:20,Rsquare)
plot(1:20,Press.vec)



knn.bos.pred=
  knn.reg(lstat, data.frame(lstat=(c(5,10,15))), 
          y=medv, k=5)

?knn.reg

knn.bos.pred


#Train versus testing performance
?sample
dim(boston.dt)
head(boston.dt)

set.seed(100)
?sample

sample(1:10,3)

set.seed(100)
index.train=sample(1:506,400)
index.train

boston.train=boston.dt[index.train,]
boston.test=boston.dt[-index.train,]

dim(boston.dt)
dim(boston.train)
dim(boston.test)

names(boston.dt)
boston.train.lm=lm(medv~.-black,data=boston.train[,-3])
summary(boston.train.lm)

predict(boston.train.lm,boston.test)-boston.test$medv

sum((predict(boston.train.lm,boston.test)-boston.test$medv)^2)

sse.test=sum((predict(boston.train.lm,boston.test)-boston.test$medv)^2)
sse.test

sst.test=sum((boston.test$medv-mean(boston.test$medv))^2)

1-sse.test/sst.test







#changer repertoire courant pour faciliter l'echange de données
setwd("~/My Drive/EMI GDrive/
      EMI Academique/EMI 2022-2023/BData 3eme 2022-2023/
      Lectures2022-2023/Chapter 3_Linear_Regression")



#Auto Dataset
#import text dataset
auto.dt=read.table("auto.data",header=T)

summary(auto.dt)
names(auto.dt)

#import csv dataset
auto.dt.csv=read.csv("auto.csv")
names(auto.dt.csv)

?read.csv

?Auto




## Qualitative Predictors

###
head(Carseats)
summary(Carseats)
###
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
             data = Carseats)
summary(lm.fit)
###
attach(Carseats)
contrasts(ShelveLoc)

## Writing  Functions

###
LoadLibraries
LoadLibraries()
###
LoadLibraries <- function() {
  library(ISLR2)
  library(MASS)
  print("The libraries have been loaded.")
}
###
LoadLibraries
###
LoadLibraries()
###



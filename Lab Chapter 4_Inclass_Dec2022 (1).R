# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data

library(ISLR2)
?Smarket
class(Smarket)
names(Smarket)
dim(Smarket)
names(Smarket)
head(Smarket)
Smarket$Direction
summary(Smarket)
head(Smarket)
plot(Smarket$Today,type='l')

pairs(Smarket,col=Smarket$Direction)


cor(Smarket[,-9])


attach(Smarket)
plot(Smarket$Volume,type='l')

# Logistic Regression


glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)

print(glm.fit)
summary(glm.fit)

names(glm.fit)
glm.fit$coefficients
glm.fit$residuals

coef(glm.fit)
residuals(glm.fit)

sum(residuals(glm.fit)^2)
deviance(glm.fit) #Same as SSE


summary(glm.fit)$coef
summary(glm.fit)$coef[,4]


glm.probs=predict(glm.fit,type="response")

glm.probs[1:6]

predict(glm.fit,type="link")[1:10]

log(glm.probs/(1-glm.probs))[1:10]


glm.logit=predict(glm.fit,type="link")[1:10]

?predict.glm

glm.probs[1:10]
contrasts(Smarket$Direction)

#First approach to move from probs to classes

glm.pred=ifelse(glm.probs>0.5,"Up","Down")

glm.pred[1:10]

#Another approach to move from probs to classes
glm.pred=rep("Down",1250)
glm.pred

glm.pred[glm.probs>.5]="Up"

glm.pred

#Confusion matrix
table(Smarket$Direction,glm.pred)

prop.table(table(glm.pred,Smarket$Direction),2)

prop.table(table(Smarket$Direction,glm.pred),1)
  
glm.pred==Smarket$Direction

sum(glm.pred==Smarket$Direction)
mean(glm.pred==Smarket$Direction)

summary(Smarket$Direction)

attach(Smarket)

summary(as.factor(Year))
names(Smarket)
train=(Year<2005)
train

train.data=Smarket[train,]

dim(train.data)

Smarket.2005=Smarket[!train,]

dim(Smarket.2005)

Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial,subset=train)

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=train.data,family=binomial)

summary(glm.fit)


glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.probs
glm.pred=ifelse(glm.probs>0.5,"Up","Down")

# glm.pred=rep("Down",252)
# glm.pred
# glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)


#reduced model
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,
            subset=train)
glm.fit
glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.train.pred=predict(glm.fit,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

predict(glm.fit,newdata=data.frame(Lag1=1.2,Lag2=1.1),type="response")

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),
        type="response")



##Part II

# Linear Discriminant Analysis


library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,
            data=train.data)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
lda.class
lda.pred$posterior
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

plot(lda.fit)


# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,
            subset=train)

qda.fit


names(qda.fit)

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)


attach(Smarket)

# K-Nearest Neighbors
length(train)
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
?knn
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)



######

library(MASS)

#library(klaR)

iris


#mydata=iris

mydata=iris
names(mydata)
summary(iris)


#attach(mydata)
#mydata$cyl=as.factor(mydata$cyl)

dim(mydata)
names(mydata)

names(mydata)=c("sl","sw","pl","pw","class")

summary(mydata)

# Scatterplot for 3 Group Problem
pairs(mydata[c("sl","sw","pl","pw")], main="iris lda ", 
      pch=22, bg=c("red", "yellow", "blue")[unclass(mydata$class)])


set.seed(100)
train=sample(1:150,100)
data.train=mydata[train,]
summary(data.train)
data.test=mydata[-train,]

# Linear Discriminant Analysis 
fit.lda <- lda(class ~ sl+sw+pl+pw, data=data.train, na.action="na.omit")

fit.lda <- lda(class ~ sl+sw+pl+pw, data=mydata ,
               CV = TRUE)
fit.lda # show results 
?lda


plot(fit.lda)

plot(fit.lda, dimen=1, type="both") # fit from lda 


# The code above performs an LDA. 
# The code below assesses the accuracy of the prediction.

fit.pred.train=predict(fit.lda, data.train)

names(fit.pred.train)

#Erreur train
fit.class.train=fit.pred.train$class
fit.class.train

table(fit.class.train,data.train$class)
mean(fit.class.train==data.train$class)

#Erreur test
fit.pred.test=predict(fit.lda, data.test)
fit.class.test=fit.pred.test$class

table(fit.class.test,data.test$class)
mean(fit.class.test==data.test$class)

fit=lda(class ~ sl+sw+pl+pw, data=na.omit(mydata))
fit.pred=predict(fit.lda, mydata)

names(fit.pred)

#Erreur 
fit.class=fit.pred$class
fit.class

table(fit.class,mydata$class)
mean(fit.class==mydata$class)


# Quadratic Discriminant Analysis with 3 groups applying
# resubstitution prediction and equal prior probabilities.
#library(MASS)
fit2 <- qda(class ~ sl+sw+pl+pw, data=na.omit(mydata))
#, prior=c(1,1,1)/3)

# Re-subsitution (using the same data to derive the functions and evaluate their prediction accuracy) 
# is the default method unless CV=TRUE is specified. 
# Re-substitution will be overly optimistic. 
fit2 

fit2.pred=predict(fit2, mydata)

names(fit2.pred)

#Erreur 
fit2.class=fit2.pred$class
fit2.class

table(fit2.class,mydata$class)
mean(fit2.class==mydata$class)




# Exploratory Graph for LDA or QDA
library(klaR)
partimat(class ~ sl+sw+pl+pw,data=mydata,method="lda") 

partimat(class ~ sl+sw+pl+pw,data=mydata,method="qda") 



# Linear Discriminant Analysis with Jacknifed Prediction
library(MASS)
fit <- lda(class ~ sl+sw+pl+pw, data=mydata,
           na.action="na.omit", CV=TRUE)
summary(fit)
names(fit) # show results 

fit$xlevels
# The code above performs an LDA, using listwise deletion of missing data. 
# CV=TRUE generates jacknifed (i.e., leave one out) predictions. 
# The code below assesses the accuracy of the prediction.


# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(mydata$class, fit$class)
ct
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))


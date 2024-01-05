#Tree methods with iris data
library(rpart)

iris.dt=read.table("iris_text.txt",header=T,na.strings='?')
dim(iris.dt)
summary(iris.dt)


iris.rpart=rpart(Species~.,iris.dt)
par(xpd=T)
plot(iris.rpart)
text(iris.rpart,pretty=0,use.n=T)

iris.pred=predict(iris.rpart,iris.dt,type='class')

table(iris.dt$Species,iris.pred)

sum(iris.pred!=iris.dt$Species)/dim(iris.dt)[1]


##Rpart for regression Hitters data from ISLR package
library(ISLR)
hitters.dt=Hitters

hitters.dt=na.omit(hitters.dt)
summary(hitters.dt)
dim(hitters.dt)
names(hitters.dt)

plot(hitters.dt)

plot(hitters.dt$Years,hitters.dt$Hits,col=hitters.dt$Salary,type='p')

set.seed(1)
train.index=sample(263,200)
train.index

hitters.dt.train=hitters.dt[train.index,]
hitters.dt.test=hitters.dt[-train.index,]

library(rpart)

hitters.rpart=rpart(Salary~.,data=hitters.dt.train)

par(xpd=T)
plot(hitters.rpart)

text(hitters.rpart,use.n=T,pretty=0)


#rpart.plot(hitters.rpart)


pred.train=predict(hitters.rpart,hitters.dt.train)

RSS.train=sum((pred.train-hitters.dt.train$Salary)^2)
Tss.train=sum((hitters.dt.train$Salary-mean(hitters.dt.train$Salary))^2)

Rsquare.train=1-RSS.train/Tss.train
Rsquare.train


pred.test=predict(hitters.rpart,hitters.dt.test)

RSS.test=sum((pred.test-hitters.dt.test$Salary)^2)
Tss.test=sum((hitters.dt.test$Salary-mean(hitters.dt.test$Salary))^2)

Rsquare.test=1-RSS.test/Tss.test
Rsquare.test

summary(hitters.rpart)


##read boston data
Boston=read.table('boston.txt',header=T)
dim(Boston)
names(Boston)
names(Boston)[14]='medv'


set.seed(100)
train=sample(506,400)
boston.train=Boston[train,]
boston.test=Boston[-train,]



# Bagging and Random Forests with boston data

library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,
                        importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test$medv)
abline(0,1)

mean((yhat.bag-boston.test$medv)^2)

bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test$medv)^2)

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test$medv)^2)
importance(rf.boston)

dd=transform(importance(rf.boston))

names(dd)
attach(dd)

dd[order(-X.IncMSE,-IncNodePurity),]


varImpPlot(rf.boston)


oob.err=double(13)
test.err=double(13)

for (i in 1:13){
  fit=randomForest(medv~.,data=Boston,subset=train,mtry=i,ntree=500)
  oob.err[i]=fit$mse[500]
  pred= predict(fit,newdata=Boston[-train,])
  test.err[i]=with(Boston[-train,],mean((pred-medv)^2))
  cat(i," ")
}
matplot(1:13,cbind(oob.err,test.err),pch=19,
        col=c("red","blue"), type='b',ylab="MSE",xlab="mtry")
legend("topright",legend=c("oob","test"),pch=19,col=c("red","blue"))

# Partial dependence of lstat and rm on cmedv
library(pdp)
partial(rf.boston, pred.var = "LSTAT", plot = TRUE, rug = TRUE)

grid.arrange(
  partial(rf.boston, pred.var = "LSTAT", plot = TRUE, rug = TRUE),
  partial(rf.boston, pred.var = "RM", plot = TRUE, rug = TRUE),
  partial(rf.boston, pred.var = c("LSTAT", "RM"), plot = TRUE, 
          chull = TRUE),
  ncol = 3
)


########################
# Boosting for regression with boston data

library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=10000,interaction.depth=4)

#boost.boston
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="RM")
plot(boost.boston,i="LSTAT")
par(mfrow=c(1,1))

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test$medv)^2)

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000,interaction.depth=4,
                 shrinkage=0.2,verbose=F)

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test$medv)^2)

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=10000,interaction.depth=4)

ntree=seq(from=100,to=10000,by=100)
predmat=predict(boost.boston,newdata=Boston[-train,],n.trees=ntree)
dim(predmat)

berr=with(Boston[-train,],apply((predmat-medv)^2,2,mean))
plot(ntree,berr,pch=19,xlab="ntree",ylab="MSE",main="boosting MSE")

abline(h=min(test.err),col='red')
abline(h=min(oob.err),col='green')


##Boosting with iris data 
data(iris)
library(gbm)
iris.mod <- gbm(Species ~ ., distribution="multinomial", 
                data=iris,
                n.trees=1000, shrinkage=0.01, cv.folds=5,
                verbose=FALSE)
gbm.perf(iris.mod)

iris.mod

summary(iris.mod)


##Boositng with pima data
library(pdp)

data (pima)  # load the pima housing data
summary(pima)

set.seed(102)  # for reproducibility
library(gbm)
pima$diabetes=ifelse(pima$diabetes=='neg',0,1)

summary(pima)

pima.gbm=gbm(diabetes~ .,data=pima, distribution="bernoulli", 
             n.trees=2000, 
             shrinkage=0.01, cv.folds=5, 
             interaction.depth = 2, verbose=F)


gbm.perf(pima.gbm)
summary(pima.gbm)

library(randomForest)
data("pima")
##random forest on pima
pima.rf <- randomForest(diabetes ~ ., data = pima, 
                        na.action = na.omit,importance=T)

# Partial dependence of positive test result on glucose (default logit scale)
partial(pima.rf, pred.var = c("glucose",'age'), 
        plot = TRUE, chull = TRUE,
        progress = "text")

varImpPlot(pima.rf)


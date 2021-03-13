##Front Matter

library(tidyverse)
library(caret)
library(rpart)
library(e1071)
library(Rborist)

##read data clean variable names, change response variable Y to a factor.

data<-read.csv("data.csv")
cdata<-data %>%
  setNames(paste0("X", seq_along(.) - 1)) %>%
  rename(Y = 1)
cdata$Y<-factor(cdata$Y)

##look at a summary and notice one variable with little variation.
summary(cdata)

##remove variables with no variation.
remove<-which(apply(cdata,2,sd)==0)
cdata<-cdata[,-remove]


##partition the data set
set.seed(1)
index<-createDataPartition(cdata$Y,times=1, p=.3, list=FALSE)
test_set<-cdata[index,]
train_set<-cdata[-index,]

##Since this is a classification problem we might try logistic regression naively 
##with one variable or with all variables. (clean up warnings)

fit<-train(Y~X1,method="glm",data=train_set,metric="Kappa")
prediction<-predict(fit,test_set)
confusionMatrix(prediction,test_set$Y)

fit<-train(Y~.,method="glm",data=train_set, metric="Kappa")
prediction<-predict(fit,test_set)
confusionMatrix(prediction,test_set$Y)

##With all features specificity is still very low.  We may be overtraining. 
##There are nearly 100 features to sort through.  Lets look at a few
##in realtion to bankruptcy. 


train_set %>% ggplot(aes(x=X1,y=X2,color=Y,size=X3))+geom_point(alpha=.5)
train_set %>% ggplot(aes(x=X4,y=X5,color=Y,size=X6))+geom_point(alpha=.5)
train_set %>% ggplot(aes(x=X7,y=X8,color=Y,size=X9))+geom_point(alpha=.5)
train_set %>% ggplot(aes(x=X10,y=X11,color=Y,size=X12))+geom_point(alpha=.5)
train_set %>% ggplot(aes(x=X13,y=X14,color=Y,size=X15))+geom_point(alpha=.5)
train_set %>% ggplot(aes(x=X16,y=X17,color=Y,size=X18))+geom_point(alpha=.2)

##We can see that there is high variation in some variables and little in others.
##We can also see that there is high correlation in some variables and little in others. 
##To avoid over-training we will use principle component analysis to select combinations of features 
##that are not correlated with each other and explain a high amount of variance.


PCA<-preProcess(train_set[,-1],method = "pca")
trainpc<-cbind(train_set$Y,predict(PCA,train_set[,-1]))
fit<-train(`train_set$Y`~.,method="glm",data=trainpc)

testpc<-predict(PCA,test_set[,-1])
prediction<-predict(fit,testpc)
confusionMatrix(prediction,test_set$Y)


## We can see that the specificity has risen significantly and so has kappa.
## We may be interested in the optimum number of principle components and can 
## Tune for that. 

tuner<-function(N){
  PCA<-preProcess(train_set[,-1],method = "pca",pcaComp = N,metric="Kappa")
  trainpc<-cbind(train_set$Y,predict(PCA,train_set[,-1]))
  fit<-train(`train_set$Y`~.,method="glm",data=trainpc)

  prediction<-predict(fit,trainpc)
  CM<-confusionMatrix(prediction,train_set$Y)
  CM$overall[[2]]
}

kappas<-sapply(1:94,tuner)

plot(kappas)

which.max(kappas)

##looks like 90 is the best number of components

PCA<-preProcess(train_set[,-1],method = "pca",pcaComp = 90)
testpc<-cbind(test_set$Y,predict(PCA,test_set[,-1]))
fit<-train(`test_set$Y`~.,method="glm",data=testpc)

prediction<-predict(fit,testpc)
confusionMatrix(prediction,test_set$Y)

##Kappa is much better.





##Next we turn to the random forest algorithm to see if we can do any better with a different 
##approach. 

##We begin by considering a tree.
levels(train_set$Y)<-c("NB","B")
levels(test_set$Y)<-c("NB","B")
set.seed(1)
fit <- train(Y~ ., method = "rpart",
             tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
             data = train_set,metric="Kappa")
plot(fit)

fit
plot(fit$finalModel,margin=.1)
text(fit$finalModel,cex=.75)

confusionMatrix(predict(fit, test_set), test_set$Y)

##We now want to use a random forest to improve our results.

fit <- train(Y~ ., method = "Rborist", data = train_set,
             tuneGrid=data.frame(predFixed=1:10,minNode=2),metric="Kappa")
confusionMatrix(predict(fit,test_set),test_set$Y)
plot(fit)
fit

##We are not getting very good results and so we might try preproccessing 
##using PCA keeping the components we found before

PCA<-preProcess(train_set[,-1],method = "pca",pcaComp = 90)
trainpc<-cbind(train_set$Y,predict(PCA,train_set[,-1]))
fit<-train(`train_set$Y`~.,method="Rborist",data=trainpc,
           tuneGrid=data.frame(predFixed=1:10,minNode=2),metric="Kappa")
fit
plot(fit)
testpc<-predict(PCA,test_set[,-1])
prediction<-predict(fit,testpc)
confusionMatrix(prediction,test_set$Y)

##Even using PCA and tuning over a number of variables the regression 
##model performs better than random forests.





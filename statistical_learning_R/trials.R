# examples taken from http://www-bcf.usc.edu/~gareth/ISL/All%20Labs.txt
setwd("~/git_data/stat_learning_R")

set.seed(1)
library(MASS)
library(ISLR)
library(dplyr)

# Linear Discriminant Analysis (ch 4)
train=(Smarket$Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Smarket$Direction[!train]

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005) # this measures the accuracy of the  prediction
sum(lda.pred$posterior[,1]>=.5) # .5 is the threshold for computing the accuracy
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)
# Quadratic linear analysis 
# I'll skip that for now

# K-Nearest Neighbors

library(class)
train.X=cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test.X=cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
train.Direction=Smarket$Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

#
library(psych)
library(ISLR)
library(neuralnet)
library(nnet)
library("pROC")

#Use the same setup and the same split of the data as we did in Module 2 and Assignment 5 and Assignment 6.
dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dat$quality <- I(dat$quality > 6) * 1
describe(dat)[,1:9]

set.seed(42)
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]
X_train <- train[,1:11]
X_test <- test[,1:11]
Y_train <- train[,12]
Y_test <- test[,12]

#dim(train)
#dim(test)

form1 <- formula(quality~.)

n1 <- nnet(form1, data=train, size=10, maxit=500, decay=0.001)

#Let’s check the predictions:
yhat.n1 <- predict(n1, test)
yhat.n1 <- (yhat.n1-min(yhat.n1))/(max(yhat.n1)-min(yhat.n1))
table(yhat.n1[,1]>0.5, test$quality)

n1.roc <- roc(test$quality, yhat.n1[,1], direction="<")
n1.roc

plot(n1.roc, lwd=3)
lines(t2.roc, lwd=3, col="blue")
lines(glm.roc, lwd=3, col="orange")
lines(abt1.roc, lwd=3, col="red")
legend("bottomright",title="ROC Curves",c("Tree", "glm", "boost", "neural"), fill=c("blue","orange","red","black"))

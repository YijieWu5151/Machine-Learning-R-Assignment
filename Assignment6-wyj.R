library(randomForest)
library(pROC)
library(gbm)

#Use the same setup and the same split of the data as we did in Module 2 and Assignment 5.
dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dat$quality <- I(dat$quality > 6) * 1
set.seed(42)
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]

xvars <- names(train)[1:11]
X <- as.matrix(train[,xvars])
Y <- factor(train$quality)

#Fit a random forest to the data
mtry <- round(ncol(X)**.5); mtry
ntree <- 1000
set.seed(42)
rf1 <- randomForest(x=X, y=Y, ntree=ntree, mtry=mtry, importance=TRUE)
rf1

summary(rf1)
importance(rf1)
varImpPlot(rf1)

pred.rf1 <- predict(rf1, test)
table(pred.rf1, test$quality)

yhat.rf1 <- predict(rf1, test, type="prob")[,2]
rf1.roc <- roc(test$quality, yhat.rf1, direction="<")
plot(rf1.roc, lwd=3)
rf1.roc

#Fit a boosted tree learner to the data (ada boost or gradient boost).
abt1 <- gbm(quality~., data = train, distribution = "adaboost", n.trees = 500, interaction.depth=6, shrinkage = 0.005)
summary(abt1)

#Determine the confusion matrix for a 50% threshold for the test set. 
yhat.abt1 <- predict(abt1,newdata = test, n.trees = 500, type="response")
table(yhat.abt1>0.5, test$quality)

#Produce an ROC curve using the test set for your classifiers.
abt1.roc <- roc(test$quality, yhat.abt1, direction="<")
abt1.roc
plot(rf1.roc, lwd=3)
lines(abt1.roc, col="red", lwd=3)

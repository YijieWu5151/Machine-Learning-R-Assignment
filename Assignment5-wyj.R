#PREDICT WINE QUALITY BASED ON PHYSIOCHEMICAL INFORMATION
#https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/

library(psych)

dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dim(dat)
describe(dat)[,1:9]
head(dat)

#########################################
#	Define high quality wines
#########################################

table(dat$quality)
dat$quality <- I(dat$quality > 6) * 1
describe(dat)[,1:9]

#########################################
#	Some exploration
#########################################

library(corrplot)
corrplot(cor(dat), method="number")

doPlot <- function(vvar){
  n <- xtabs(~dat[,vvar])
  x <- unique(dat[,vvar])
  x <- x[order(x)]
  y <- tapply(dat$quality, dat[,vvar], mean)
  plot(x,y, cex=(n/mean(n))^.5, col="navy", lwd=2
       , xlab=vvar, ylab="high quality", main=vvar, las=1)
  abline(h=mean(dat$quality), col="blue", lwd=2)
  abline(lm(y~x, weight=n), col="chartreuse3", lwd=2)
}


par(mfrow=c(1,1))
doPlot("fixed.acidity")
doPlot("volatile.acidity")
doPlot("citric.acid")
doPlot("residual.sugar")
doPlot("chlorides")
doPlot("free.sulfur.dioxide")
doPlot("total.sulfur.dioxide")
doPlot("density")
doPlot("pH")
doPlot("sulphates")
doPlot("alcohol")

#########################################
#	Split dataset
#########################################

set.seed(42)
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]
X_train <- train[,1:11]
X_test <- test[,1:11]
Y_train <- train[,12]
Y_test <- test[,12]

#########################################
#	logistic regression
#########################################

glm <- glm(quality ~ ., family="binomial", data=train)
summary(glm)
yhat_glm <- predict(glm, type="response")
table(train$quality, (yhat_glm >0.5))
test$yhat.glm <- predict(glm, test, type="response")
55/(97+55) #TRUE POSITIVE RATE, SENSITIVITY
TPR <- function(y,yhat)  { sum(y==1 & yhat==1) / sum(y==1) }
TPR(train$quality, (yhat_glm >0.5))
953/(953+31) #TRUE NEGATIVE RATE, SPECIFICITY
TNR <- function(y,yhat)  { sum(y==0 & yhat==0) / sum(y==0) }
TNR(train$quality, (yhat_glm >0.5))

table(test$quality, (test$yhat.glm > 0.5))
TPR(test$quality, (test$yhat.glm > 0.5))
TNR(test$quality, (test$yhat.glm > 0.5))

#########################################
# LDA/QDA
#########################################

library(MASS)
lda <- lda(quality ~ ., data=train)
yhat_lda <- predict(lda)$posterior[,2]
table(train$quality, (yhat_lda >0.5))
test$yhat.lda <- predict(lda, test)$posterior[,2]
table(test$quality, (test$yhat.lda > 0.5))

qda <- qda(quality ~ ., data=train)
yhat_qda <- predict(qda)$posterior[,2]
table(train$quality, (yhat_qda >0.5))
test$yhat.qda <- predict(qda, test)$posterior[,2]
table(test$quality, (test$yhat.qda > 0.5))
TPR(test$quality, (test$yhat.qda >0.5))
TNR(test$quality, (test$yhat.qda >0.5))

par(mfrow=c(1,1))
library(pROC)
glm.roc <- roc(test$quality, test$yhat.glm, direction="<")
glm.roc
lda.roc <- roc(test$quality, test$yhat.lda, direction="<")
lda.roc
qda.roc <- roc(test$quality, test$yhat.qda, direction="<")
qda.roc
plot(glm.roc, lwd=3)
lines(lda.roc, lwd=3, col = "yellow")
lines(qda.roc, lwd=3, col = "blue")
legend("bottomright",title="ROC Curves",c("glm","lda","qda"), fill=c("black","yellow","blue"))


#########################################
# fit a classification tree
#########################################

library(rpart)
form1 <- formula(quality~.)
t1 <- rpart(form1, data=train, cp=.001, method="class")
plot(t1,uniform=T,compress=T,margin=.05,branch=0.3)
text(t1, cex=.7, col="navy",use.n=TRUE)

plotcp(t1)

CP <- printcp(t1)
#According to the documentation, “a good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line”. 
cp <- CP[,1][CP[,2] == 2]
cp

#prune accordingly
t2 <- prune(t1,cp=cp[1])
plot(t2,uniform=T,compress=T,margin=.05,branch=0.3)
text(t2, cex=.7, col="navy",use.n=TRUE)

yhat.t2 <- predict(t2, test, type="prob")[,2]
table(yhat.t2>0.5,Y_test)

TPR <- function(y,yhat)  { sum(y==1 & yhat==1) / sum(yhat==1) }
TPR(yhat.t2>0.5,Y_test)

library("pROC")
t2.roc <- roc(Y_test, yhat.t2, direction="<")
t2.roc

plot(t2.roc, lwd=3, col="blue")
lines(glm.roc, lwd=3, col="orange")
legend("bottomright",title="ROC Curves",c("Tree", "glm"), fill=c("blue","orange"))

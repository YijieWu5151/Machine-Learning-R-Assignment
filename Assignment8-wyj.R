dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dim(dat)
dat[1:3,]
library(psych)
describe(dat)[,1:5]

#scale the data so as to make sure we are comparing apples to apples:
dat <- scale(dat)
dat <- as.data.frame(dat)
dat[1:2,]
describe(dat)[,1:4]

#Exploratory data analysis
library(corrplot)
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor(dat), tl.cex=.7, order="hclust", col=col3(50))
#As a heuristic to suggest the appropriate number of clusters, we evaluate how the within sum of squares varies by clusters:
ss <- function(x)  sum( ( x-mean(x) )^2 )
wss <- NULL
wss[1] <- sum( apply(dat,2,ss) )
for (k in 2:10) {
  temp <- kmeans(dat, k)
  wss[k] <- sum(temp$withinss)
}

barplot(wss, col="dodgerblue", names.arg=1:length(wss)
        , xlab="Number of Clusters (k)"
        , ylab="Total Within Sum of Squares")
abline(h=0)
title("Within Sum-of-Squares Analysis", col.main="navy")

###########################Clustering###########################
#One way to run the clustering algorithm that allows for a number of “knobs” is NbClust from the library NbClust (e.g., try NbClust(dat, min.nc=3, max.nc=6, method="kmeans")) but it takes a long time to run. So let’s use the more basic package again, where we set the number of clusters to 3:
 

###########################PCA###########################
pc1 <- prcomp(dat)
pc1 <- prcomp(scale(dat))
round(pc1$rotation[,1:2], 3)
#Let’s compute the PCs and check their correlation:
pcs <- predict(pc1) 
describe(pcs)[,1:5]

dim(pcs); dim(dat)
corrplot(cor(pcs))

vars <- apply(pcs, 2, var)
sum(vars); ncol(dat); ncol(pcs)

barplot(vars[1:10], col="lightblue", ylab="variance", las=1)
title("Principal Components Analysis Scree Plot", col.main="navy")
abline(h=1:7, col="darkcyan")
abline(h=0)

plot(pc1)   
summary(pc1)

biplot(pc1, col=c("slategrey", "navy"), cex=c(.2, .8))
round(pc1$rotation, 4)[,1:2]

#We can visualize our clusters in PC space:
reord <- function(cluster){
  avg <- tapply(scored$YPLL.Rate, cluster, mean); avg
  ord <- order(avg); ord
  clus <- factor(cluster, levels=ord); table(clus)
  levels(clus) <- 1:length(clus)
  return( as.numeric(as.character(clus)) )
}
scored <- dat
scored$clust.km <- reord(clust.km)
scored$clust.hc <- reord(clust.hc1)

col <- c("blue","dodgerblue","lightgreen","purple","pink","red","orange","yellow")

#par(mfrow=c(1,1))

clust <- clust.km
plot(pcs, type="n", main="k-means")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)


clust <- scored$clust.hc
plot(pcs, type="n", main="hierarchical clustering")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)
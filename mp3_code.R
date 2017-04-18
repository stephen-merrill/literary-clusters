data <- read.table("C:/Users/Owner/Google Drive/666/collins.txt",header=T)
data_pt1 <- data[,c(2:19)]
rownames(data_pt1) <- data[,1]
library(clValid)
library(cluster)
results <- clValid(data_pt1,3:7,c("hierarchical","diana","agnes"),"internal",method="ward")
summary(results)
#dunn index indicates that hierarchical clustering is always best
#divisive is better for 3 clusters, agglomerative for 4-7 clusters
hierclusters.div <- diana(dist(data_pt1))
hierclusters.agg <- hclust(dist(data_pt1))
plot(hierclusters.div)
plot(hierclusters)
library(dendextend)
rect.hclust(hierclusters,k=4)
table(cutree(hierclusters,3))
#algorithm in pg.546 of rencher christensen for determining cluster validity
nclust <- 4
nsplits <- 700
indexes <- sample(1:nrow(data_pt1),nsplits)
seta <- data_pt1[indexes,]
setb <- data_pt1[-indexes,]
hierclusters.diva <- hclust(dist(seta))
seta.div <- cutree(hierclusters.diva,nclust)
means <- matrix(0,nrow=nclust,ncol=ncol(data_pt1))
for(i in 1:nclust) {
  means[i,] <- colMeans(data_pt1[which(seta.div==i),])
}
mean.dists <- matrix(0,nrow=nrow(setb),ncol=nclust)
for(i in 1:nrow(setb)) {
  for(j in 1:nclust) {
    mean.dists[i,j] <- dist(rbind(setb[i,],means[j,]))    
  }
}
mins <- apply(mean.dists,1,min)
clust.min <- numeric(nrow(setb))
for(i in 1:nrow(setb)) {
  clust.min[i] <- which(mean.dists[i,]==mins[i])  
}
hierclusters.divb <- hclust(dist(setb))
setb.div <- cutree(hierclusters.divb,nclust)
sum(clust.min == setb.div)/nrow(setb)

#process of determining the best clustering algorithm and classification rule
#classification rules: linear, quad, kNN
#clustering: divisive, agglomerative(single, complete, average, ward)

### Comp, Linear, 6 Clusters
library(MASS)
hierclusts.comp <- hclust(dist(data_pt1),method="complete")
y6.comp <- cutree(hierclusts.comp,6)
clusts6.comp <- cbind(y6.comp,data_pt1)

comp.linear6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.linear6<-lda(y6.comp ~ .,data = clusts6.comp[-i,])
  comp.linear6[i]<-predict(hclust.linear6,newdata = clusts6.comp[i,])$class
}
AER.comp.linear6<- 1-(sum(diag(table(y6.comp,comp.linear6)))/nrow(data_pt1))

### Comp, Linear, 7 Clusters
hierclusts.comp <- hclust(dist(data_pt1),method="complete")
y7.comp <- cutree(hierclusts.comp,7)
clusts7.comp <- cbind(y7.comp,data_pt1)

comp.linear7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.linear7<-lda(y7.comp ~ .,data = clusts7.comp[-i,])
  comp.linear7[i]<-predict(hclust.linear7,newdata = clusts7.comp[i,])$class
}
AER.comp.linear7<- 1-(sum(diag(table(y7.comp,comp.linear7)))/nrow(data_pt1))

### Single, Linear, 6 Clusters
hierclusts.sing <- hclust(dist(data_pt1),method="single")
y6.sing <- cutree(hierclusts.sing,6)
clusts6.sing <- cbind(y6.sing,data_pt1)

sing.linear6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.linear6<-lda(y6.sing ~ .,data = clusts6.sing[-i,])
  sing.linear6[i]<-predict(hclust.linear6,newdata = clusts6.sing[i,])$class
}
AER.sing.linear6<- 1-(sum(diag(table(y6.sing,sing.linear6)))/nrow(data_pt1))

### Single, Linear, 7 Clusters
hierclusts.sing <- hclust(dist(data_pt1),method="single")
y7.sing <- cutree(hierclusts.sing,7)
clusts7.sing <- cbind(y7.sing,data_pt1)

sing.linear7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.linear7<-lda(y7.sing ~ .,data = clusts7.sing[-i,])
  sing.linear7[i]<-predict(hclust.linear7,newdata = clusts7.sing[i,])$class
}
AER.sing.linear7<- 1-(sum(diag(table(y7.sing,sing.linear7)))/nrow(data_pt1))

### Average, Linear, 6 Clusters
hierclusts.avg <- hclust(dist(data_pt1),method="average")
y6.avg <- cutree(hierclusts.avg,6)
clusts6.avg <- cbind(y6.avg,data_pt1)

avg.linear6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.linear6<-lda(y6.avg ~ .,data = clusts6.avg[-i,])
  avg.linear6[i]<-predict(hclust.linear6,newdata = clusts6.avg[i,])$class
}
AER.avg.linear6<- 1-(sum(diag(table(y6.avg,avg.linear6)))/nrow(data_pt1))

### Average, Linear, 7 Clusters
hierclusts.avg <- hclust(dist(data_pt1),method="average")
y7.avg <- cutree(hierclusts.avg,7)
clusts7.avg <- cbind(y7.avg,data_pt1)

avg.linear7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.linear7<-lda(y7.avg ~ .,data = clusts7.avg[-i,])
  avg.linear7[i]<-predict(hclust.linear7,newdata = clusts7.avg[i,])$class
}
AER.avg.linear7<- 1-(sum(diag(table(y7.avg,avg.linear7)))/nrow(data_pt1))

### Ward, Linear, 6 Clusters
hierclusts.ward <- hclust(dist(data_pt1),method="ward.D2")
y6.ward <- cutree(hierclusts.ward,6)
clusts6.ward <- cbind(y6.ward,data_pt1)

ward.linear6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.linear6<-lda(y6.ward ~ .,data = clusts6.ward[-i,])
  ward.linear6[i]<-predict(hclust.linear6,newdata = clusts6.ward[i,])$class
}
AER.ward.linear6<- 1-(sum(diag(table(y6.ward,ward.linear6)))/nrow(data_pt1))

### Ward, Linear, 7 Clusters
hierclusts.ward <- hclust(dist(data_pt1),method="ward.D2")
y7.ward <- cutree(hierclusts.ward,7)
clusts7.ward <- cbind(y7.ward,data_pt1)

ward.linear7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.linear7<-lda(y7.ward ~ .,data = clusts7.ward[-i,])
  ward.linear7[i]<-predict(hclust.linear7,newdata = clusts7.ward[i,])$class
}
AER.ward.linear7<- 1-(sum(diag(table(y7.ward,ward.linear7)))/nrow(data_pt1))

### Quadratic classification

### Comp, quad, 6 Clusters
hierclusts.comp <- hclust(dist(data_pt1),method="complete")
y6.comp <- cutree(hierclusts.comp,6)
clusts6.comp <- cbind(y6.comp,data_pt1)

comp.quad6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.quad6<-qda(y6.comp ~ .,data = clusts6.comp[-i,])
  comp.quad6[i]<-predict(hclust.quad6,newdata = clusts6.comp[i,])$class
}
AER.comp.quad6<- 1-(sum(diag(table(y6.comp,comp.quad6)))/nrow(data_pt1))

### Comp, Single, 7 Clusters
hierclusts.comp <- hclust(dist(data_pt1),method="complete")
y7.comp <- cutree(hierclusts.comp,7)
clusts7.comp <- cbind(y7.comp,data_pt1)

comp.quad7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.quad7<-qda(y7.comp ~ .,data = clusts7.comp[-i,])
  comp.quad7[i]<-predict(hclust.quad7,newdata = clusts7.comp[i,])$class
}
AER.comp.quad7<- 1-(sum(diag(table(y7.comp,comp.quad7)))/nrow(data_pt1))

### Single, quad, 6 Clusters
hierclusts.sing <- hclust(dist(data_pt1),method="single")
y6.sing <- cutree(hierclusts.sing,6)
clusts6.sing <- cbind(y6.sing,data_pt1)

sing.quad6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.quad6<-qda(y6.sing ~ .,data = clusts6.sing[-i,])
  sing.quad6[i]<-predict(hclust.quad6,newdata = clusts6.sing[i,])$class
}
AER.sing.quad6<- 1-(sum(diag(table(y6.sing,sing.quad6)))/nrow(data_pt1))

### Single, quad, 7 Clusters
hierclusts.sing <- hclust(dist(data_pt1),method="single")
y7.sing <- cutree(hierclusts.sing,7)
clusts7.sing <- cbind(y7.sing,data_pt1)

sing.quad7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.quad7<-qda(y7.sing ~ .,data = clusts7.sing[-i,])
  sing.quad7[i]<-predict(hclust.quad7,newdata = clusts7.sing[i,])$class
}
AER.sing.quad7<- 1-(sum(diag(table(y7.sing,sing.quad7)))/nrow(data_pt1))

### Average, quad, 6 Clusters
hierclusts.avg <- hclust(dist(data_pt1),method="average")
y6.avg <- cutree(hierclusts.avg,6)
clusts6.avg <- cbind(y6.avg,data_pt1)

avg.quad6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.quad6<-qda(y6.avg ~ .,data = clusts6.avg[-i,])
  avg.quad6[i]<-predict(hclust.quad6,newdata = clusts6.avg[i,])$class
}
AER.avg.quad6<- 1-(sum(diag(table(y6.avg,avg.quad6)))/nrow(data_pt1))

### Average, quad, 7 Clusters
hierclusts.avg <- hclust(dist(data_pt1),method="average")
y7.avg <- cutree(hierclusts.avg,7)
clusts7.avg <- cbind(y7.avg,data_pt1)

avg.quad7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.quad7<-qda(y7.avg ~ .,data = clusts7.avg[-i,])
  avg.quad7[i]<-predict(hclust.quad7,newdata = clusts7.avg[i,])$class
}
AER.avg.quad7<- 1-(sum(diag(table(y7.avg,avg.quad7)))/nrow(data_pt1))

### Ward, quad, 6 Clusters
hierclusts.ward <- hclust(dist(data_pt1),method="ward.D2")
y6.ward <- cutree(hierclusts.ward,6)
clusts6.ward <- cbind(y6.ward,data_pt1)

ward.quad6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.quad6<-qda(y6.ward ~ .,data = clusts6.ward[-i,])
  ward.quad6[i]<-predict(hclust.quad6,newdata = clusts6.ward[i,])$class
}
AER.ward.quad6<- 1-(sum(diag(table(y6.ward,ward.quad6)))/nrow(data_pt1))

### Ward, quad, 7 Clusters
hierclusts.ward <- hclust(dist(data_pt1),method="ward.D2")
y7.ward <- cutree(hierclusts.ward,7)
clusts7.ward <- cbind(y7.ward,data_pt1)

ward.quad7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.quad7<-qda(y7.ward ~ .,data = clusts7.ward[-i,])
  ward.quad7[i]<-predict(hclust.quad7,newdata = clusts7.ward[i,])$class
}
AER.ward.quad7<- 1-(sum(diag(table(y7.ward,ward.quad7)))/nrow(data_pt1))

### Divisive Clustering
library(cluster)

### Divisive, quad, 6 Clusters
hierclusts.div <- diana(dist(data_pt1))
y6.div <- cutree(hierclusts.div,6)
clusts6.div <- cbind(y6.div,data_pt1)

div.quad6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.quad6<-qda(y6.div ~ .,data = clusts6.div[-i,])
  div.quad6[i]<-predict(hclust.quad6,newdata = clusts6.div[i,])$class
}
AER.div.quad6<- 1-(sum(diag(table(y6.div,div.quad6)))/nrow(data_pt1))

### Divisive, quad, 7 Clusters
hierclusts.div <- diana(dist(data_pt1))
y7.div <- cutree(hierclusts.div,7)
clusts7.div <- cbind(y7.div,data_pt1)

div.quad7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.quad7<-qda(y7.div ~ .,data = clusts7.div[-i,])
  div.quad7[i]<-predict(hclust.quad7,newdata = clusts7.div[i,])$class
}
AER.div.quad7<- 1-(sum(diag(table(y7.div,div.quad7)))/nrow(data_pt1))

### Divisive, linear, 6 Clusters
hierclusts.div <- diana(dist(data_pt1))
y6.div <- cutree(as.hclust(hierclusts.div),6)
clusts6.div <- cbind(y6.div,data_pt1)

div.linear6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  hclust.linear6<-lda(y6.div ~ .,data = clusts6.div[-i,])
  div.linear6[i]<-predict(hclust.linear6,newdata = clusts6.div[i,])$class
}
AER.div.linear6<- 1-(sum(diag(table(y6.div,div.linear6)))/nrow(data_pt1))

### Divisive, linear, 7 Clusters
hierclusts.div <- diana(dist(data_pt1))
y7.div <- cutree(as.hclust(hierclusts.div),7)
clusts7.div <- cbind(y7.div,data_pt1)

div.linear7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  hclust.linear7<-lda(y7.div ~ .,data = clusts7.div[-i,])
  div.linear7[i]<-predict(hclust.linear7,newdata = clusts7.div[i,])$class
}
AER.div.linear7<- 1-(sum(diag(table(y7.div,div.linear7)))/nrow(data_pt1))

### K NEAREST NEIGHBORS
library(class)
kn <- 32

### Divisive, KNN, 6 Clusters
hierclusts.div <- diana(dist(data_pt1))
y6.div <- as.factor(cutree(as.hclust(hierclusts.div),6))
div.knn6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)) {
  div.knn6[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y6.div[-i],k=kn)
}
AER.div.knn6<- 1-(sum(diag(table(y6.div,div.knn6)))/nrow(data_pt1))

### Divisive, KNN, 7 Clusters
hierclusts.div <- diana(dist(data_pt1))
y7.div <- as.factor(cutree(as.hclust(hierclusts.div),7))
div.knn7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)) {
  div.knn7[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y7.div[-i],k=kn)
}
AER.div.knn7 <- 1-(sum(diag(table(y7.div,div.knn7)))/nrow(data_pt1))

### Comp, knn, 6 Clusters
hierclusts.comp <- hclust(dist(data_pt1),method="complete")
y6.comp <- as.factor(cutree(hierclusts.comp,6))
comp.knn6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  comp.knn6[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y6.comp[-i],k=kn)
}
AER.comp.knn6<- 1-(sum(diag(table(y6.comp,comp.knn6)))/nrow(data_pt1))

### Comp, knn, 7 Clusters
hierclusts.comp <- hclust(dist(data_pt1),method="complete")
y7.comp <- as.factor(cutree(hierclusts.comp,7))
comp.knn7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  comp.knn7[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y7.comp[-i],k=kn)
}
AER.comp.knn7<- 1-(sum(diag(table(y7.comp,comp.knn7)))/nrow(data_pt1))

### Single, knn, 6 Clusters
hierclusts.sing <- hclust(dist(data_pt1),method="single")
y6.sing <- as.factor(cutree(hierclusts.sing,6))
sing.knn6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  sing.knn6[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y6.sing[-i],k=kn)
}
AER.sing.knn6<- 1-(sum(diag(table(y6.sing,sing.knn6)))/nrow(data_pt1))

### Single, knn, 7 Clusters
hierclusts.sing <- hclust(dist(data_pt1),method="single")
y7.sing <- as.factor(cutree(hierclusts.sing,7))
sing.knn7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  sing.knn7[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y7.sing[-i],k=kn)
}
AER.sing.knn7<- 1-(sum(diag(table(y7.sing,sing.knn7)))/nrow(data_pt1))

### Average, knn, 6 Clusters
hierclusts.avg <- hclust(dist(data_pt1),method="average")
y6.avg <- as.factor(cutree(hierclusts.avg,6))
avg.knn6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  avg.knn6[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y6.avg[-i],k=kn)
}
AER.avg.knn6<- 1-(sum(diag(table(y6.avg,avg.knn6)))/nrow(data_pt1))

### Average, knn, 7 Clusters
hierclusts.avg <- hclust(dist(data_pt1),method="average")
y7.avg <- as.factor(cutree(hierclusts.avg,7))
avg.knn7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  avg.knn7[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y7.avg[-i],k=kn)
}
AER.avg.knn7<- 1-(sum(diag(table(y7.avg,avg.knn7)))/nrow(data_pt1))

### Ward, knn, 6 Clusters
hierclusts.ward <- hclust(dist(data_pt1),method="ward.D2")
y6.ward <- as.factor(cutree(hierclusts.ward,6))
ward.knn6<-factor(levels = c("1","2","3","4","5","6"))
for(i in 1:nrow(data_pt1)){
  ward.knn6[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y6.ward[-i],k=kn)
}
AER.ward.knn6<- 1-(sum(diag(table(y6.ward,ward.knn6)))/nrow(data_pt1))

### Ward, knn, 7 Clusters
hierclusts.ward <- hclust(dist(data_pt1),method="ward.D2")
y7.ward <- as.factor(cutree(hierclusts.ward,7))
ward.knn7<-factor(levels = c("1","2","3","4","5","6","7"))
for(i in 1:nrow(data_pt1)){
  ward.knn7[i] <- knn(train=data_pt1[-i,],test=data_pt1[i,],cl=y7.ward[-i],k=kn)
}
AER.ward.knn7<- 1-(sum(diag(table(y7.ward,ward.knn7)))/nrow(data_pt1))

#=== Elbow
set.seed(123)
# Compute and plot wss for k = 3 to k = 7
k.max <- 7 # Maximal number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(data_pt1, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Method")

abline(v = 3, lty =2)
#=== end Elbow

#=== Silhouette
library(cluster)
k.max <- 7
sil <- rep(0, k.max-2)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 3:k.max){
  km.res <- kmeans(data_pt1, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data_pt1))
  sil[i-2] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(3:k.max, sil, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k")
abline(v = 3, lty = 2)
#=== end Silhouette

#=== Gap method
set.seed(123)
gap_stat <- clusGap(data_pt1, FUN = kmeans, nstart = 20, K.max = 7, B = 20)
# Print the result
print(gap_stat, method = "firstmax")
plot(gap_stat, frame = FALSE, xlab = "Number of clusters k",main="Gap Method")
abline(v = 3, lty = 2)
#=== end Gap

### Chosen clustering: 4 clusters with complete linkage
# Best clusters: hierarchical with complete linkage and 4 clusters
# Calculate E and H
clust<-hclust(dist(data_pt1))
clust.div<-diana(dist(data_pt1))
clust.4<-cutree(clust,4)
clust.3<-cutree(clust.div,3)
clust.5<-cutree(clust,5)
clust.4.all<-cbind(clust.4,data_pt1)

H<-matrix(0,ncol = 18,nrow = 18)
clust.4.means<-colMeans(clust.4.all[,2:19])
clust.4.means.groups<-matrix(NA,ncol=18,nrow = 4)
for(j in 1:4){
  clust.4.means.groups[j,]<-colMeans(clust.4.all[clust.4.all$clust.4==j,2:19])
}

n.c<-as.numeric(table(clust.4.all$clust.4))
for(i in 1:4){
  H<-H+n.c[i]*(clust.4.means.groups[i,]-clust.4.means)%*%t(clust.4.means.groups[i,]-clust.4.means)
}

E<-matrix(0,ncol = 18,nrow = 18)
for(i in 1:4){
  clust.4.group<-as.matrix(clust.4.all[clust.4.all$clust.4==i,2:19])
  for(k in 1:n.c[i]){
    E<-E+(clust.4.group[k,]-clust.4.means.groups[i,])%*%t(clust.4.group[k,]-clust.4.means.groups[i,])
  }
}

eigens<-eigen(solve(E)%*%H)
eval<-as.numeric(eigens[[1]])[1:3]
evec<-matrix(as.numeric(eigens$vectors[,1:3]),ncol=3)
#essential dimension is 2 
cumsum(eval)/sum(eval)

# Get the discriminant function
nu.e<-1000-3
a.star<-diag(sqrt((1/nu.e)*diag(E)))%*%evec[,1:3]
row.names(a.star)<-names(clust.divis.3.all[,2:19])
a.star[abs(a.star[,1])>max(abs(a.star[,1]))/2,1]


### Use genre to determine cluster differences
genre <- data[,21]
genre <- as.factor(genre)
library(plyr)
genre <- mapvalues(genre,from=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
          to=c("Press: Reporting","Press: Editorial","Press: Reviews","Religion",
               "Skills/Hobbies","Popular Lore", "Biography","Official Comm","Learned",
               "Gen Fiction","Mystery","Sci Fi","Adventure/Western","Romance","Humor"))
group1 <- genre[which(clust.4==1)]
group2 <- genre[which(clust.4==2)]
group3 <- genre[which(clust.4==3)]
group4 <- genre[which(clust.4==4)]
results <- cbind(table(group1),table(group2),table(group3),table(group4))
colnames(results) <- c("Group 1","Group 2","Group 3","Group 4")
library(xtable)
xtable(results)

group1 <- genre[which(clust.3==1)]
group2 <- genre[which(clust.3==2)]
group3 <- genre[which(clust.3==3)]
results <- cbind(table(group1),table(group2),table(group3))
colnames(results) <- c("Group 1","Group 2","Group 3")
library(xtable)
xtable(results)
apply(results,2,sum)

supergenre <- as.factor(data[,21])
supergenre <- revalue(supergenre,c("1"="Press","2"="Press","3"="Press","4"="Non-Press Non-Fiction",
                                   "5"="Non-Press Non-Fiction","6"="Non-Press Non-Fiction","7"="Biography",
                                   "8"="Scholarship/Official Documents","9"="Scholarship/Official Documents",
                                   "10"="Fiction","11"="Fiction","12"="Fiction","13"="Fiction",
                                   "14"="Fiction","15"="Fiction"))
group1 <- supergenre[which(clust.5==1)]
group2 <- supergenre[which(clust.5==2)]
group3 <- supergenre[which(clust.5==3)]
group4 <- supergenre[which(clust.5==4)]
group5 <- supergenre[which(clust.5==5)]
super.results <- cbind(table(group1),table(group2),table(group3),table(group4),table(group5))
colnames(super.results) <- c("Group 1","Group 2","Group 3","Group 4","Group 5")
xtable(super.results)

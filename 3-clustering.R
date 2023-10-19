#Hierarical Clustering
head(mtcars)
distance_mat<-dist(mtcars)
hcl<-hclust(distance_mat,method="average")
plot(hcl)

#KMeans Clustering
install.packages("ClusterR")
install.packages("cluster")
library(ClusterR)
library(cluster)
head(iris)
set.seed(300)
kms<-kmeans(iris[1:4],centers = 3,nstart = 20)
cm<-table(iris$Species,kms$cluster)
cm
plot(iris[1:2],col=kms$cluster)
clusplot(iris[1:2],kms$cluster)

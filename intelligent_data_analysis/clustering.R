## 1 read file 
library(factoextra)
df = read.csv("./intelligent_data_analysis/data.csv",row.names = 1)
plot(df)
data = scale(df)# data scale
head(data)
## 2 Determine the best number of clusters
### a.using elbow method  finding center number
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) 
  wss[i] <- sum(kmeans(data,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
####  the best cluster center is 2
### b.using NbClust package finding center number
library(NbClust)
NbClust(data, min.nc=2, max.nc=6, 
        method = "ward.D2", index = "all")   
#### the best cluster center is 2
## 3.Сluster data into the optimal number of clusters using hierarchical clustering

### 1 euclidiaan

euclidiaan_data = dist(data,method = "euclidean")
euclidiaan_fit<-hclust(euclidiaan_data,method='average')
plot(euclidiaan_fit)
cut_euclidiaan <- cutree(euclidiaan_fit, k = 2)
plot(euclidiaan_fit)
rect.hclust(euclidiaan_fit , k = 3, border = 2:6)
abline(h = 3, col = 'red')
fviz_cluster(list(data = data, cluster = cut_euclidiaan),main="Euclidiaan Method Cluster")
### 2.manhattan
manhattan_data = dist(data,method = "manhattan")
manhattan_fit<-hclust(manhattan_data,method='average')
plot(manhattan_fit)
cut_manhattan <- cutree(manhattan_fit, k = 2)
rect.hclust(manhattan_fit , k = 3, border = 2:6)
abline(h = 3, col = 'red')
fviz_cluster(list(data = data, cluster = cut_manhattan),main="Manhattan Method Cluster")


## 4.K-means 
library(ggfortify)
library(ggplot2)
kmeans(data,2)
autoplot(kmeans(data, 2),data=data,label=TRUE, label.size=3, frame=TRUE)

### 5.SOM
library(kohonen)

foo <-som(data, grid = somgrid(5, 5, "hexagonal"))
summary(foo)
plot(foo,type="counts")
plot(foo,type="quality")
plot(foo,type="code")
foo$unit.classif

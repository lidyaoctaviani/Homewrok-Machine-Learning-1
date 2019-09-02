#title : "Clustering using Hierarchial Clustering"
#Description : Application of Hierarchial Clustering

#loading dataset
x <- read.csv("https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv")

#Berkenalan dulu dengan data (hint: summary(), dim(), plot())
summary(x)
dim(x)
plot(x)

# Calculates similarity as Euclidean distance between observations
d <- dist(x[,2:4])

# Returns hierarchical clustering model
hclust.out <- hclust(d = d)
summary(hclust.out)

# Draws a dendrogram
layout(1)
plot(hclust.out)
abline(h = 3.5, col = "red")

# Cut by height h
cutree(hclust.out, h = 6)

# Cut by number of clusters
cutree(hclust.out, k = 3)

#Data yang masuk ke cluster 1
which(cutree(hclust.out, k = 3) %in% 1)
paste("Data in cluster: ",as.character(length(which(cutree(hclust.out, k = 3) %in% 1))))

#Data yang masuk cluster 2
which(cutree(hclust.out, k = 3) %in% 2)
paste("Data in cluster: ",as.character(length(which(cutree(hclust.out, k = 3) %in% 2))))

#Data yang masuk cluster 3
which(cutree(hclust.out, k = 3) %in% 3)
paste("Data in cluster: ",as.character(length(which(cutree(hclust.out, k = 3) %in% 3))))

#Fitting hierarchical clustering models using different methods
hclust.complete <- hclust(d, method = "complete")
hclust.average <- hclust(d, method = "average")
hclust.single <- hclust(d, method = "single")

plot(hclust.complete)
plot(hclust.average)
plot(hclust.single)

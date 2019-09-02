#title : "Clustering using K-Means"
#Description : Application of K-Means for Clustering

#loading dataset
x <- read.csv("https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv")

#ambil kolom ke 2 hingga 4
x<-x[,2:4]
head(x)

#Berkenalan dulu dengan data (hint: summary(), dim(), plot())
summary(x)

# Determine Number of Clusters using Elbow Method
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x[,1:3], centers = i, nstart = 20)
  
# Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#Determine Number of Clusters using Silhouette Method
silhouette_score <- function(k){
  km <- kmeans(x, centers = k, nstart=20)
  ss <- silhouette(km$cluster, dist(x))
  mean(ss[, 3])
}

#For 2 to 15 clusters
k <- 2:15
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

#Berdasarkan hasil yang dijalankan menggunakan elbow method, cluster optimalnya adalah 3

#Berdasarkan hasil yang dijalankan menggunakan silhoutte method, cluster optimalnya adalah 3

#Maka banyaknya cluster yang digunakan adalah 3.

#Menggunakan K-Means dengan K=3
km <- kmeans(x, centers = 3, nstart=20)
plot(x, col = km$cluster,
     main = "k-means with 3 clusters")

#Membership
km$cluster

#Berikut adalah data yang masuk kedalam cluster 1
which(km$cluster %in% 1)
paste("Data in cluster: ",as.character(length(which(km$cluster %in% 1))))

#Berikut adalah data yang masuk kedalam cluster 2
which(km$cluster %in% 2)
paste("Data in cluster: ",as.character(length(which(km$cluster %in% 2))))

#Berikut adalah data yang masuk kedalam cluster 3
which(km$cluster %in% 3)
paste("Data in cluster: ",as.character(length(which(km$cluster %in% 3))))

#Data beserta cluster akan disimpan pada data frame bernama df
df<-cbind(x,cluster=km$cluster)
head(df)

#Ini adalah 6 data pertama yang masuk pada cluster 1
head(df[df$cluster==1,])
summary(df[df$cluster==1,])[,1:3]

#Ini adalah yang masuk pada cluster 2
head(df[df$cluster==2,])

#Ini adalah 6 data pertama yang masuk pada cluster 3
head(df[df$cluster==3,])
summary(df[df$cluster==3,])[,1:3]

#Dan berikut adalah center dari k-means dengan cluster 3
km$centers
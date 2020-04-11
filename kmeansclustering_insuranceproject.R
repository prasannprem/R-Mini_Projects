library(dplyr)
data1<-read.csv("/home/prasannprem_live/Projects/cluster_data.csv",header= TRUE,sep=",")

summary(data1)

#to keep only s(elected variables from data
data2<- data1[c(2,5)]

# elbow method for kmeans clustering,to identify the no of cluster 

k.max <- 15 # maximum no of clusters 

wss <- sapply(1:k.max,function(k) {kmeans(data2,
                                          k)$tot.withinss})

plot (1:k.max,wss,type = "b",frame = FALSE,
      xlab = "no of cluster k ",
      ylab = "total within cluster sum of square")

#kmeans clustering
km<-kmeans(data2,3)
km

km$cluster
km$centers
km$withinss
km$tot.withinss

#Plot the clusters
library(animation)

kmeans.ani(data2,3)

#bind the cluster value with data
Final_clus<-cbind(data1,km$cluster)

#export the file
write.csv(Final_clus,"Kmeansclustering_insurance_output.csv")

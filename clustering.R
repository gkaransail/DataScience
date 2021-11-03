Universities <- read.csv("C:/Users/gupta/Desktop/data science assignments/Universities.csv")

install.packages("factoextra")
library(factoextra)

fviz_nbclust(Universities[,-1],kmeans,method = "wss")+labs(subtitle="Elbow method")

km<-kmeans(Universities[,-1],4)
km

km$centers
km$cluster

install.packages("animation")
library(animation)
windows()
km<-kmeans.ani(Universities[,-1],4)


data<- read.csv("~/R/Ecommerce Project/Mall_Customers.csv", header=FALSE)

View(data)

library(ggplot2)
#Data cleaning 
library(tidyverse)
x =na.omit(data)
View(x)



u = x[-1,]
View(u)

#Principle component Analysis
pca = prcomp(u[,-2], center = TRUE,scale. = TRUE)
summary(pca)

l <- pca$center[,1:4]
l
View(l)

#K-mean clustering 

library(factoextra)
library(cluster)

fviz_nbclust(l, kmeans, method = "wss")


k <-kmeans(l, centers=5)  
m <-kmeans(l, centers=6) 
n <-kmeans(l, centers=7) 
k
m
n


#Creating animation
install.packages("plyr")
install.packages("animation")
library(plyr)
library(animation)
ani5 <- kmeans.ani(l, centers=5)
ani6 <- kmeans.ani(l, centers=6)
ani7 <- kmeans.ani(l, centers=7)

#Heierarical clustering 
d <- dist(l,method = "euclidean")
fit <- hclust(d,method= "average")
plot(fit)

cluster1 <- cutree(fit,k=6)
cluster2 <- cutree(fit,k=7)


#Draw Dendogram with red border number
rect.hclust(fit,k=6,border="red")
rect.hclust(fit,k=7,border="red")
install.packages('package_name', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(fit)
avg_col_dend <- color_branches(avg_dend_obj, h = 7)
plot(avg_col_dend)

#Association Rules using Apriori (Market Basket analysis)
install.packages("arules")
library(arules)

rules <- apriori(data, parameter = list(supp = 0.01, conf = 0.2))
inspect(rules[1:10])



#Validation 

train <- data[1:130,]
test <- data[131:200,]
View(train)
predictTest <- predict(n, newdata = test, type = "response")
confusion <- table(prob>0.5,data$V3)
confusion

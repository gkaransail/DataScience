library(tidyverse)
library(ggplot2)

data <- read.csv("~/R/heart.csv")
data
glimpse(data)
summary(data)

#Data transformation
data2 <- data %>%
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fbs = if_else(fbs == 1, ">120", "<=120"),
         exang = if_else(exang == 1, "YES" ,"NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())
data2

head(data2)

#Data Cleaning 
x =na.omit(data2)
x

#####Data Analytics
#Scatter Plot
plot(x)


#Probable table
prop.table(table(data2$target))

#
x %>%
  group_by(ï..age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(ï..age, n), fill = 'skyblue')+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Agecount")

#Corrplot
install.packages("corrplot")
install.packages("ggplot2")
library(corrplot)



cor_heart <- cor(x[, 10:14])
cor_heart
corrplot(cor_heart, method ='circle', type='upper')

####Machine Learning Models######################################

#Princple Component Analysis
pca = prcomp(x[,10:14], center = TRUE,scale. = TRUE)
summary(pca)

#K-mean Clustering 
library(factoextra)
library(cluster)

fviz_nbclust(data2[,10:14], kmeans, method = "wss")
y = data2[,10:14]

k <-kmeans(y, centers=11)  
m <-kmeans(y, centers=10) 
n <-kmeans(y, centers=9)
o <-kmeans(y, centers=12)
k
m
n
o

#Animation 
library(plyr)
library(animation)
ani5 <- kmeans.ani(y, centers=9)
ani6 <- kmeans.ani(y, centers=10)
ani7 <- kmeans.ani(y, centers=11)

#Heierarical clustering 
d <- dist(y,method = "euclidean")
fit <- hclust(d,method= "average")
plot(fit)

cluster1 <- cutree(fit,k=10)
cluster2 <- cutree(fit,k=11)

#Draw Dendogram with red border number
rect.hclust(fit,k=10,border="red")
rect.hclust(fit,k=11,border="red")
install.packages('package_name', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(fit)
avg_col_dend <- color_branches(avg_dend_obj, h = 10)
plot(avg_col_dend)

#Random Forest
install.packages("caret",dependencies = TRUE)
install.pacakges("randomForest")
library(caret)
library(randomForest)

model <- randomForest(data2$target~.,data=data2,ntree=1000)
model

#prediction
pred <- predict(model,data2)
pred
table(pred,data2$target)


setwd("C:/Users/Sachin Jain/Downloads/Sachin Jain Personal/2. Fall 2021/IE 500/Project/Working")

df0<-read.csv("ahs2019n.csv",encoding = "UTF-8")

####Importing Library####
library(Hmisc)
library(dplyr)
library(data.table)

####Creating a subset of New York-Newark-Jersey City, NY-NJ-PA MSA####
df1 <- filter(df0,HHCITSHP!="'5'" & HHCITSHP!="'-6'")
df2 <- filter(df0,HHCITSHP=="'5'")

df3<-rbind(df1[1:500,],df2[1:500,])

#fwrite(df3,"Final_data.csv")

rm(df0,df1,df2)
####Saving data####
#fwrite(df3,"Sampledata.csv")

df3<-read.csv("Final_data.csv")

####Data Wrangling####
attach(df3)

selcoldf<- as.data.frame(cbind(TOTROOMS,BEDROOMS,BATHROOMS,UNITSIZE,ELECAMT, 
            HEATTYPE, ACPRIMARY, RODENT, ROACH, FLOORHOLE, 
            WALLCRACK, PAINTPEEL, LEAKI, LEAKO, ROOFSAG, ROOFHOLE,WALLSIDE, WINBROKE,
            MOLDBASEM,MOLDBATH,MOLDBEDRM,MOLDKITCH,MOLDLROOM,MOLDOTHER,
            HHRACE,HHSPAN,HHRACEAS,HHRACEPI,HHDTHSP, HHCITSHP,
            NUMPEOPLE, HINCP, FINCP, TOTHCAMT, RENT, MARKETVAL, MAINTAMT,
            HHGRAD,RATINGHS,RATINGNH))

detach(df3)

rm(selcoldf)
#coldata<-as.data.frame(colnames(df3))
#fwrite(coldata,"Colnames.csv")
df4<-selcoldf #creating a copy
#fwrite(df3,"AHSdata2.csv")

for (i in 1:ncol(df4)){
  cat(i,colnames(df4)[i],"\n")
  x<- df4[,i]
  cat(unique(x),"\n")
}


for (x in 3:30){
  df4[,x]<-as.numeric(gsub("'","",df4[,x]))
}

df4$ROOFSAG[df4$ROOFSAG<0]<-NA
df4$ROOFHOLE[df4$ROOFHOLE<0]<-NA
df4$WALLSIDE[df4$WALLSIDE<0]<-NA
df4$WINBROKE[df4$WINBROKE<0]<-NA
df4$MOLDBASEM[df4$MOLDBASEM<0]<-NA
df4$MOLDBATH[df4$MOLDBATH<0]<-NA
df4$MOLDBEDRM[df4$MOLDBEDRM<0]<-NA
df4$MOLDKITCH[df4$MOLDKITCH<0]<-NA
df4$MOLDLROOM[df4$MOLDLROOM<0]<-NA
df4$MOLDOTHER[df4$MOLDOTHER<0]<-NA
df4$HHGRAD[df4$HHGRAD=="'-6'"]<-NA

df5<-na.omit(df4)

df5$HHCITSHP[df5$HHCITSHP<5]<-1 #US Citizen
df5$HHCITSHP[df5$HHCITSHP==5]<-2 # Immigrants

df5$HHCON <- rowSums(df5[,c(10:24)]) #Summing Housing Condition

df5$HHCONF<-ifelse(df5$HHCON>29,"Good","Bad")

df5$HHGRAD<-as.numeric(gsub("'","",df5$HHGRAD))

for (x in 31:41){
  df5[,x]<-as.numeric(df5[,x])
}

for (x in 1:2){
  df5[,x]<-as.numeric(df5[,x])
}

df5$ROOMPP<-df5$TOTROOMS/df5$NUMPEOPLE

df5$ROOMPPF<-ifelse(df5$ROOMPP>=1,"Good","Bad")

df5$HHGRAD<-ifelse(df5$HHGRAD>=39,"Graduate","Not Graduate")

#fwrite(df5,"Final_data_for_Visualization.csv")

#df5<-read.csv("dataset.csv")

####Visualization####

hist(df5$MARKETVAL, 
     main="Histogram for Market Value", 
     xlab="Market Price", 
     border="blue", 
     col="maroon",
     breaks=100)

boxplot(df5$MARKETVAL, col='maroon',xlab = 'MarketPrice', main = 'Box Plot for Market Value')

hist(df5$HINCP, 
     main="Histogram for Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     breaks=50)


boxplot(df5$HINCP, col='maroon',xlab = 'Income', main = 'Box Plot for House Hold Income')

counts <- table(df5$TOTROOMS)
hist(counts, 
     main="Histogram for Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     breaks=1)


par(mfrow=c(1,1))
counts <- table(df5$HHCITSHP)
barplot(counts, main="No.of Citizens and Immigrants",
        xlab="Citizenship Status", col=c("darkgrey","maroon"),
        names.arg = c("Citizens", "Immigrants"))

counts <- table(df5$HHCONF)
barplot(counts, main="No. of Good and bad House",
        xlab="House Status", col=c("darkgrey","maroon"),
        names.arg = c("Bad House", "Good House"))

par(mfrow=c(1,1))
counts <- table(df5$HHCONF,df5$HHCITSHP)
counts
barplot(counts, main="House Condition by Citizenship Status",
        xlab="Citizenship Status", col=c("darkgrey","maroon"),
        legend = rownames(counts))

par(mfrow=c(1,1))
counts <- table(df5$HHRACE)
counts[1:5]
barplot(counts[1:5], main="Races in Survey",
        xlab="Race", names.arg = c("Whites","Blacks","Alaska Native","Asian","Pacific I"))

par(mfrow=c(1,1))
counts <- table(df5$BEDROOMS,as.factor(df5$BEDROOMS))
barplot(counts, main="Total no. of Bedrooms in Survey",
        xlab="No. of Bedrooms", col="grey")

par(mfrow=c(1,1))
counts <- table(df5$TOTROOMS,as.factor(df5$TOTROOMS))
barplot(counts, main="Total no. of Rooms in Survey",
        xlab="No. of Rooms", col=rainbow(factor(df5$TOTROOMS)))

library(ggplot2)
# counts
hist(df6$HHCONF)
ggplot(df5,aes(x=as.factor(HHCITSHP))) +
  geom_bar()

library(corrplot)
M = cor(df6)
M[26,15]= 0.28
M[15,26]=0.28
corrplot(M)

####Models####

head(df5)

str(df5)

attach(df5)
df6<-as.data.frame(cbind(TOTROOMS,BEDROOMS,BATHROOMS,UNITSIZE,ELECAMT, 
                         HEATTYPE, ACPRIMARY, RODENT, ROACH,
                         HHRACE,HHSPAN,HHRACEAS,HHRACEPI,HHDTHSP, HHCITSHP,
                         NUMPEOPLE, HINCP, FINCP, TOTHCAMT, RENT, MARKETVAL, MAINTAMT,
                         HHGRAD,RATINGHS,RATINGNH,HHCONF,ROOMPP))

detach(df5)

df6 <- transform(df6,HHGRAD=ifelse(df6$HHGRAD=="Graduate",1,0))
df6 <- transform(df6,HHCONF=ifelse(df6$HHCONF=="Good",1,0))

for (x in 1:ncol(df6)){
  df6[,x]<-as.numeric(df6[,x])
}

df6<-df6[sample(1:nrow(df6)), ]

#fwrite(df6,"Final_data_for_models.csv")

F_Split <- function(dframe=data.frame(),p=int()){
  df_size = floor((p/100)*nrow(dframe)) 
  picked = sample(seq_len(nrow(dframe)),size = df_size)
  df_train  = dframe[picked,]
  df_test = dframe[-picked,]
  assign("df_train",df_train,envir = .GlobalEnv)
  assign("df_test",df_test,envir = .GlobalEnv)
}

F_Split(df6,80)

####Logistic Regression####

logit.model<-glm(HHCONF~.,family=binomial,data=df_train)
summary(logit.model)
trainerror<-predict(logit.model, df_train, type = "response")
tab_cm<-table(df_train$HHCONF, trainerror > 0.65)
tab_cm
acc_logit <- sum(diag(tab_cm)) / nrow(df_train)
acc_logit

my_prediction_tr2<-predict(logit.model, df_test, type = "response")
tab_cm2<-table(df_test$HHCONF, my_prediction_tr2 > 0.65)
tab_cm2
acc_logit2 <- sum(diag(tab_cm2)) / nrow(df_test)
acc_logit2

####Logistic 5-Fold Validation####

F_CV<-function(dframe1=data.frame(),k=int()){
  set.seed(100)
  flag<-sample(c(1:k),nrow(dframe1),replace=T)
  dframe2 <-cbind(dframe1,flag)
  list_of_kfolds<-list()
  for (i in 1:k) {
    list_of_kfolds[[i]]<- data.frame(subset(dframe1,dframe2$flag== i))
  }
  
  testing_data<-list_of_kfolds
  training_data<-list()
  
  for(i in 1:k){
    training_data[[i]]<- data.frame(subset(dframe1, dframe2$flag!=i))
  }
  
  assign("testing_data",testing_data,envir = .GlobalEnv)
  assign("training_data",training_data,envir = .GlobalEnv)
}

F_CV(df6,5)

logit.train.err<-list()
logit.test.err<-list()


for (i in 1:5){
  logit.model2<-glm(HHCONF~.,family=binomial,data=training_data[[i]])
  train.error<-predict(logit.model2,data=training_data[[i]], type = "response")
  tempdata<- training_data[[i]]
  tab_cm<-table(tempdata$HHCONF, train.error> 0.65)
  acc_logit <- sum(diag(tab_cm)) / nrow(training_data[[1]])
  logit.train.err[i]<-acc_logit
  
  testerror<-predict(logit.model2, testing_data[[1]], type = "response")
  tempdata<- testing_data[[1]]
  tab_cm2<-table(tempdata$HHCONF, testerror> 0.65)
  acc_logit2 <- sum(diag(tab_cm2)) / nrow(testing_data[[1]])
  logit.test.err[i]<-acc_logit2
}

plot(1:5,logit.train.err,xlab = "Test Data", ylab = 'Accuracy', type = 'l')
max.acc.train <- which.max(logit.train.err)
points(max.acc.train, logit.train.err[max.acc.train], col = 'red', cex = 2, pch = 19)

plot(1:5,logit.test.err,xlab = "Test Data", ylab = 'Accuracy', type = 'l')
max.acc.test <- which.max(logit.test.err)
points(max.acc.test, logit.test.err[max.acc.test], col = 'red', cex = 2, pch = 19)

####Decision tree####
#install.packages("party")
library(rpart)

dtree <- rpart(HHCONF~.,method="class", data=df_train,parms=list(split="information"))

dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree, cp=0.01000000)
install.packages("rpart.plot")
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")


dtree.pred<- predict(dtree.pruned, df_train, type="class")

dtree.perf <- table(df_train$HHCONF, dtree.pred,
                    dnn=c("Actual", "Predicted"))
dtree.perf

dtree.acc <- sum(diag(dtree.perf)) / nrow(df_train)
dtree.acc

##test data

dtree.pred2 <- predict(dtree.pruned, df_test, type="class")

dtree.perf2 <- table(df_test$HHCONF, dtree.pred2,
                    dnn=c("Actual", "Predicted"))
dtree.perf2

dtree.acc2 <- sum(diag(dtree.perf2)) / nrow(df_test)
dtree.acc2


####Random Forest####
#install.packages("randomForest")
library(randomForest)
fit.forest<-randomForest(HHCONF~.,data=df_train,importance=TRUE,proximity=TRUE)

fit.forest

importance(fit.forest, type=2)

varImpPlot(fit.forest,type=2)

forest.pred <- predict(fit.forest, df_train)
forest.pred
forest.perf <- table(df_test$HHCONF, forest.pred,
                     dnn=c("Actual", "Predicted"))
dim(forest.perf)
library(caret)
accuracy <- confusionMatrix(forest.pred, df_test)


ctrl<-trainControl(method = "cv",number=10)

rf.fit<-train(HHCONF~.,data=df_train,method="rf",trControl=ctrl,truelength=10)
plot(rf.fit)
predt<-predict(rf.fit,df_train)
confusionMatrix(table(df_train$HHCONF,predt)
############################Clustering
####K-mean clustering
install.packages("factoextra")
install.packages("cluster")
library(factoextra)
library(cluster)

fviz_nbclust(df_train, kmeans, method = "wss")
km.out1 <- kmeans(df_test, centers=6, nstart=20)
km.out2 <- kmeans(df_train, centers=9, nstart=20)
summary(km.out1)
print(km.out1)

summary(km.out2)
print(km.out2)

#confusion matrix
con <- table(df_test$HHCONF,km.out1$cluster)

accuracy <- sum(diag(con)/sum(con))
accuracy
####Animation in kmean
install.packages("plyr")
install.packages("animation")
library(plyr)
library(animation)
ani1 <- kmeans.ani(df_train,centers = 7)
ani2 <- kmeans.ani(df_train,centers = 5)
ani3 <- kmeans.ani(df_train,centers = 5)
##Hierarical Clustering
d <- dist(df_train,method = "euclidean")
fit <- hclust(d,method= "average")
plot(fit)

heirar <- cutree(fit,k=2)
heirar

con <- table(df_train$HHCONF, heirar)

accuracy <- sum(diag(con)/sum(con))
accuracy

rect.hclust(fit,k=2,border="red")
rect.hclust(fit,k=9,border="red")
#install.packages('package_name', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(fit)
avg_col_dend <- color_branches(avg_dend_obj, h = 2)
plot(avg_col_dend)

####SVM####
library(e1071)
svmfit = svm(df_train$HHCONF ~ ., data =df_train, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit, df_train)

summary(svmfit)
pred <- predict(svmfit,df_train)
table(pred,df_train)


'''####Pca
library(factoextra)

pca <- princomp(df_train, cor=TRUE, scores = TRUE, covmat = NULL)
summary(pca)
pca$scores

df <- pca$scores[,1:20]
df
pairs(df)
cor(df)

trg <- data.frame(df, df_train$HHCONF)
trg

library(caret)
set.seed(3456)
trainIndex <- createDataPartition(trg$df_train.HHCONF, p = .7,
                                  list = FALSE,
                                  times = 1)
train1 <- trg[ trainIndex,]
test1 <- trg[-trainIndex,]

reg <- lm(train1$df_train.HHCONF~., data = train1)
summary(reg)


pred <- predict(reg,test1)
pred'''

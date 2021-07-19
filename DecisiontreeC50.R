data("iris")
install.packages("caret")
install.packages("C50")
library(caret)
library(C50)
set.seed(1000)
print(iris)

data<-(createDataPartition(iris$Species,p=0.70,list=F))
train<-iris[data,]
test<-iris[-data,]

#model building 
model<-C5.0(Species~.,data=train)
model
summary(model)

#Predict the model
pred<-predict.C5.0(model,test[,])
View(pred)
a<-table(test$Species,pred)
accuracy=sum(diag(a))/sum(a)
accuracy
plot(model)

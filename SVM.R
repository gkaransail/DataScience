forestfires <- read.csv("C:/Users/karan/OneDrive/Desktop/Data science course/R/forestfires.csv")
View(forestfires)
train<-forestfires[1:400,]
test<-forestfires[401:517,]
install.packages("kernlab")
library(kernlab)
model<-ksvm(train$wind~.,data=train, kernel = "vanilladot")

pred<-predict(model,train)
head(pred)

agreement<-pred == test$wind
prop.table(table(agreement))


#still need to work on svm
data <- read.csv("~/R/winequality-red.csv")
View(data)
head(data)
#Barplot
barplot(table(data$quality))

#Categorizing quality into different groups
data$taste <- ifelse(data$quality <6,'Bad','Good')
data$taste[data$quality ==6]<- 'Normal'
data$taste
data$taste<-as.factor(data$taste)
table(data$taste)

#trains and test data
install.packages("caTools")
library(caTools)
data1= sample.split(data,SplitRatio = 0.7)
train =subset(data,data1==TRUE)
test =subset(data,data1==FALSE)


#Implementation of Randomforest
set.seed(1)
install.packages('randomForest')
library(randomForest)
model <- randomForest(
  formula = taste ~ .,
  data = train
)
model
pred<- predict(model,newdata=test)
table(pred,test$taste)

#accuracy measure
226+69+196/nrow(test)

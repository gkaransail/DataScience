wc_at<- read.csv("C:/Users/gupta/Desktop/data science assignments/wc-at (1).csv")
reg<-plot(wc_at$AT,wc_at$Waist)
abline(reg)
lr<-lm(wc_at$AT~wc_at$Waist)
lrm<-lm(AT~Waist)
summary(lr)

#splitting the data into training and  test data
set.seed(2)
library(caTools)
split<-sample.split(wc_at,splitRatio=0.7)
split
train<-subset(wc_at,split="TRUE")
test<-subset(wc_at,split="FALSE")
train
test
#create the model
model<-lm(wc_at$AT~wc_at$Waist)
model1<-lm(AT~.,data=train)
summary(model1)

#prediction
pred <-predict(model1,data=test)
pred

#comparing predicted vs actual values
plot(test$AT,type="l",col="red")


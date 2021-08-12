#Install libraries

library(recommenderlab)
library(Matrix)
library(caTools)

#importing data
book <- read.csv("C:/Users/Karan/Downloads/book.csv")

#structure the data
str(book)

#histogram-plot for movie rating 
hist(book$Book.Rating)

#scatter plot
plot(book$Book.Rating)

#datastructure should be in matrix for recommendation engine
matrix<- as(book, 'realRatingMatrix')
matrix@data

#recommendation model using "Popular" method

recomm <- Recommender(matrix, method = "POPULAR")

#predicting for userid =1083
pred <- predict(recomm, matrix[1083], n=5)
as(pred,"list")
 
#prediction
pred <- predict(recomm, matrix, n=5)


as(pred,"list")



#recommendation using "collaborative filtring" method
recom2 <- Recommender(matrix, method = "UBCF")
pred2 <- predict(recomm2, matrix, n=5)
as(pred2,"list")

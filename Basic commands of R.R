install.packages('HSAUR2')
library(HSAUR2)
data("Forbes2000", package="HSAUR2")
View(Forbes2000)
ls()
print(Forbes2000)

str(Forbes2000)    #to get the Internal structure of the data(datatypes)


help("Forbes2000")     #Information about the command "Forbes2000"

class(Forbes2000)       #class of the object

dim(Forbes2000)       #dimension of dataframe

nrow(Forbes2000)       #get the number of rows

ncol(Forbes2000)        #get the number of columns

length(Forbes2000[,'rank'])

nlevels(Forbes2000[,'category'])      #No. of levels in the frame 



f<-Forbes2000     #Assign a name to the Dataframe
f


table(Forbes2000[,'category'])

f[f$assets>1000,c('name','sales')]           #satisfying the logical condition

table(Forbes2000$assets>1000)


UK<- subset(Forbes2000,country=="United Kingdom")    #subsetting the data 
UK


lapply(f,summary)          #returns the summary of the data 


table(complete.cases(Forbes2000))         #Returns a logical value of # of TRUE
f.complete.cases <- complete.cases(Forbes2000)        #to find the missing the values in the data 
f.complete.cases

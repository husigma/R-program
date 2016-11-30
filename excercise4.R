#Kurt Bembridge
#Data 630 - 9040
##Multiple linear regression on the imports-85.csv

#import the data into R
cars<-read.csv(file.choose(), T, ',')

#looking at the first 10 observatin. 
View(head(cars,10))

#names before removing variables
names(cars)

#removing variables 
cars$make<- NULL
cars$engine_type<-NULL
cars$num_of_cylinders<-NULL
cars$fuel_system<-NULL

#names after removing variables
names(cars)

#summary of data
summary(cars)

#amount of rows with missing values
nrow(cars[!complete.cases(cars),])

#Get ride of the variable with alot of NA's
cars$normalized_losses<-NULL

#splitting the data into two set. 
set.seed(1234)
ind<-sample(2, nrow(cars), replace = T, prob = c(0.7,0.3))
car.train<-cars[ind == 1,]
car.test<-cars[ind==2,]

#building 1st model
model<-lm(Price~., car.train)

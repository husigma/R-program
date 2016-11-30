#Kurt Bembridge
#Data 630 9040
#Kmeans

###This script will place the vowels in cluster base off the analysis 
###Kmean method

#initiating the library
library(caret)
library(cluster)
library(ggplot2)

#setting seed so the result are reproducable 
set.seed(1223)

#importing the data and viewing the content
vowel <- read.csv(file.choose(), header = T, sep = ',')
View(vowel)

#making a copy of the data to be analyze
newvowel<-vowel

#attaching data sets so they could easily be searched
attach(vowel)
attach(newvowel)


#destribution of class variable
table(newvowel$Class)

##getting rid of the variable
newvowel$Class<-NULL


#looking at the structure of the data
str(newvowel)

#gettng rid of the categorical variables

newvowel$Sex<-NULL
newvowel$TrainTest<-NULL
newvowel$SpeakerNumber<-NULL

#making sure the operation is done
names(newvowel)
str(newvowel)
summary(newvowel)
#scaling the data
hot<-scale(newvowel, center = T, scale = T)

###building the model###
kc<-kmeans(hot, 5)

#looking at the model
kc

###confusion table on the data set
table(vowel$Class, kc$cluster)

clusplot(newvowel,kc$cluster, shade = T, color = T)

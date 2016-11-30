#Kurt Bembridge
#Data 630 9040
#Kmeans

###This script will place customers in cluster base off the analysis 
###Kmean method

#initiating the library
library(psych)
library(cluster)
library(ggplot2)
library(fpc)
library(caret)
library(clv)
library(clValid)
library(kohonen)
library(party)
library(plyr)
library(RankAggreg)
library(stats)

#setting seed so the result are reproducable 
set.seed(1223)

#importing the data
wholesale <- read.csv("/Volumes/USB DISK/ASSIGNMENT6/Wholesale customers data.csv")

#drescription of the data
head(wholesale,10)
summary(wholesale)
str(wholesale)
#converting the data into sales data by store
wholesale$StoreID<-c(1:nrow(wholesale))
names(wholesale)

#copying the data
colnames(wholesale)[2] <- "Priority"

#changing the label of the Priority variable
wholesale$Priority<-factor(wholesale$Priority, levels = c(1,2,3), 
                           labels = c('low','medium','high') )


#reordering rows so ID goes to the frist and at the same time getting ride of channel variable
wholesale<-wholesale[,c(9,2,3,4,5,6,7,8)]
head(wholesale, 10)

#table of priority variable
table(wholesale$Priority)

#making a copy of the data
wholesale1<-wholesale

#attaching the data
attach(wholesale)
attach(wholesale1)

#getting rid of Store ID variable because it and ID
wholesale1$StoreID<-NULL

#Since the region is the one we would like to cluster 
wholesale1$Priority<-NULL

#looking at the data to describe the variable
describe(wholesale1)

#Normalizing the data because the Standard Deviation was too wide
wholesale1<-scale(wholesale1, center = T, scale = T)
describe(wholesale1)

#building the model
kc<-kmeans(wholesale1,3, iter.max = 10)

#looking at the model
kc
kc$iter

#table to see which store is place in which cluster
table(kc$cluster, wholesale$Priority)

#plotting the cluster
clusplot(wholesale1,kc$cluster, shade = T, color = T, label = 3)

#looking how many clusters are going to be the best.
bss<-integer(length(2:15))
for(i in 2:15)bss[i]<-kmeans(wholesale1, centers = i)$betweenss
plot(1:15,bss,type = 'b',xlab = 'Number of Clusters', ylab = 'Sum of Square', col = 'blue')

wss<-integer(length(2:15))
for(i in 2:15)wss[i]<-kmeans(wholesale1,centers = i)$tot.withinss
lines(1:15,wss,type = 'b' )

###center = 4###
#building the model
kc<-kmeans(wholesale1,4, iter.max = 10,)

#looking at the model
kc

#plotting the cluster
clusplot(wholesale1,kc$cluster, shade = T, color = T, labels = 4)

#analyzing the data more to find the best value
tuned_model<-pamk(wholesale1)
tuned_model

#ploting the best 
clusplot(wholesale1,tuned_model$pamobject$clustering, color = T, shade = T, labels = 2)


#another way to analyze data to see the value of K


validate<-clValid(wholesale1, 2:6, clMethods = "kmeans", validation = "internal")
bestnumber<-getRanksWeights(validate)
RankAggreg(x=bestnumber$ranks, k=2, weights=bestnumber$weights, seed = 1234, verbose=FALSE, rho=.115)

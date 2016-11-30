#Kurt Bembridge
#Data 630 9040
#K-means cluster analysis

###This script will analyze a data set using k-means.

#innitiate library
library(cluster)

#setting seed so the model could be replecated. 
set.seed(1234)

###importing the data###
vehicle<-read.csv(file.choose(), header = T, sep = ',', na=c(' ', 'NA', '?'))
attach(vehicle)

#copying the data just incase
myvehicle<-vehicle
attach(myvehicle)

#removing class variable
myvehicle$Class<-NULL

#scaling the data
newvehicle<-scale(myvehicle, center = TRUE, scale = TRUE)

####data processing###
prop.table(table(vehicle$Class))


#running model with k=4
kc<-kmeans(newvehicle, 4)

#looking at the model
kc

#kc#ite
kc$iter

#Cross validation of the data
table(vehicle$Class, kc$cluster)

clusplot(myvehicle, kc$cluster, color=TRUE, shade=TRUE, labels=4, lines=0,
         main = 'Plot of the cluster')

# k = 5
kc5<-kmeans(newvehicle,5)

# k = 8
kc8<-kmeans(newvehicle,8)

# k = 3
kc3<-kmeans(newvehicle,3)

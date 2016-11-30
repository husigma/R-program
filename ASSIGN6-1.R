#Kurt Bembridge
#Data 630 9040
#Kmeans

###This script will place the us Open men in cluster base off the analysis 
###Kmean method

#initiating the library
library(psych)
library(cluster)
library(ggplot2)

#setting seed so the result are reproducable 
set.seed(1223)
USOpenmen <- read.csv(file.choose(), header = T, sep = ',', na = c('?', ' ', '?'))
USOpenmen <- read.csv("C:/Users/bembridgek01/Desktop/USOpen-men-2013.csv")
View(head(USOpenmen))


#seeing how many observation with missing variables
nrow(USOpenmen[!complete.cases(USOpenmen),])

#duplicate the data
MenOpen <- USOpenmen

#looking at the data
str(MenOpen)
summary(MenOpen)

#attaching the variable
attach(MenOpen)

#deleting logical regression
MenOpen$WNR.1 <- NULL
MenOpen$WNR.2 <- NULL
MenOpen$UFE.1 <- NULL
MenOpen$UFE.2 <- NULL

#Replacing the missing value with the mean
MenOpen$NPA.1[is.na(MenOpen$NPA.1)] <- round(mean(MenOpen$NPA.1, na.rm = TRUE))
MenOpen$NPW.1[is.na(MenOpen$NPW.1)] <- round(mean(MenOpen$NPW.1, na.rm = TRUE))
MenOpen$NPA.2[is.na(MenOpen$NPA.2)] <- round(mean(MenOpen$NPA.2, na.rm = TRUE))
MenOpen$NPW.2[is.na(MenOpen$NPW.2)] <- round(mean(MenOpen$NPW.2, na.rm = TRUE))
MenOpen$ST3.1[is.na(MenOpen$ST3.1)] <- round(mean(MenOpen$ST3.1, na.rm = TRUE))
MenOpen$ST4.1[is.na(MenOpen$ST4.1)] <- round(mean(MenOpen$ST4.1, na.rm = TRUE))
MenOpen$ST5.1[is.na(MenOpen$ST5.1)] <- round(mean(MenOpen$ST5.1, na.rm = TRUE))
MenOpen$ST5.2[is.na(MenOpen$ST5.2)] <- round(mean(MenOpen$ST5.2, na.rm = TRUE))
MenOpen$ST4.2[is.na(MenOpen$ST4.2)] <- round(mean(MenOpen$ST4.2, na.rm = TRUE))
MenOpen$ST3.2[is.na(MenOpen$ST3.2)] <- round(mean(MenOpen$ST3.2, na.rm = TRUE))

#deleting the unique value variable
MenOpen$Player1<-NULL
MenOpen$Player2<-NULL

#deleting the round variable because it only have one factor
MenOpen$Round<-NULL

#Deleting the result variable because that is what we would like to cluster. 

#Looking at the statistical description of the data
describe(MenOpen)



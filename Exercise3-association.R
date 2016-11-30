#Kurt Bembridge
#Data 630-9040
#Data Mining

##An association rule applied to credit card 

#read the file
card<-read.csv(file.choose(), T, ',')

#View the first 10 lines
View(head(card), 10)

#View the structure of the data
str(card)

#View the summary to see if there is any NA's
summary(card)

#Go through the columns 1 by 1 and replace the NA's if theres any
for(i in 1:ncol(card)){
card[is.na(card[,i]), i] <- mean(card[,i], na.rm = TRUE)
}

#Make sure the NA's was removed
summary(card)

#Import library to convert to factor
library(arules)

#Convert the numerical and integers to factors using frequency and cluster
card$A2<-discretize(card$A2, 'frequency', categories = 6)
card$A3<-discretize(card$A3, 'frequency',categories = 6)
card$A8<-discretize(card$A8,'frequency',categories = 6)
card$A11<-discretize(card$A11,'frequency', categories = 6)
card$A14<-discretize(card$A14,'cluster',categories = 6)
card$A15<-discretize(card$A15,'frequency',categories = 6)

#making sure they were converting to factor
str(card)

#Viewing the factors and see how they were divided.
summary(card)
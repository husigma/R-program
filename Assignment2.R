#Kurt Bembridge
#Data 630-9040
#October 6, 2016

##Assignment 2-- to clean the data and make some visualization on the data obtained
##from a client. 
college<-read.csv(file.choose(), header = F, sep = ',')

#finding out how many variables and observation
dim(college)

#View the head of the data in a separate window
head(college, 10)

#Summary of the data looking for anomilities
summary(college)

#the structure of the data
str(college)

#changing the . to NA
college[college=='*']<-NA
summary(college)

#transforming the variables into numerics data to be operate on.
college$V5<-as.integer(college$V5)
college$V6<-as.integer(college$V6)
college$V7<-as.integer(college$V7)
college$V8<-as.integer(college$V8)
college$V9<-as.integer(college$V9)
college$V10<-as.integer(college$V10)
college$V11<-as.integer(college$V11)

#checking for the change of class
str(college)

#changing the columns names.
colnames(college)<-c('FICE', 'College','State','Type','AS_full.prof','AS_assoc.prof',
                     'AS_assis.prof','AS_all_ranks','AC_full.prof','AC_assoc.prof',
                     'AC_assis.prof','AC_all_ranks','Num.full.prof','Num.assoc.prof',
                     'Num.assis.prof','Num.instructor','Num.faculty')

#checking to see if all the names were changed
names(college)



#changing the NA to the mean for the dataset 
for(i in 1:ncol(college)){
  college[is.na(college[,i]), i] <- mean(college[,i], na.rm = TRUE)
}
summary(college)

#Removing the FICE variable because it not needed. 
college$FICE<-NULL
names(college)


#getting ride of the noise in Type
college$Type[college$Type=='VIIB']<-'IIB'
summary(college$Type)

#plotting the All rank salary
plot(college$AS_all_ranks,college$AC_all_ranks, xlab='Average Salary All Rank',
     ylab='Average Compensation All Rank', main='Comparison of All Rank')

#plot the number of school by categories they falls in. 
plot(college$Type, col=c('1','2','3'), ylab='Number of Schools',
     xlab='Type of School', main='Categories of the school type')

#A box plot can show us the outliers of the graph some other thing. 
##importing the library to be used
library(ggplot2)
type.plot<-ggplot(college,aes(factor(Type), Num.faculty),xlab='type', ylab='Faculty')
type.plot + geom_boxplot()+ggtitle('Plot of Type of school and total faculty')








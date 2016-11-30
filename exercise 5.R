library(party)

credit<-read.csv(file.choose(), F, ',')

attach(credit)

head(credit, 10)
summary(credit)
str(credit)
nrow(credit[!complete.cases(credit),])

names(credit)<-c('Male','Age','Debt','Married','BankCustomer',
'EducationLevel','Ethnicity','YearsEmployed','PriorDefault',
'Employed','CreditScore','DriversLicense','Citizen','ZipCode',
'Income','Approved')

names(credit)

#changing the male variable if needed
#credit$Male<-factor(credit$Male, levels=c('a','b'), labels = c('1','0'))

#duplicate the data set
crditcard<-credit

credit$Approved<-factor(credit$Approved, levels = c('+','-'), labels = c('yes','no'))

#splitting the data into training and testing.
set.seed(1234)
ind<-sample(2, nrow(credit), replace = T, prob = c(0.7,0.3))
train.data<-credit[ind==1,]
test.data<-credit[ind==2,]

nrow(train.data[!complete.cases(train.data),])

train.data<-na.omit(train.data)

#building the model
model<-ctree(Approved~.,train.data)

#print the model to see the summary
print(model)

#plotting the model
plot(model)

#confussion matrix on train.data
table(predict(model,train.data), train.data$Approved)

#confusion matrix on the test.data
table(predict(model,test.data), test.data$Approved)

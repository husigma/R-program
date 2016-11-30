stops<-read.csv(file.choose(), T,',')

summary(stops)

stops$stop_id<-NULL

library(e1071)
library(arules)

summary(stops)

stops$driver_id<-discretize(stops$driver_id, "frequency", categories = 6)
stops$driver_age<-discretize(stops$driver_age, 'cluster', categories = 6)
stops$ticket<-factor(stops$ticket)


set.seed(1234)
ind<-sample(2,nrow(stops), replace = T, prob = c(0.9,0.1))
train<-stops[ind==1,]
test<-stops[ind==2,]

model<- naiveBayes(ticket~., data = train)

print(model)

prediction<-predict(model, test)

table(prediction, test$ticket)

summary(prediction)

print(prediction)

summary(model)

View(model$apriori)

plot(stops$officer_id, stops$ticket)

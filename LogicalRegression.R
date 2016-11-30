stops<-read.csv(file.choose(), T,',')

summary(stops)

stops$stop_id<-NULL

library(e1071)

set.seed(1234)
ind<-sample(2,nrow(stops), replace = T, prob = c(0.7,0.3))
train<-stops[ind==1,]
test<-stops[ind==2,]

attach(c(train,test))

model<-glm(ticket~., family = gaussian, data = train)

print(model)

summary(model)

table(round(model$fitted.values), train$ticket)

predict(model, test)[1:10]

prediction<-round(predict(model, test))

table(prediction, test$ticket)

summary(step(model))

model2<-step(model)

prediction2<-round(predict(model2, test))

table(prediction2, test$ticket)

plot(predict(model), residuals(model), col=c('blue'))
lines(lowess(predict(model), residuals(model)), col=c('black'),
      lwd=2)
abline(h=0, col='grey')

#Script for Assignment 5
library(party)

stop1data<-read.csv(file.choose(), T, ',')
stops<-stop1data



attach(stops)

summary(stops)
str(stops)

stops$stop_id<- NULL
stops$ticket<-factor(stops$ticket)

set.seed(1234)
ind <- sample(2, nrow(stops), replace = TRUE, prob = c(0.8, 0.2))
train <- stops[ind == 1, ]
test<- stops[ind == 2, ]

formula<- ticket~.
stops_tree<-ctree(formula, train)

print(stops_tree)
nodes(stops_tree)
plot(stops_tree)
plot(stops_tree, type = 'simple')

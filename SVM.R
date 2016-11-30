library(e1071)

cancer<- read.csv(file.choose(), T, ',')

attach(cancer)

names(cancer)<-c('id_number', 'Clump_Thickness','Uniformity_of_Cell_Size',
                 'Uniformity of Cell Shape','Marginal Adhesion',
                 'Single_Epithelial_Cell_Size','Bare_Nuclei',
                  'Bland_Chromatin', 'Normal_Nucleoli',
                 'Mitoses','Class')

View(head(cancer))

summary(cancer)

nrow(cancer)

wow<-na.omit(cancer)

summary(wow)

cancer<-wow

set.seed(1234)
ind<-sample(2, nrow(cancer), replace = T, prob = c(0.7,0.3))
train<-cancer[ind == 1,]
test<-cancer[ind==2,]

model<-svm(Class~., data = train, cost = 1000, gamma = 0.0001)

#Kurt Bembridge
#Data 630 - 9040
#Data Mining

#This model will find association between products on a market analysis. 

#initiate the library used
library(arules)
library(Rcpp)
library(arulesViz)

#read the csv file
basket<-read.transactions(file.choose(), format = 'basket',sep = ',')
#inspecting the first 
inspect(head(basket,10))

#measure the size of the entire basket
basket_size<-size(basket)

#calculate frequency of the occurance of the items.
basket_freq<-itemFrequency(basket)

#a count of how many times each item occur in all the baskets total. 
basket_count<-(basket_freq/sum(basket_freq))*sum(basket_size)

#Sorting the basket in desending order by the total occurance of each item
basket_sort<-sort(basket_count, decreasing = T)

#looking at the first 10 head
basket_sort[1:10]

#Frequency plot of the 20 most used item
itemFrequencyPlot(basket,topN=20,type="absolute", col= 1:20)

#deleting the basket with 1 item
usable_basket<-basket[basket_size>1]

#the size of the new dataset
dim(usable_basket)

#building the rules with the default values and minimum length of 2
rules<-apriori(usable_basket,parameter = list(supp=0.1,conf=0.8,minlen=2))
inspect(rules[1:10])

#building the rules with support=0.01,confidence=0.9 and length=2
rules<-apriori(usable_basket,parameter = list(supp=0.01,conf=0.9,minlen=2))
inspect(head(rules,10))

#Sort the rules by lift and inspect it
rules.sorted <- sort(rules, by="lift")
inspect(head(rules.sorted, 10))

#pruning the rules for redundancies
subset.matrix <- is.subset(rules.sorted, rules.sorted)
View(subset.matrix)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1

#investigating which rules are prunes
which(redundant)
sum(redundant)

#putting the unpruned rules into a new matrix
rules.pruned <- rules.sorted[!redundant]

#the size of the new rules
rules.pruned
#inspecting the new rules
inspect(rules.pruned[1:10])

#looking at the bread data only
rules_bread<-apriori(usable_basket,parameter = list(supp=0.1,conf=0.9,minlen=2), 
               appearance = list(default ="lhs", rhs = "semi-finished bread"))
rules_bread.sorted <- sort(rules_bread, by="lift")
subset.matrix <- is.subset(rules_bread.sorted, rules_bread.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant_bread <- colSums(subset.matrix, na.rm=T) >= 1
sum(redundant_bread)
bread_rules<-rules_bread.sorted[!redundant_bread]
bread_rules
inspect(bread_rules)

plot(rules.pruned)
plot(rules.pruned, method = 'group')
plot(rules.pruned,method="graph",interactive=TRUE)
plot(bread_rules)
plot(bread_rules,method="graph",interactive=TRUE)
sel <- plot(rules_bread, measure=c("support", "lift"), shading="confidence", interactive=TRUE)
plot(bread_rules, method="paracoord", control=list(reorder=TRUE))
plot(bread_rules, method="graph", control=list(type="itemsets"))
plot(bread_rules, method = "matrix3D")




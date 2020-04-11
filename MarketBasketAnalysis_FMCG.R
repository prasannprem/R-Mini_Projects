#Project-Market Basket analysis - Sector-FMCG
library(dplyr)
library(arules)

#import using read.transaction in arules package for transactional
Groceries<-read.transactions("/home/prasannprem_live/Projects/groceries.csv",sep=",")
Groceries

#The total no of transactions : 1.7 million
9835*169

summary(Groceries)

#The total no of filled cellwith data are: 43k
9835*169*0.02609146
#0.02609146 is density explained in summary

# by default support is 0.1 and confidence is 0.8
rules<- apriori(Groceries)
rules

#apriori function is in arules package, it employs levelwise search for frequent item sets
#here minlen value means how many minimum items can be in one basket
rules<- apriori(Groceries,parameter = list(support=0.001,confidence=0.8,minlen=2))
rules

summary(rules)


#lhs                                #rhs
#{whipped/sour cream , berries}    => {whole milk} = 2+1 = 3
  
#{bottled water, other vegetables,pip fruit}        => {whole milk} = 3+1 = 4 


#  using association rule mining,check rules and their stats
inspect(head(sort(rules,by="lift"),2))
inspect(head(sort(rules,by="lift")[1:4]))

inspect(head(sort(rules,by="lift"),100))
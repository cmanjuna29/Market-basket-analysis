
#Association


new_data <- read.csv('ifood_df.csv')

new_data$Product_1 <- ifelse(data$MntWines>0,'MntWines','')
new_data$Product_2 <- ifelse(data$MntFruits>0,'MntFruits','')
new_data$Product_3 <- ifelse(data$MntMeatProducts>0,'MntMeatProducts','')
new_data$Product_4 <- ifelse(data$MntFishProducts>0,'MntFishProducts','')
new_data$Product_5 <- ifelse(data$MntSweetProducts>0,'MntSweetProducts','')
new_data$Product_6 <- ifelse(data$MntGoldProds>0,'MntGoldProds','')

head(new_data)

new_data2 <- new_data[,c('Income','MntWines','MntFruits','MntMeatProducts','MntFishProducts','MntSweetProducts','MntGoldProds','Product_1','Product_2','Product_3','Product_4','Product_5','Product_6')]
write.csv(new_data,'new_data2.csv')  

write.csv(new_data2,'new_data3.csv')  

getwd()

mod_data <- read.csv('new_data3 (1).csv')
mod_data1 <- mod_data[,c('X','Income','Product.Combination')]


library(plyr)
transactionData <- ddply(mod_data1,c("X","Income"),
                         function(df1)paste(df1$Product.Combination,
                                            collapse = ","))



transactionData

transactionData$X <- NULL
transactionData$Income <- NULL

#Rename column to items
colnames(transactionData) <- c("items")
transactionData

install.packages('arules')
library(arules)
library(arulesViz)
library(tidyverse)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

write.csv(transactionData,'market_basket_transactions.csv', quote = FALSE, row.names = FALSE)

baskets <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

summary(baskets)

head(baskets)
summary(baskets)


rules <- apriori(baskets)
rules <- as.data.frame(rules)
a <- inspect(rules[1:30])
rules
a


#generating the rules
basket_rules = apriori(baskets,parameter = list(sup = 0.7, conf = 0.2))
basket_rules2 = apriori(baskets,parameter = list(sup = 0.7, conf = 0.7))
basket_rules3 = apriori(baskets,parameter = list(sup = 0.7, conf = 0.1))

basket_rules = sort(basket_rules, by='confidence', decreasing = TRUE)
basket_rules2 = sort(basket_rules2, by = 'confidence', decreasing = TRUE)
basket_rules3 = sort(basket_rules3, by = c("confidence","lift","support"), decreasing = c(TRUE,TRUE,TRUE))
rules = sort(rules, by = c("confidence","lift","support"), decreasing = c(TRUE,TRUE,TRUE))


#Going ahead with basket_rules3 as the parameters are minimal

basket_rules2 = apriori(baskets,parameter = list(sup = 0.9, conf = 0.8))
basket_rules2 <- as.data.frame(rules)
a <- inspect(basket_rules2[1:20])
basket_rules2

summary(basket_rules3)

basket_rules3 = sort(basket_rules3, by = c("support",'confidence',"lift"), decreasing = c(TRUE,TRUE,TRUE))

a <- inspect(basket_rules3[1:80])
a <- as.data.frame(a)
a

basket_rules3 = sort(basket_rules3, by = c('confidence',"lift",'support'), decreasing = c(TRUE,TRUE,TRUE))

b <- inspect(basket_rules3[1:30])
a <- as.data.frame(b)
b

write.csv(a,'Rules.csv')


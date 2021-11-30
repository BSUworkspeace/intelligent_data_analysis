install.packages("arules")
install.packages("arulesViz")
library(arules)


set.seed(101)
orders <- data.frame(
  transactionID = sample(1:500, 1000, replace=T),
  item = paste("item", sample(1:6, 1000, replace=T),sep = "")
)
orders
tab = table(orders$transactionID,orders$item)
tab
binarymat = ifelse(tab > 0,1,0)
print(binarymat)
# use write.csv function
write.csv(binarymat,"binarymat.csv")
myrules=arules::apriori(binarymat, 
                        parameter = list(
                          support = 0.1,  #最低支持度
                          confidence = 0.25, #最低置信度
                          minlen = 2)) #最低项数
inspect(myrules)
summary(myrules)
ordered_myrules <- sort(myrules, by="lift")
inspect(ordered_myrules[1:5])
# lhs        rhs     support   confidence coverage  lift      count
# [1] {item2} => {item1} 0.1154734 0.3623188  0.3187067 1.0121552 50   
# [2] {item1} => {item2} 0.1154734 0.3225806  0.3579677 1.0121552 50   
# [3] {item1} => {item5} 0.1039261 0.2903226  0.3579677 0.9451855 45   
# [4] {item5} => {item1} 0.1039261 0.3383459  0.3071594 0.9451855 45   
# [5] {item3} => {item6} 0.1016166 0.3358779  0.3025404 0.9322764 44 

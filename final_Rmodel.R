rm(list = ls())

data <- read.csv('C:\\Users\\Vineet\\Desktop\\data mining\\Assignment 7\\MortgageDefaultersData_V2.csv')
data$LoanValuetoAppraised=as.numeric(data$LoanValuetoAppraised)

Mortgage1 <- subset( data, select = -c(Status))
#Mortgage1
str(Mortgage1)
table(Mortgage1$OUTCOME)
library(ROSE)
data_balanced_under <-ovun.sample(OUTCOME ~ ., 
                                  data = Mortgage1, method = "under", N = 1340, seed = 1)$data

prop.table(table(data_balanced_under$OUTCOME))

set.seed(2014)

ind = sample(2, nrow(data_balanced_under), replace=TRUE, prob=c(0.75,0.25))
trainData = data_balanced_under[ind==1,]
testData = data_balanced_under[ind==2,]
dim(trainData)
dim(testData)


library(rpart)
Mortgage_tree = rpart(OUTCOME~., data =data_balanced_under,
                      control = rpart.control(minsplit=10),
                      parms = list(split="gini"))
plot(Mortgage_tree , 
     uniform = T)
text(Mortgage_tree , use.n = T, all = T, cex = 0.7, xpd = T)
printcp(Mortgage_tree )
Mortgage_tree$variable.importance
barplot(Mortgage_tree$variable.importance)
library(caret)
confusionMatrix( predict(Mortgage_tree ,  newdata = testData, 
                         type="class"),  testData$OUTCOME)
pred.tree.under<-predict(Mortgage_tree ,  newdata = testData)
roc.curve(testData$OUTCOME, pred.tree.under[,2])

length(pred.tree.under)
library(ROCR)
pred = prediction(pred.tree.under[,2], testData$OUTCOME)

perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=F)


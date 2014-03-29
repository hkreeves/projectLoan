##
## Cousera- Data Analysis by Jeff Leek
## Assignment 1 - Study of Loans
##
## Author: Kai He
##
## revisit on March 28, 2014
##

library(rpart)
library(rpart.plot)
library(gbm)
library(caret)

load("./data/loanRaw.rda")
names(loan.p)

loan.p <- loan.p[,-c(7,10)]

set.seed(334)
split <- createDataPartition(loan.p$Interest, p=0.7, list=F)
train <- loan.p[split,]
test <- loan.p[split,]

# linear regression
lm1 <- lm(Interest.Rate ~ fico * Loan.Length  
		+ Amount.Requested + log10(Monthly.Income)
		+ Loan.Purpose
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=train)
summary(lm1)

# regression tree
tree1 <- rpart(Interest.Rate ~., data=train, method="anova")
printcp(tree1)
plotcp(tree1)

# gradient boosting regression
gbm1 <- gbm(Interest.Rate ~., data=train, dist="gaussian")
gbm.caret <- train(Interest.Rate ~., data=train, method="gbm")
# c-v says best n.trees=100, int.depth=3, shrinkage=0.1

# predict
library(medley)

y.lm <- predict(lm1, newdata=test)
y.tree <- predict(tree1, newdata=test)
y.gbm <- predict(gbm.caret, new=test)

rmse(y.lm, test$Interest.Rate) # 0.02010133
rmse(y.tree, test$Interest.Rate) # 0.02194533
rmse(y.gbm, test$Interest.Rate) # 0.01582453
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

# gradient boosted regression
fit.control <- trainControl("cv", 10)
gbm.grid <- expand.grid(interaction.depth=c(1,3,5),
				n.trees=(1:5)*50,
				shrinkage=c(0.01, 0.05, 0.1, 0.2))
gbm1 <- gbm(Interest.Rate ~., data=train, dist="gaussian")
gbm.caret <- train(Interest.Rate ~., data=train, method="gbm", 
			trControl=fit.control,
			tuneGrid=gbm.grid,
			verbose=F)
# c-v says best n.trees=100, int.depth=3, shrinkage=0.1
# customized grid says  n.trees = 200, int.depth = 5 and shrinkage = 0.05
trellis.par.set(caretTheme())
plot(gbm.caret)

# random forests
rf.grid <- expand.grid(mtry=c(1,3,5))
rf1 <- train(Interest.Rate ~. - Employment.Length - Loan.Purpose, 
		data=train, method="rf", 
		trControl=fit.control,
		tuneGrid=rf.grid)

# predict
library(medley)

y.lm <- predict(lm1, newdata=test)
y.tree <- predict(tree1, newdata=test)
y.gbm <- predict(gbm.caret, new=test)
y.rf <- predict(rf1, new=test)

rmse(y.lm, test$Interest.Rate) # 0.02010133
rmse(y.tree, test$Interest.Rate) # 0.02194533
rmse(y.gbm, test$Interest.Rate) # 0.01489804
rmse(y.rf, test$Interest.Rate) # 0.008386137

# importance
summary(gbm.caret$finalModel)
#                                                          var      rel.inf
#fico                                                     fico 60.212647684
#Loan.Length60 months                     Loan.Length60 months 15.238933635
#Amount.Funded.By.Investors         Amount.Funded.By.Investors  9.069470774
#Amount.Requested                             Amount.Requested  5.811899600
#Open.CREDIT.Lines                           Open.CREDIT.Lines  3.614806525
#Inquiries.in.the.Last.6.Months Inquiries.in.the.Last.6.Months  2.511193095
#Debt.To.Income.Ratio                     Debt.To.Income.Ratio  1.384537324
#Monthly.Income                                 Monthly.Income  1.155252913

rf1$finalModel$importance
#                               IncNodePurity
#Amount.Requested                  0.23682688
#Amount.Funded.By.Investors        0.26442070 *
#Loan.Length60 months              0.39145736 **
#Debt.To.Income.Ratio              0.12944817
#Monthly.Income                    0.11169155
#Open.CREDIT.Lines                 0.12203626
#Inquiries.in.the.Last.6.Months    0.09524658
#fico                              1.61419656 ***
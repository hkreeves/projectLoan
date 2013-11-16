##
## Cousera- Data Analysis by Jeff Leek
## Assignment 1 - Study of Loans
##
## Author: Kai He
##
## Exploratory Plotting
##

library(lattice)

load("./data/loanRaw.rda")

table(loan.p$Loan.Purpose)
##               car        credit_card debt_consolidation        educational   home_improvement              house 
##                50                444               1307                 15                152                 20 
##    major_purchase            medical             moving              other   renewable_energy     small_business 
##               101                 30                 29                201                  4                 87 
##          vacation            wedding 
##                21                 39 
par(mfrow=c(2,1), xaxt="s", las=0, mar=c(1, 4.1, 1, 2.1))
plot(loan.p$Loan.Purpose, xaxt="n")
boxplot(Amount.Requested ~ Loan.Purpose, data=loan.p, las=2)

xyplot( Amount.Funded.By.Investors ~ Amount.Requested | Loan.Purpose, data=loan.p)

## Interest rate relationship with FICO scores
bwplot( Interest.Rate ~ FICO.Range | Loan.Length, data=loan.p, scales=list(relation="free", rot=90))
xyplot( Interest.Rate ~ fico | Loan.Length, data=loan.p)
xyplot( Interest.Rate ~ fico | Loan.Length, data=loan.p, as.table=T, 
		panel=function(x,y,...)
		{
			panel.xyplot(x,y,...)
			fit <- lm(y ~ x)
			panel.abline(fit,col="red")
		}
)
lm1 <- lm(Interest.Rate ~ fico * Loan.Length + Revolving.CREDIT.Balance + Open.CREDIT.Lines, data=loan.p)
summary(lm1)

## Get mean(or median) of int.rate in each FICO group
ir.mean <- tapply(loan.p$Interest.Rate, loan.p$FICO.Range, mean)
ir.median <- tapply(loan.p$Interest.Rate, loan.p$FICO.Range, median)
fico.range <- sort(unique(loan.p$fico))
plot(fico.range, ir.mean)
plot(fico.range, ir.median)

## Cut some numerical variables, e.g. Monthly.Income, Revolving.CREDIT.Balance, and see impact
moninc.cut <- cut2(loan.p$Monthly.Income, g=5)
crebal.cut <- cut2(loan.p$Revolving.CREDIT.Balance, g=5)
xyplot(Interest.Rate ~ fico | moninc.cut, data=loan.p, as.table=T, 
	panel=function(x,y,...)
		{
			panel.xyplot(x,y,...)
			fit <- lm(y ~ x)
			panel.abline(fit,col="red")
		}
)
plot(loan.p$fico, loan.p$Interest.Rate, col=moninc.cut, pch=19)
xyplot(Interest.Rate ~ fico | crebal.cut, data=loan.p, as.tabRle=T)
## observe a turn-point at fico=730 for the 30-month loans
## create a feature indicating the relation to the threshold
loan.p$fico.above.threshold <- as.factor(loan.p$fico > 730)

## int rate on income, grouped by FICO scores
xyplot( Interest.Rate ~ Monthly.Income | FICO.Range, data=loan.p, as.table=T, panel=function(x,y,...)
		{
			panel.xyplot(x,y,...)
			fit <- lm(y ~ x)
			panel.abline(fit,col="red")
		}
)

## Remove the FICO-group mean from ir
#ir.resid <- loan.p$Interest.Rate - sapply(loan.p$FICO.Range, function(x) ir.mean[x])
lm1 <- lm(Interest.Rate ~ fico, data=loan.p)
ir.resid <- lm1$residuals


## Search relations b/w ir.resid and other variables
xyplot(ir.resid ~ loan.p$Amount.Funded.By.Investors)
xyplot(ir.resid ~ loan.p$Amount.Requested) # as amount increases, rate adjustment increases.

bwplot(ir.resid ~ loan.p$Loan.Length) # longer loan length, more rate addon

bwplot(ir.resid ~ loan.p$Loan.Purpose, scales=list(rot=90)) # maybe not

xyplot(ir.resid ~ loan.p$Debt.To.Income.Ratio) # maybe not
plot(loan.p$Debt.To.Income.Ratio, ir.resid, col="blue")
dtir.fit <- lm(ir.resid ~ loan.p$Debt.To.Income.Ratio)
abline(dtir.fit) # again, maybe not

xyplot(ir.resid ~ log(loan.p$Monthly.Income)) # maybe not
debt <- loan.p$Monthly.Income*loan.p$Debt.To.Income.Ratio
xyplot(ir.resid ~ debt)
plot(log(loan.p$Monthly.Income), ir.resid, col="blue")
moninc.fit <- lm(ir.resid ~ log(loan.p$Monthly.Income))
abline(moninc.fit) # yes, higher income seems to indicate higher rate

ocl.mean <- tapply(ir.resid, as.factor(loan.p$Open.CREDIT.Lines), mean)
xyplot(ir.resid ~ jitter(loan.p$Open.CREDIT.Lines), 
	panel=function(x,y,...)
		{
			panel.xyplot(x,y,...)
			panel.points(ocl.mean ~ as.numeric(names(ocl.mean)), col="red", pch="-", cex=5)
		}
) 
bwplot(ir.resid ~ as.factor(loan.p$Open.CREDIT.Lines))
# credit lines matters

credit.limit <- loan.p$Open.CREDIT.Lines*loan.p$Monthly.Income
util <- loan.p$Revolving.CREDIT.Balance/credit.limit
xyplot(ir.resid ~ util)
plot(util, ir.resid, col="blue")
util.fit <- lm(ir.resid ~ util)
abline(util.fit)
plot(loan.p$Revolving.CREDIT.Balance, ir.resid, col="blue")
rcb.fit <- lm(ir.resid ~ loan.p$Revolving.CREDIT.Balance)
abline(rcb.fit)

inq6mon.mean <- tapply(ir.resid, as.factor(loan.p$Inquiries.in.the.Last.6.Months), mean)
xyplot(ir.resid ~ jitter(loan.p$Inquiries.in.the.Last.6.Months), 
	panel=function(x,y,...)
		{
			panel.xyplot(x,y,...)
			panel.points(inq6mon.mean ~ as.numeric(names(inq6mon.mean)), col="red", pch="-", cex=5)
		}
) 
bwplot(ir.resid ~ as.factor(loan.p$Inquiries.in.the.Last.6.Months))
# matters

bwplot(ir.resid ~ emp.len) # maybe not

## Monthly Income histogram
hist(loan.p$Monthly.Income, breaks=40, col="light blue", freq=T)

## Important variables can be Amount.Requested, Monthly.Income, Loan.Length, Credit.Lines, 
## Credit.Balance, Inquiries, alongside FICO scores

model1 <- lm(Interest.Rate ~ fico + Amount.Requested + Monthly.Income + Loan.Length 
		+ Open.CREDIT.Lines + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months, 
		data=loan.p)
model2 <- lm(Interest.Rate ~ fico + Amount.Requested + log(Monthly.Income) + Loan.Length 
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=loan.p)
model3 <- lm(Interest.Rate ~ fico * Loan.Length + fico * fico.above.threshold  + Amount.Requested + log(Monthly.Income)
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=loan.p)
model4 <- lm(Interest.Rate ~ fico * (1 + Loan.Length + Amount.Requested + log(Monthly.Income)
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months), 
		data=loan.p)
summary(model1)
summary(model2)
summary(model3) # choice
summary(model4)
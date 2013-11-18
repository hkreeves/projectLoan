##
## Cousera- Data Analysis by Jeff Leek
## Assignment 1 - Study of Loans
##
## Author: Kai He
##
## final code
##

library(lattice)
library(Hmsic)
library(RColorBrewer)

#download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv",
#		dest="./loansData.csv")
#loan <- read.csv("loansData.csv")
## import raw data
load("./data/loanRaw.rda") 

## copy data
## convert the classes of Interest.Rate, Debt.To.Income.Ratio from char/factor to numeric
loan.p <- loan
loan.p$Interest.Rate <- as.numeric(sub("%", "", loan$Interest.Rate))*0.01
loan.p$Debt.To.Income.Ratio <- as.numeric(sub("%", "", loan$Debt.To.Income.Ratio))*0.01

## FICO score extract (mid-point value is used)
scores <- unlist(strsplit(as.character(loan.p$FICO.Range), "-"))
scores <- apply(matrix(as.numeric(scores), byrow=T, ncol=2), 1, mean)
loan.p$fico <- scores

## missing value handling. For variables other than Employment.Length, Only two
## rows have NA's, and they have most variables as NA. Better remove them.
badpoints <- which(is.na(loan.p$Open.CREDIT.Lines))
## [1]  367 1595
loan.p <- loan.p[-badpoints,]

dim(loan.p)
## [1] 2498   14

sum(is.na(loan.p[,1:13]))
## 0

## split data into two subgroup according to Loan.Length
loan.short <- loan.p[loan.p$Loan.Length=="36 months",]
loan.long <- loan.p[loan.p$Loan.Length=="60 months",]
nrow(loan.short);nrow(loan.long)
## [1] 1950
## [1] 548

## add a variable indicating the FICO score threshold for each subgroup
## short: 730, long: 760
loan.short$fico.above.threshold <- as.factor(loan.short$fico > 730)
loan.long$fico.above.threshold <- as.factor(loan.long$fico > 760)
loan.p$fico.st.abv.thd <- as.factor(loan.p$fico > 730 & (loan.p$Loan.Length=="36 months"))
table(loan.short$fico.above.threshold)
## FALSE  TRUE 
##  1475   475 
table(loan.long$fico.above.threshold)
## FALSE  TRUE 
##   501    47 

## Models without adjustment
lm.short <- lm(Interest.Rate ~ fico * fico.above.threshold, data=loan.short)
summary(lm.short)
## ---------------------
##Residuals:
##      Min        1Q    Median        3Q       Max 
##-0.055782 -0.014968 -0.002892  0.012589  0.104373 
##
##Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
##(Intercept)                    9.093e-01  1.958e-02   46.45   <2e-16 ***
##fico                          -1.121e-03  2.831e-05  -39.60   <2e-16 ***
##fico.above.thresholdTRUE      -6.439e-01  3.832e-02  -16.80   <2e-16 ***
##fico:fico.above.thresholdTRUE  8.777e-04  5.184e-05   16.93   <2e-16 ***
##---
##Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##
##Residual standard error: 0.02139 on 1946 degrees of freedom
##Multiple R-squared:  0.6634,    Adjusted R-squared:  0.6629 
##F-statistic:  1279 on 3 and 1946 DF,  p-value: < 2.2e-16
## ---------------------

ir.res.short <- lm.short$residuals
## cut Amount.Requested into 5 groups
amt.break <- c(0, 5000, 10000, 20000, 30000, 40000)
amt.cut <- cut(loan.short$Amount.Requested, breaks=amt.break, dig.lab=5)

lm.long <- lm(Interest.Rate ~ fico * fico.above.threshold, data=loan.long)
summary(lm.long)
ir.res.long <- lm.long$residuals

## Models with adjustment
model.short <- lm(Interest.Rate ~ fico * fico.above.threshold  
		+ Amount.Requested + log10(Monthly.Income)
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=loan.short)
model.long <- lm(Interest.Rate ~ fico * fico.above.threshold  
		+ Amount.Requested + log10(Monthly.Income)
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=loan.long)
model.comb <- lm(Interest.Rate ~ fico * Loan.Length + fico * fico.st.abv.thd  
		+ Amount.Requested + log10(Monthly.Income)
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=loan.p)
summary(model.short)
## -------------------
##Residuals:
##      Min        1Q    Median        3Q       Max 
##-0.064498 -0.012161 -0.001877  0.010188  0.097353 
##
##Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
##(Intercept)                     9.508e-01  1.865e-02  50.974  < 2e-16 ***
##fico                           -1.152e-03  2.520e-05 -45.714  < 2e-16 ***
##fico.above.thresholdTRUE       -6.582e-01  3.380e-02 -19.473  < 2e-16 ***
##Amount.Requested                1.533e-06  7.165e-08  21.397  < 2e-16 ***
##log10(Monthly.Income)          -9.479e-03  2.148e-03  -4.414 1.07e-05 ***
##Open.CREDIT.Lines              -4.596e-04  9.984e-05  -4.604 4.42e-06 ***
##Inquiries.in.the.Last.6.Months  3.548e-03  3.457e-04  10.263  < 2e-16 ***
##fico:fico.above.thresholdTRUE   8.979e-04  4.573e-05  19.633  < 2e-16 ***
##---
##Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##
##Residual standard error: 0.01884 on 1942 degrees of freedom
##Multiple R-squared:  0.7393,    Adjusted R-squared:  0.7384 
##F-statistic: 786.9 on 7 and 1942 DF,  p-value: < 2.2e-16
## --------------------
summary(model.long)
summary(model.comb)

## Get mean values of Int Rate of each FICO score group
meanir.short <- tapply(loan.short$Interest.Rate, loan.short$FICO.Range, mean)
meanir.long <- tapply(loan.long$Interest.Rate, loan.long$FICO.Range, mean)


## final figure
## Auxiliary color function
## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha))  
}

## set par()
pdf(file = "./figure/finalfigure.pdf", height = 4, width = 3 * 4)
par(mar = c(2.5, 2.5, 2.1, 1.1), mgp = c(1.5, 0.5, 0))
par(mfrow=c(1,3))
## color
mycols <- brewer.pal(3, "RdYlBu")
mycols.alpha <- add.alpha(mycols, 0.6)

## panel 1
plot(loan.p$fico, loan.p$Interest.Rate, type="n", xlab="FICO Score", ylab="Interest Rate")
points(loan.short$fico, loan.short$Interest.Rate, col=mycols.alpha[1], pch=19)
points(loan.long$fico, loan.long$Interest.Rate, col=mycols.alpha[3], pch=15)

points(sort(unique(loan.short$fico)), meanir.short, col=rgb(0.95,0.1,0.1,1), pch="-", cex=3)
points(sort(unique(loan.short$fico))[!is.na(meanir.long)], meanir.long[!is.na(meanir.long)], 
	col=rgb(0.3,0.2,0.8,1), pch="-", cex=3)
legend(780, 0.25, c("36 Months", "60 Months"), pch=c(19,15), col=mycols.alpha[c(1,3)], bty="o")
mtext("Short Term vs. Long Term", line=0.5)
text(650, 0.06, "(a)")

## panel 2
palette(brewer.pal(5, "RdYlBu"))
plot(loan.short$fico, ir.res.short, col=amt.cut, pch=19, xlab="FICO Score", 
	ylab="Residuals of Interest Rate (36 Months)")
legend(775, 0.105, levels(amt.cut), pch=19, col=1:5, bty="o", title="Loan Amount (USD)")
mtext("Model without Adjustment", line=0.5)
text(820, -0.05, "(b)")

## panel 3
plot(loan.short$fico, model.short$residuals, col=amt.cut, pch=19, xlab="FICO Score", 
	ylab="Residuals of Interest Rate (36 Months)")
legend(775, 0.098, levels(amt.cut), pch=19, col=1:5, bty="o", title="Loan Amount (USD)")
mtext("Full Model", line=0.5)
text(820, -0.06, "(c)")
dev.off()

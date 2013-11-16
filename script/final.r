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
loan.long$fico.above.threshold <- as.factor(loan.long$fico > 765)
table(loan.short$fico.above.threshold)
## FALSE  TRUE 
##  1475   475 
table(loan.long$fico.above.threshold)
## FALSE  TRUE 
##   501    47 

model.short <- lm(Interest.Rate ~ fico * fico.above.threshold  
		+ Amount.Requested + log10(Monthly.Income)
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=loan.short)
model.long <- lm(Interest.Rate ~ fico * fico.above.threshold  
		+ Amount.Requested + log10(Monthly.Income)
		+ Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
		data=loan.long)
summary(model.short)
summary(model.long)

meanir.short <- tapply(loan.short$Interest.Rate, loan.short$FICO.Range, mean)
meanir.long <- tapply(loan.long$Interest.Rate, loan.long$FICO.Range, mean)

mycols <- brewer.pal(3, "PuOr")
## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha))  
}
mycols.alpha <- add.alpha(mycols, 0.4)

plot(loan.p$fico, loan.p$Interest.Rate, type="n", xlab="FICO score", ylab="Interest Rate")
points(loan.short$fico, loan.short$Interest.Rate, col=mycols.alpha[1], pch=19)
points(loan.long$fico, loan.long$Interest.Rate, col=mycols.alpha[3], pch=15)

points(sort(unique(loan.short$fico)), meanir.short, col=rgb(0.9,0.2,0.1,0.68), pch=19, cex=1.5)
points(sort(unique(loan.short$fico))[!is.na(meanir.long)], meanir.long[!is.na(meanir.long)], 
	col=rgb(0.3,0.2,0.7,0.68), pch=15, cex=1.5)
legend(790, 0.25, c("36 months", "60 months"), pch=c(19,15), col=mycols.alpha[c(1,3)], bty="n")

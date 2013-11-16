##
## Cousera- Data Analysis by Jeff Leek
## Assignment 1 - Study of Loans
##
## Author: Kai He
##
## Raw Data Processing
##

download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv",
		dest="./loansData.csv")

data.download.date <- date()
data.download.date
## [1] "Mon Nov 11 17:18:32 2013"

loan <- read.csv("loansData.csv")

## peek at the data

dim(loan)
## [1] 2500   14

head(loan, 3)
##      Amount.Requested Amount.Funded.By.Investors Interest.Rate Loan.Length       Loan.Purpose
##81174            20000                      20000         8.90%   36 months debt_consolidation
##99592            19200                      19200        12.12%   36 months debt_consolidation
##80059            35000                      35000        21.98%   60 months debt_consolidation
##      Debt.To.Income.Ratio State Home.Ownership Monthly.Income FICO.Range Open.CREDIT.Lines
##81174               14.90%    SC       MORTGAGE        6541.67    735-739                14
##99592               28.36%    TX       MORTGAGE        4583.33    715-719                12
##80059               23.81%    CA       MORTGAGE       11500.00    690-694                14
##      Revolving.CREDIT.Balance Inquiries.in.the.Last.6.Months Employment.Length
##81174                    14272                              2          < 1 year
##99592                    11140                              1           2 years
##80059                    21977                              1           2 years

sapply(loan[1,], class)
##              Amount.Requested     Amount.Funded.By.Investors                  Interest.Rate 
##                     "integer"                      "numeric"                       "factor" 
##                   Loan.Length                   Loan.Purpose           Debt.To.Income.Ratio 
##                      "factor"                       "factor"                       "factor" 
##                         State                 Home.Ownership                 Monthly.Income 
##                      "factor"                       "factor"                      "numeric" 
##                    FICO.Range              Open.CREDIT.Lines       Revolving.CREDIT.Balance 
##                      "factor"                      "integer"                      "integer" 
##Inquiries.in.the.Last.6.Months              Employment.Length 
##                     "integer"                       "factor" 

## copy data
## convert the classes of Interest.Rate, Debt.To.Income.Ratio from char/factor to numeric
loan.p <- loan
loan.p$Interest.Rate <- as.numeric(sub("%", "", loan$Interest.Rate))*0.01
loan.p$Debt.To.Income.Ratio <- as.numeric(sub("%", "", loan$Debt.To.Income.Ratio))*0.01

## get a view of missing values
apply(loan.p, 2, function(x) sum(is.na(x)))
##            Amount.Requested     Amount.Funded.By.Investors                  Interest.Rate 
##                             0                              0                              0 
##                   Loan.Length                   Loan.Purpose           Debt.To.Income.Ratio 
##                             0                              0                              0 
##                         State                 Home.Ownership                 Monthly.Income 
##                             0                              0                              1 
##                    FICO.Range              Open.CREDIT.Lines       Revolving.CREDIT.Balance 
##                             0                              2                              2 
##Inquiries.in.the.Last.6.Months              Employment.Length 
##                             2                             77 

## find out the common rows of missing values -- 367, 1595. Consider removing them entirely for the following study.
which(is.na(loan.p$Open.CREDIT.Lines))
## [1]  367 1595
which(is.na(loan.p$Revolving.CREDIT.Balance))
## [1]  367 1595
which(is.na(loan.p$Inquiries.in.the.Last.6.Months))
## [1]  367 1595
which(is.na(loan.p$Monthly.Income))
## [1] 367

## two outliers in the Monthly.Income (> 50000)
which(loan.p$Monthly.Income > 50000)
## [1]  268 1852

## [optional] remove "bad points"
badpoints <- c(which(is.na(loan.p$Open.CREDIT.Lines)), which(loan.p$Monthly.Income > 50000))
loan.p <- loan.p[-badpoints,]

## FICO score extract (mid-point value is used)
scores <- unlist(strsplit(as.character(loan.p$FICO.Range), "-"))
scores <- apply(matrix(as.numeric(scores), byrow=T, ncol=2), 1, mean)
loan.p$fico <- scores

## Reorder Employment.Length levels
loan.p$Employment.Length <- factor(loan$Employment.Length, levels(loan.p$Employment.Length)[c(1,2,4:12,3)])

## save processed data
save(data.download.date, loan, loan.p, file="./loanRaw.rda")
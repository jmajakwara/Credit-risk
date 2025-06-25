rm(list=ls())
library(MASS)
library(caret)
library(reticulate)
library(data.table)
library(survival)
library(Hmisc)
#library(BART)
library(rsample)
#library(SurvMetrics)
library(prodlim)
library(pec)
library(rms)
library(cuRe)
library(flexsurv)
library(flexsurvcure)
library(tidymodels)

##### Housekeeping ######
options(expressions=10000)
R.version.string
date()
Sys.time()
options(digits=6)
options(scipen=6)


set.seed(1900)
loans <- read.csv("loans.csv",header=TRUE,sep=",",na.strings = "NA")

loans$time <- as.numeric(loans$time)
loans$OpenRevolvingMonthlyPayment <- as.numeric(loans$OpenRevolvingMonthlyPayment)
loans$EmploymentStatusDuration <- as.numeric(loans$EmploymentStatusDuration)
loans$AmountDelinquent <- as.numeric(loans$AmountDelinquent)
loans$RevolvingCreditBalance <- as.numeric(loans$RevolvingCreditBalance)
loans$AvailableBankcardCredit <- as.numeric(loans$AvailableBankcardCredit)
loans$LoanOriginalAmount <- as.numeric(loans$LoanOriginalAmount)
loans$ListingCategory <- as.factor(loans$ListingCategory)
loans$EmploymentStatus <- as.factor(loans$EmploymentStatus)
loans$CreditScoreRange <- as.factor(loans$CreditScoreRange)
#loans$CreditScoreRange <- relevel(loans$CreditScoreRange, ref = "Fair")
loans$Occupation <- as.factor(loans$Occupation)
#loans <- loans  %>% mutate_if(is.character, as.factor)
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
loans$ProsperScore <- ifelse(loans$ProsperScore == 11,10,loans$ProsperScore)
loans$ProsperScore <- as.factor(loans$ProsperScore)
loans$Term <- as.factor(loans$Term)
loans$IncomeVerifiable <- as.factor(loans$IncomeVerifiable)

# convert days to months


loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
loans <- loans %>% dplyr::select(-2,-Occupation,-ProsperScore)

loans_cc <- loans  %>% na.omit()

loans_cc <- loans_cc %>% select(time, status, OpenCreditLines, InquiriesLast6Months, AmountDelinquent,  BankcardUtilization, DebtToIncomeRatio,  BorrowerRate,AvailableBankcardCredit,
				CurrentlyInGroup, CreditScoreRange, Term, TradesNeverDelinquent, DelinquenciesLast7Years, CurrentDelinquencies, IsBorrowerHomeowner)
loans <- loans %>% select(-ProsperRating,-LenderYield,-CurrentCreditLines)



data_split <- initial_split(loans_cc, prop = 0.7, strata = status)     
#Create data frames for the two sets:
loans_train <- training(data_split)
loans_test  <- testing(data_split)

loans_train_all <- loans[!(loans$LoanKey %in% c(loans_test$LoanKey)),]


loans_train$Term <- as.factor(loans_train$Term)
loans_test$Term <- as.factor(loans_test$Term)

loans_test <- loans_test  %>%  select(time:Term)
loans_train <- loans_train  %>%  select(time:Term)
loans_train_all <- loans_train_all  %>%  select(time:Term)


###### Lognormal Modeling ########

loans_12 <- loans_cc %>% filter(Term == 12)
loans_12 <- as.data.frame(loans_12)
loans_36 <- loans_cc %>% filter(Term == 36)
loans_36 <- as.data.frame(loans_36)
loans_60 <- loans_cc %>% filter(Term == 60)
loans_60 <- as.data.frame(loans_60)

cat('The Generalised-gamma cure Model using flexsurvcure package  \n')

ggam <- flexsurvcure::flexsurvcure(Surv(time,status) ~  1, 
							anc = list(mu = ~ OpenCreditLines  + BorrowerRate + Term  + BankcardUtilization + TradesNeverDelinquent +
							DebtToIncomeRatio + InquiriesLast6Months + IsBorrowerHomeowner + CreditScoreRange), 
    							data = loans_cc,
    							dist = 'gengamma',
    							inits = c(0.65, 6.5,  0.7, -1.1),
    							fixedpars = 4,
    							link = 'logistic',
    							mixture = TRUE)

tidy(ggam)
glance(ggam)
glance(ggam)[['logLik']]

ggam1 <- flexsurvcure::flexsurvcure(Surv(time,status) ~ InquiriesLast6Months + CreditScoreRange, 
							anc = list(mu = ~ OpenCreditLines  + BorrowerRate + Term  + BankcardUtilization + TradesNeverDelinquent +
							DebtToIncomeRatio + IsBorrowerHomeowner),   
    							data = loans_cc, 
    							dist = 'gengamma',
    							inits = c(0.65, 6.5, 0.7, -1.1),
    							fixedpars = 4,
    							link = 'logistic',
    							mixture = TRUE)


tidy(ggam1)
glance(ggam1)
glance(ggam1)[['logLik']]
-2*(glance(ggam)[['logLik']]-glance(ggam1)[['logLik']])
pchisq(-2*(glance(ggam)[['logLik']]-glance(ggam1)[['logLik']]),df=2, lower.tail=FALSE)

#pdf(file="Surval of cure models with and without covariates.pdf")
#plot(ggam1,col=1,ylab="Survival probability",xlab="Time to default (days)")
#plot(ggam,add=TRUE,lty=2,col=1)
#legend("topright",legend=c("With covariates","No-covariates"),lty=1:2)
#dev.off()

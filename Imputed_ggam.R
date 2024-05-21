rm(list=ls())
library(MASS)
library(ggplot2)
library(caret)
library(reticulate)
library(data.table)
library(survival)
library(flexsurv)
library(lattice)
library(Hmisc)
library(pan)
library(mice)
library(miceadds)
#library(BART)
#library(timeROC)
#library(SurvMetrics)
library(pec)
library(rsample)
library(rms)
library(nlme)
library(nnet)
library(prodlim)
library(rstpm2)
library(splines)
library(cuRe)
library(flexsurvcure)
library(tidymodels)



##### Housekeeping ######
options(expressions=100000)
R.version.string
date()
Sys.time()
options(digits=12)
options(scipen=12)



set.seed(1900)
loans <- read.csv("loans.csv",header=TRUE,sep=",",na.strings = "NA")
loans$time <- as.numeric(loans$time)
loans$OpenRevolvingMonthlyPayment <- as.numeric(loans$OpenRevolvingMonthlyPayment)
loans$EmploymentStatusDuration <- as.numeric(loans$EmploymentStatusDuration)
loans$AmountDelinquent <- as.numeric(loans$AmountDelinquent)
loans$OpenRevolvingMonthlyPayment <- as.numeric(loans$OpenRevolvingMonthlyPayment)
loans$RevolvingCreditBalance <- as.numeric(loans$RevolvingCreditBalance)
loans$AvailableBankcardCredit <- as.numeric(loans$AvailableBankcardCredit)
loans$LoanOriginalAmount <- as.numeric(loans$LoanOriginalAmount)
loans$StatedMonthlyIncome <- as.numeric(loans$StatedMonthlyIncome)
loans$LoanCurrentDaysDelinquent <- as.numeric(loans$LoanCurrentDaysDelinquent)
loans$ListingCategory <- as.factor(loans$ListingCategory)
loans$EmploymentStatus <- as.factor(loans$EmploymentStatus)
loans$CreditScoreRange <- as.factor(loans$CreditScoreRange)
loans$Occupation <- as.factor(loans$Occupation)
loans$Term <- as.factor(loans$Term)

# convert days to months
loans <- loans %>% mutate(month = round(time/30.4375, digit=0))


# Removing cases whose debt to income ratio is more than 100
loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))

loans <- loans %>% dplyr::select(-2,-Occupation,-ProsperScore)




# Removing cases whose debt to income ratio is more than 100


# loans <- loans %>% mutate_if(is.character, as.factor)
# loans %>%  summarize_all(funs(sum(is.na(.))))/dim(loans)[1]*100

loans_cc <- loans %>% na.omit()




# Split the data into a test and training set
# following https://www.tidymodels.org/start/recipes/#recipe

data_split <- initial_split(loans_cc, prop = 0.70, strata = status)     
#Create data frames for the two sets:
loans_train <- training(data_split)
loans_test  <- testing(data_split)

loans_train_all <- loans[!(loans$LoanKey %in% c(loans_test$LoanKey)),]


loans_train_all <- loans_train_all %>% select(-ProsperRating,-CurrentCreditLines,-LenderYield,-LoanKey,
                      -LP_CustomerPayments,-LP_CustomerPrincipalPayments,-LP_InterestandFees,-LP_ServiceFees,-LP_CollectionFees,-LP_GrossPrincipalLoss,
                      -LP_NetPrincipalLoss,-LP_NonPrincipalRecoverypayments,-Recommendations,-InvestmentFromFriendsCount,-InvestmentFromFriendsAmount,
                      -Investors,-BorrowerState,-TotalCreditLinespast7years,-PublicRecordsLast12Months,-TotalInquiries,-IncomeVerifiable,
                      -PercentFunded,-ListingCategory,-EstimatedEffectiveYield,-EstimatedLoss,-EstimatedReturn,-EmploymentStatusDuration)

loans_test <- loans_test %>% select(-ProsperRating,-CurrentCreditLines,-LenderYield,-LoanKey,
                      -LP_CustomerPayments,-LP_CustomerPrincipalPayments,-LP_InterestandFees,-LP_ServiceFees,-LP_CollectionFees,-LP_GrossPrincipalLoss,
                      -LP_NetPrincipalLoss,-LP_NonPrincipalRecoverypayments,-Recommendations,-InvestmentFromFriendsCount,-InvestmentFromFriendsAmount,
                      -Investors,-BorrowerState,-TotalCreditLinespast7years,-PublicRecordsLast12Months,-TotalInquiries,-IncomeVerifiable,
                      -PercentFunded,-ListingCategory,-EstimatedEffectiveYield,-EstimatedLoss,-EstimatedReturn,-EmploymentStatusDuration) 

loans <- loans %>% select(-ProsperRating,-CurrentCreditLines,-LenderYield,-LoanKey,
                      -LP_CustomerPayments,-LP_CustomerPrincipalPayments,-LP_InterestandFees,-LP_ServiceFees,-LP_CollectionFees,-LP_GrossPrincipalLoss,
                      -LP_NetPrincipalLoss,-LP_NonPrincipalRecoverypayments,-Recommendations,-InvestmentFromFriendsCount,-InvestmentFromFriendsAmount,
                      -Investors,-BorrowerState,-TotalCreditLinespast7years,-PublicRecordsLast12Months,-TotalInquiries,-IncomeVerifiable,
                      -PercentFunded,-ListingCategory,-EstimatedEffectiveYield,-EstimatedLoss,-EstimatedReturn,-EmploymentStatusDuration, -EmploymentStatus, -StatedMonthlyIncome) 




######################################### MICE IMPUTATION #####################################

# Select columns with missing values

cat('Summarising missing variables \n')

ini <- mice(loans,maxit=0)

predict <- mice(loans,maxit=0)
pred <- predict$pred

meth <- predict$meth

meth["DebtToIncomeRatio"] <- "pmm"

n=20
#imp <- mice(loans_train_all, m=n,seed=1974,method=meth,predictorMatrix=quickpred(loans_train_all, mincor=0.1),printFlag=FALSE,maxit=40,ridge=0.01)
imp <- mice(loans, m=n,seed=1900,method=meth,predictorMatrix=quickpred(loans, mincor=0.2), max=1, printFlag=FALSE,maxit=40)#,ridge=0.01)

imp$loggedEvents

#pdf(file="Lognormal_imputations_plot_cured_pmm.pdf")
#plot(imp)	


#pdf(file="Lognormal_densityplot_cured_pmm.pdf")
#densityplot(imp)		
#dev.off()




ggam_flex <- with(imp, flexsurvcure::flexsurvcure(Surv(time,status) ~ InquiriesLast6Months + LoanCurrentDaysDelinquent, 
							anc = list(mu = ~ OpenCreditLines + InquiriesLast6Months  +  LoanCurrentDaysDelinquent + Term +
							DebtToIncomeRatio + CreditScoreRange + BorrowerRate + BankcardUtilization + CurrentlyInGroup + TradesNeverDelinquent), 
    							data = complete(imp),
    							dist = 'gengamma',
    							inits = c(0.9, 7.5, 0.7, -0.4),
    							fixedpars = 4,
    							link = 'logistic',
    							mixture = TRUE)
		)

broom::tidy(pool(ggam_flex))
summary(pool(ggam_flex))
glance(pool(ggam_flex))

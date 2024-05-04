#### Multicollinearity #####
library(MASS)
library(tidyverse)
library(caret)
library(data.table)
library(survival)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(survminer)
library(rsample)
library(flexsurv)
library(tidymodels)
library(censored)
library(prodlim)

filename = "loans_final_jj.csv"
loans <- read.table(filename,header=TRUE,sep=",",na.strings = "NA")
#loans <- read.csv("Data/loans_final_jj.csv",header=TRUE,sep=",",na.strings = "NA")
loans$time <- as.numeric(loans$time)
loans$OpenRevolvingMonthlyPayment <- as.numeric(loans$OpenRevolvingMonthlyPayment)
loans$EmploymentStatusDuration <- as.numeric(loans$EmploymentStatusDuration)
loans$AmountDelinquent <- as.numeric(loans$AmountDelinquent)
loans$OpenRevolvingMonthlyPayment <- as.numeric(loans$OpenRevolvingMonthlyPayment)
loans$RevolvingCreditBalance <- as.numeric(loans$RevolvingCreditBalance)
loans$AvailableBankcardCredit <- as.numeric(loans$AvailableBankcardCredit)
loans$LoanOriginalAmount <- as.numeric(loans$LoanOriginalAmount)
loans$ListingCategory <- as.factor(loans$ListingCategory)
loans$EmploymentStatus <- as.factor(loans$EmploymentStatus)
loans$CreditScoreRange <- as.factor(loans$CreditScoreRange)
loans$Occupation <- as.factor(loans$Occupation)


# convert days to months
#loans <- loans %>% mutate(month = round(time/30.4375, digit=0))

#term_adj_fctn <- function(df, t) {
#  term  <-  df$Term == t
#  df <- df[term, ]
  # plot frequncy graph
  #barplot(table(df$month) / sum(table(df$month)))
  
#  maxtime <- t
#  max_term  <-  df$month <= maxtime
#  df <- df[max_term, ]
#}


#loans_12m <- term_adj_fctn(loans, 12)
#loans_36m <- term_adj_fctn(loans, 36)
#loans_60m <- term_adj_fctn(loans, 60)

# combine adjusted datasets
#loans <- rbind(loans_12m, loans_36m, loans_60m)
#loans$time <- round(loans$time/30.417,0)

# Removing cases whose debt to income ratio is more than 100
loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))

loans <- loans %>% dplyr::select(-2,-Occupation,-ProsperScore)

#loans <- loans %>% dplyr::select(time : MonthlyLoanPayment)

dim(loans)
# loans <- loans %>% mutate_if(is.character, as.factor)
# loans %>%  summarize_all(funs(sum(is.na(.))))/dim(loans)[1]*100

loans_cc <- loans %>% na.omit()

# Scaling the numerical variables using caret package



#train_index <- createDataPartition(loans_cc$status,p=0.7,list=FALSE)
#loans_train <- loans_cc[train_index,]
#loans_test <- loans_cc[-train_index,]



# Split the data into a test and training set
# following https://www.tidymodels.org/start/recipes/#recipe
set.seed(1974)
library(rsample)
data_split <- initial_split(loans_cc, prop = 0.70, strata = status)     
#Create data frames for the two sets:
loans_train <- training(data_split)
loans_test  <- testing(data_split)

loans_train_all <- loans[!(loans$LoanKey %in% c(loans_test$LoanKey)),]

#loans_train_all <- loans_train_all %>% select(-1)

loans_train <- loans_train %>% dplyr::select(time : MonthlyLoanPayment)
loans_test <- loans_test %>% dplyr::select(time : MonthlyLoanPayment)

## Partioning of data with missingnes
loans_missing <- loans[!(loans$LoanKey %in% c(loans_cc$LoanKey)),]
dim(loans_cc)
dim(loans_missing)
## Variables aliased (Highly correlated > 0.5)
##  LenderYield and BorrowRate; OppenCreditLines and CurrentCreditLines;OpenRevolvingAccounts and Current CreditLines;
##   ProsperScore and ProsperRating; BorrowerRate and BorrowerAPR, TradesNeverDelinquent and DelinquenciesLast7Years
##   CurrentlyInGroup and IsBorrowerHomeowner,OpenRevolvingAccounts and OpenRevolvingMonthlyPayment#"ProsperScore",
highly <- loans_cc %>% 
            dplyr::select("LenderYield","BorrowerRate","OpenCreditLines","CurrentCreditLines","OpenRevolvingAccounts","BorrowerAPR","OpenRevolvingAccounts","ProsperRating","DebtToIncomeRatio","StatedMonthlyIncome","EmploymentStatusDuration","InquiriesLast6Months","TotalInquiries","AmountDelinquent","DelinquenciesLast7Years","CurrentDelinquencies") %>%  cor()
highly 

#After removing variables highly correlated; PropserRating, CurrentCreditLines, LenderYield ,"OpenRevolvingAccounts","TotalInquiries","ProsperScore",

var_sel <- c("time","status","BorrowerRate","EmploymentStatusDuration","OpenCreditLines",
        "TotalCreditLinespast7years","InquiriesLast6Months","Term","AmountDelinquent","TradesOpenedLast6Months","DelinquenciesLast7Years", "RevolvingCreditBalance","PublicRecordsLast10Years", "BankcardUtilization","CurrentlyInGroup","OpenRevolvingAccounts", "DebtToIncomeRatio","LoanOriginalAmount","TradesNeverDelinquent","PercentFunded", "EmploymentStatus", 
       "LoanCurrentDaysDelinquent","IsBorrowerHomeowner","CreditScoreRange","CurrentDelinquencies","StatedMonthlyIncome")

loans_train <- loans_train %>% dplyr::select(all_of(var_sel))



# fit survival model
library(survival)
cox_model <- coxph(Surv(time, status) ~ ., data = loans_train, y=TRUE, x = TRUE)

# calculate VIF for each predictor variable; VIF greater than 5 indicates significant multicollinearity.
library(car)
cat("calculate VIF for each predictor variable, might need to consider those VIF > 2.5")
vif(cox_model)


## # perform PCA on predictor variables

#cat("Performing PCA on scaled predictor numerical variables")
#pca <- prcomp(loans_train[, c(3:16)], center = TRUE, scale = TRUE)
#summary(pca)

#plot(pca, type = "l", ylim=c(0,4) )
#abline(h=0)
#abline(v=0)

#pca$rotation[,1:7]

#table(loans_cc$InquiriesLast6Months_cat)
loans_cc <- loans_cc |> mutate(InquiriesLast6Months_cat = ifelse(InquiriesLast6Months == 0, 0,
                                              ifelse(InquiriesLast6Months == 1 | InquiriesLast6Months == 2,1,2)))


fit.surv <- survfit(Surv(time, status) ~ InquiriesLast6Months_cat, data = loans_cc)
ggsurvplot(fit.surv, conf.int = FALSE, 
           xlab = "Time (in days)",
           ylab = "Survival probability",
           linetype =  "strata",
           surv.plot.height = 1.2,
         #  palette = "jco",
           ggtheme =  theme_light(),
           legend = "right",
           legend.title = "Credit score ranges",
           linewidth = 0.4,
           size = 1)


summary(survfit(Surv(time, status) ~ 1, data = loans_cc), times = c(1,365, median(loans_cc$time), 3*365, max(loans_cc$time)))



### Fitting lognormal to data
hist(loans_cc$BorrowerAPR)
fit <- fitdistr(loans_cc$BorrowerAPR, "lognormal")
fit$estimate
fit$sd

hist((loans_cc[loans_cc$status==1])$time)
obs <- dplyr::filter(loans_cc,status==1)
cens <- dplyr::filter(loans_cc,status==0)
dim(loans_cc)
dim(obs)
dim(cens)
hist(obs$time)
hist(cens$time)

## Fiting lognormal distribution to cases experience the event of interest
fit <- fitdistr(obs$time, "lognormal")
fit$estimate
fit$sd

fit1 <- fitdistr(cens$time, "lognormal")
fit1$estimate
fit1$sd

# Fitting DTIR
hist(loans_cc$CurrentDelinquencies)
fit <- fitdistr(loans_cc$CurrentDelinquencies +0.00000001, "lognormal")
fit$estimate
fit$sd

fit1 <- fitdistr(loans_cc$DebtToIncomeRatio, "lognormal")
fit1$estimate
fit1$sd

table((loans_cc$Term)[loans_cc$Term==36])


rm(list=ls())
library(MASS)
library(caret)
library(reticulate)
#library(tidyverse)
library(data.table)
library(survival)
library(Hmisc)
library(rsample)
library(SurvMetrics)
library(rms)
library(prodlim)
library(pec)
library(censored)
library(tidymodels)
library(BART)

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
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
#loans <- loans  %>% mutate_if(is.character, as.factor)
loans$CreditScoreRange <- relevel(loans$CreditScoreRange, ref = "Fair")
loans$ProsperScore <- ifelse(loans$ProsperScore == 11,10,loans$ProsperScore)
loans$ProsperScore <- as.factor(loans$ProsperScore)
loans$Term <- as.factor(loans$Term)

table(loans$status)
A=loans %>% filter(Term==12 & time > 365.25)
hist(A$time)
table(A$status)
# convert days to months
#loans <- loans %>% mutate(time = round(time/30.417, digit=0))


loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
loans <- loans %>% dplyr::select(-2,-Occupation)

loans_cc <- loans  %>% na.omit()




###### Lognormal Modeling ########


# cor(cbind(loans$TradesNeverDelinquent,loans$DelinquenciesLast7Years,loans$LoanCurrentDaysDelinquent,loans$DelinquenciesLast7Years))
# table(loans$CurrentlyInGroup,loans$IsBorrowerHomeowner)
cat('The Lognormal Model using selected variables output \n')

loans_cc <- loans_cc |>
  mutate(event_time = Surv(time, status))


split   <- initial_split(loans_cc, prop = 0.7, strata = status)
loans_tr  <- training(split)
loans_val <- testing(split)

log_spec <- 
  survival_reg(dist = "lnorm") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
log_spec


log_fit <- log_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + BorrowerRate +  BankcardUtilization + 
                                          	DebtToIncomeRatio +  CurrentlyInGroup + CreditScoreRange + Term, 
						data = loans_tr)

log_fit1 <- log_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + BorrowerRate + BankcardUtilization + 
                                          	DebtToIncomeRatio +   CurrentlyInGroup + CreditScoreRange + Term, 
						data = loans_cc)


log_fit
tidy(log_fit)
cat("Results from training set \n")
glance(log_fit)
cat("Results from CC set \n")
tidy(log_fit1)
glance(log_fit1)


time_points <- sort(unique(loans_val$time))
log_pred <- augment(log_fit, loans_val, eval_time = time_points)
brier_scores <-
  log_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_scores$.estimate)


IBS_log <- log_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_log

roc_scores <-
  log_pred |> 
  roc_auc_survival(truth = event_time, .pred)



log_cindex <- log_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)
log_cindex

### Cross-validation lognormal
loans_folds <- vfold_cv(loans_cc, v = 5, strata = status)

loans_folds$splits[[1]] |> analysis() |> dim()

cat("Cross validation \n ")
log_cross_val <- log_spec |> fit_resamples(event_time ~ OpenCreditLines + InquiriesLast6Months + BorrowerRate +  BankcardUtilization + 
                                          	DebtToIncomeRatio +  CurrentlyInGroup + CreditScoreRange + Term,
						resamples = loans_folds, 
						eval_time = sort(unique(loans_cc$time)), 
						metrics = metric_set(concordance_survival,brier_survival,brier_survival_integrated))

log_metric = collect_metrics(log_cross_val)

tail(log_metric)

cat("The IBS is = ")
collect_metrics(log_cross_val) |> 
  filter(.metric == "brier_survival_integrated")
cat("The c-index is = ")
collect_metrics(log_cross_val) |> 
  filter(.metric == "concordance_survival")


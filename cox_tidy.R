rm(list=ls())
library(MASS)
library(caret)
library(reticulate)
library(data.table)
library(survival)
library(ranger)
library(BART)
library(Hmisc)
library(censored)
library(tidymodels)
library(rsample)
library(SurvMetrics)
library(prodlim)
library(eha)
library(pec)
library(rms)


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
#loans$ListingCategory <- as.factor(loans$ListingCategory)
loans$EmploymentStatus <- as.factor(loans$EmploymentStatus)
loans$CreditScoreRange <- as.factor(loans$CreditScoreRange)
loans$Occupation <- as.factor(loans$Occupation)
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
#loans$CreditScoreRange <- relevel(loans$CreditScoreRange, ref = "Fair")
loans$ProsperScore <- ifelse(loans$ProsperScore == 11,10,loans$ProsperScore)
loans$ProsperScore <- as.factor(loans$ProsperScore)
loans$Term <- as.factor(loans$Term)

#loans <- loans  %>% mutate_if(is.character, as.factor)

# convert days to months
#loans <- loans %>% mutate(month = round(time/30.417, digit=0))



loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
loans <- loans %>% dplyr::select(-2,-Occupation)


loans_cc <- loans  %>% na.omit()



###### Cox Modeling ########

cat('The Cox Model using selected variables output \n')

loans_cc <- loans_cc |>
  mutate(event_time = Surv(time, status))


split   <- initial_split(loans_cc, prop = 0.7, strata = status)
loans_tr  <- training(split)
loans_val <- testing(split)

cox_spec <- 
  proportional_hazards() |>
  set_engine("survival") |>
  set_mode("censored regression") 
cox_spec


cat('The Cox Model on all variables output \n')

cox_fit <- cox_spec |> fit(Surv(time, status) ~ BorrowerRate +  OpenCreditLines + InquiriesLast6Months +  
                                        BankcardUtilization + DebtToIncomeRatio + IsBorrowerHomeowner + CreditScoreRange + Term,
					data = loans_tr)

cox_fit1 <- cox_spec |> fit(Surv(time, status) ~ BorrowerRate +  OpenCreditLines + InquiriesLast6Months + 
                                        BankcardUtilization + DebtToIncomeRatio + IsBorrowerHomeowner + CreditScoreRange + Term,
					data = loans_cc)



cox_fit
tidy(cox_fit)
cat("Results from training set \n")
cat("The number of observations = ")
(glance(cox_fit))[["n"]]
cat("The number of events = ")
(glance(cox_fit))[["nevent"]]
cat("The logLik = ")
(glance(cox_fit))[["logLik"]]
cat("The AIC = ")
(glance(cox_fit))[["AIC"]]
cat("The BIC = ")
(glance(cox_fit))[["BIC"]]
#royston(cox_fit)

cat("Results from CC set \n")
tidy(cox_fit1)
cat("The number of observations = ")
(glance(cox_fit1))[["n"]]
cat("The number of events = ")
(glance(cox_fit1))[["nevent"]]
cat("The logLik = ")
(glance(cox_fit1))[["logLik"]]
cat("The AIC = ")
(glance(cox_fit1))[["AIC"]]
cat("The BIC = ")
(glance(cox_fit1))[["BIC"]]
#royston(cox_fit1)


time_points <- sort(unique(loans_val$time))
cox_pred <- augment(cox_fit, loans_val, eval_time = time_points)

brier_scores <-
  cox_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_scores$.estimate)


IBS_cox <- cox_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_cox

roc_scores <-
  cox_pred |> 
  roc_auc_survival(truth = event_time, .pred)



cox_cindex <- cox_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)
cox_cindex

### Cross-validation Cox PH
loans_folds <- vfold_cv(loans_cc, v = 5, strata = status)

loans_folds$splits[[1]] |> analysis() |> dim()

cat("Cross validation \n")
cox_cross_val <- cox_spec |> fit_resamples(Surv(time, status) ~  BorrowerRate +  OpenCreditLines + InquiriesLast6Months + 
                                        			AmountDelinquent +  BankcardUtilization + DebtToIncomeRatio + 
                                        			IsBorrowerHomeowner + CreditScoreRange + Term,
								resamples = loans_folds, 
								eval_time = sort(unique(loans_cc$time)), 
								metrics = metric_set(concordance_survival,brier_survival,brier_survival_integrated))

cox_metric = collect_metrics(cox_cross_val)
tail(cox_metric)
cat("The IBS is = ")
collect_metrics(cox_cross_val) |> 
  filter(.metric == "brier_survival_integrated")
cat("The c-index is = ")
collect_metrics(cox_cross_val) |> 
  filter(.metric == "concordance_survival")




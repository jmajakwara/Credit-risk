rm(list=ls())
library(MASS)
library(caret)
library(reticulate)
library(data.table)
library(survival)
library(Hmisc)
library(BART)
library(flexsurv)
library(rsample)
library(DynNom)
library(SurvMetrics)
library(prodlim)
library(eha)
library(pec)
library(rms)
library(censored)
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
loans$OpenRevolvingMonthlyPayment <- as.numeric(loans$OpenRevolvingMonthlyPayment)
loans$RevolvingCreditBalance <- as.numeric(loans$RevolvingCreditBalance)
loans$AvailableBankcardCredit <- as.numeric(loans$AvailableBankcardCredit)
loans$LoanOriginalAmount <- as.numeric(loans$LoanOriginalAmount)
loans$ListingCategory <- as.factor(loans$ListingCategory)
loans$EmploymentStatus <- as.factor(loans$EmploymentStatus)
loans$CreditScoreRange <- as.factor(loans$CreditScoreRange)
loans$Occupation <- as.factor(loans$Occupation)
loans$Term <- as.factor(loans$Term)
loans$CreditScoreRange <- relevel(loans$CreditScoreRange, ref = "Fair")
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
#loans <- loans  %>% mutate_if(is.character, as.factor)

# convert days to months

#loans <- loans %>% mutate(time = round(time/30.417, digit=0))



loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
loans <- loans %>% dplyr::select(-2,-Occupation)

loans_cc <- loans  %>% na.omit()


###### Gamma Modeling ########

loans_cc <- loans_cc |>
  mutate(event_time = Surv(time, status))

split   <- initial_split(loans_cc, prop = 0.7, strata = status)
loans_tr  <- training(split)
loans_val <- testing(split)

g_spec <- 
  survival_reg(dist = "gamma") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
g_spec


gamma_fit <- g_spec |> fit(event_time ~  BorrowerRate + CreditScoreRange +  BankcardUtilization + DebtToIncomeRatio + OpenCreditLines + 
					CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months, 
					data = loans_tr)

gamma_fit1 <- g_spec |> fit(Surv(time,status) ~  BorrowerRate + CreditScoreRange +  BankcardUtilization + DebtToIncomeRatio + OpenCreditLines + 
					CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months, 
					data = loans_cc)


gamma_fit
tidy(gamma_fit)
cat("Results from training set \n")
glance(gamma_fit)
cat("Results from CC set \n")
glance(gamma_fit1)

time_points <- sort(unique(loans_val$time))
gamma_pred <- augment(gamma_fit, loans_val, eval_time = time_points)
brier_scores <-
  gamma_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_scores$.estimate)


IBS_g <- gamma_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_g

roc_scores <-
  gamma_pred |> 
  roc_auc_survival(truth = event_time, .pred)



gamma_cindex <- gamma_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)
gamma_cindex

### Cross-validation gamma
loans_folds <- vfold_cv(loans_cc, v = 5, strata = status)

loans_folds$splits[[1]] |> analysis() |> dim()

cat("Cross validation \n ")
g_cross_val <- g_spec |> fit_resamples(event_time ~ BorrowerRate + CreditScoreRange +  Term + BankcardUtilization + DebtToIncomeRatio + OpenCreditLines + 
					CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months,
					method = "Nelder-Mead",
					resamples = loans_folds, 
					eval_time = sort(unique(loans_cc$time)), 
					metrics = metric_set(concordance_survival,brier_survival,brier_survival_integrated))

g_metric = collect_metrics(g_cross_val)

tail(g_metric)

cat("The IBS is = ")
collect_metrics(g_cross_val) |> 
  filter(.metric == "brier_survival_integrated")
cat("The c-index is = ")
collect_metrics(g_cross_val) |> 
  filter(.metric == "concordance_survival")



rm(list=ls())
library(MASS)
library(caret)
library(reticulate)
library(data.table)
library(survival)
library(flexsurv)
library(Hmisc)
library(BART)
library(rsample)
library(SurvMetrics)
library(prodlim)
library(pec)
library(rms)
library(tidymodels)
library(censored)
options(expressions=10000)

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
loans$Occupation <- as.factor(loans$Occupation)
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
#loans <- loans  %>% mutate_if(is.character, as.factor)
loans$CreditScoreRange <- relevel(loans$CreditScoreRange, ref = "Fair")
loans$ProsperScore <- ifelse(loans$ProsperScore == 11,10,loans$ProsperScore)
loans$ProsperScore <- as.factor(loans$ProsperScore)
loans$Term <- as.factor(loans$Term)

# convert days to months

#loans <- loans %>% mutate(time = round(time/30.417, digit=0))
loans <- loans %>% dplyr::select(-2,-Occupation)


loans_cc <- loans  %>% na.omit()



###### Weibull Modeling ########

loans_cc <- loans_cc |>
  mutate(event_time = Surv(time, status))


split   <- initial_split(loans_cc, prop = 0.7, strata = status)
loans_tr  <- training(split)
loans_val <- testing(split)

wei_spec <- 
  survival_reg(dist = "weibull") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
wei_spec


wei_fit <- wei_spec |> fit(event_time ~  OpenCreditLines + InquiriesLast6Months + BankcardUtilization + DebtToIncomeRatio + BorrowerRate  +
                                         CreditScoreRange + Term, 
					data = loans_tr)

wei_fit1 <- wei_spec |> fit(event_time ~  OpenCreditLines + InquiriesLast6Months + BankcardUtilization + DebtToIncomeRatio + BorrowerRate  +
                                         CreditScoreRange + Term, 
					data = loans_cc)


wei_fit
tidy(wei_fit)
cat("Results from training set \n")
tidy(wei_fit)
glance(wei_fit)
cat("Results from CC set \n")
tidy(wei_fit1)
glance(wei_fit1)


time_points <- sort(unique(loans_val$time))
wei_pred <- parsnip::augment(wei_fit, loans_val, eval_time = time_points)

brier_scores <-
  wei_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_scores$.estimate)


IBS_wei <- wei_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_wei

roc_scores <-
  wei_pred |> 
  roc_auc_survival(truth = event_time, .pred)



wei_cindex <- wei_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)
wei_cindex

### Cross-validation lognormal
loans_folds <- vfold_cv(loans_cc, v = 5, strata = status)

loans_folds$splits[[1]] |> analysis() |> dim()

cat("Cross validation \n ")
wei_cross_val <- wei_spec |> fit_resamples(event_time ~ OpenCreditLines + InquiriesLast6Months + BankcardUtilization + DebtToIncomeRatio + BorrowerRate  +
                                         	CreditScoreRange + Term,
						resamples = loans_folds, 
						eval_time = sort(unique(loans_cc$time)), 
						metrics = metric_set(concordance_survival, brier_survival, brier_survival_integrated))

wei_metric = collect_metrics(wei_cross_val)

tail(wei_metric)

cat("The IBS is  \n")
collect_metrics(wei_cross_val) |> 
  filter(.metric == "brier_survival_integrated")
cat("The c-index is  \n")
collect_metrics(wei_cross_val) |> 
  filter(.metric == "concordance_survival")


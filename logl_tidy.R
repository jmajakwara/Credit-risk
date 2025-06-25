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
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
#loans <- loans  %>% mutate_if(is.character, as.factor)
#loans$CreditScoreRange <- relevel(loans$CreditScoreRange, ref = "Fair")
loans$ProsperScore <- ifelse(loans$ProsperScore == 11,10,loans$ProsperScore)
loans$ProsperScore <- as.factor(loans$ProsperScore)
loans$Term <- as.factor(loans$Term)

# convert days to months

#loans <- loans %>% mutate(time = round(time/30.417, digit=0))


loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
loans <- loans %>% dplyr::select(-2,-Occupation)

loans_cc <- loans  %>% na.omit()



###### Rxponential Modeling ########



cat('The Logligistic Model using selected variables output \n')

loans_cc <- loans_cc |>
  mutate(event_time = Surv(time, status))


split   <- initial_split(loans_cc, prop = 0.7, strata = status)
loans_tr  <- training(split)
loans_val <- testing(split)

logl_spec <- 
  survival_reg(dist = "llogis") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
logl_spec


logl_fit <- logl_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + Term +
                                        BankcardUtilization + DebtToIncomeRatio + CurrentlyInGroup + CreditScoreRange, 
					data = loans_tr)

logl_fit1 <- logl_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + Term +
                                        BankcardUtilization + DebtToIncomeRatio + CurrentlyInGroup + CreditScoreRange, 
					data = loans_cc)


logl_fit
tidy(logl_fit)
cat("Results from training set \n")
glance(logl_fit)
cat("Results from CC set \n")
glance(logl_fit1)


time_points <- sort(unique(loans_val$time))
logl_pred <- augment(logl_fit, loans_val, eval_time = time_points)
brier_scores <-
  logl_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_scores$.estimate)


IBS_logl <- logl_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_logl

roc_scores <-
  logl_pred |> 
  roc_auc_survival(truth = event_time, .pred)



logl_cindex <- logl_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)
logl_cindex

### Cross-validation lognormal
loans_folds <- vfold_cv(loans_cc, v = 5, strata = status)


cat("Cross validation \n ")
logl_cross_val <- logl_spec |> fit_resamples(event_time ~ OpenCreditLines + InquiriesLast6Months + Term +
                                        		BankcardUtilization + DebtToIncomeRatio + CurrentlyInGroup + CreditScoreRange,
							# method = "Nelder-Mead", 
							resamples = loans_folds, 
							eval_time = sort(unique(loans_cc$time)), 
							metrics = metric_set(concordance_survival,brier_survival,brier_survival_integrated))

logl_metric = collect_metrics(logl_cross_val)

tail(logl_metric)

cat("The IBS is = ")
collect_metrics(logl_cross_val) |> 
  filter(.metric == "brier_survival_integrated")
cat("The c-index is = ")
collect_metrics(logl_cross_val) |> 
  filter(.metric == "concordance_survival")



rm(list=ls())
library(MASS)
library(caret)
library(ggplot2)
library(reticulate)
library(tidyverse)
library(data.table)
library(survival)
library(Hmisc)
library(BART)
library(flexsurv)
library(survminer)
library(rsample)
library(DynNom)
library(SurvMetrics)
library(prodlim)
library(eha)
library(pec)
library(rms)
library(censored)
library(tidymodels)
#library(nanair)


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
#loans <- loans  %>% mutate_if(is.character, as.factor)
loans$Term <- as.factor(loans$Term)
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
loans$IsBorrowerHomeowner <- as.factor(loans$IsBorrowerHomeowner)
loans$ProsperScore <- ifelse(loans$ProsperScore == 11,10,loans$ProsperScore)
loans$ProsperScore <- as.factor(loans$ProsperScore)

# convert days to months

#loans <- loans %>% mutate(time = round(time/30.417, digit=0))

term_adj_fctn <- function(df, t) {
  term  <-  df$Term == t
  df <- df[term, ]
  # plot frequncy graph
  #barplot(table(df$month) / sum(table(df$month)))
  
  maxtime <- t
  max_term  <-  df$month <= maxtime
  df <- df[max_term, ]
}


loans_12m <- term_adj_fctn(loans, 12)
loans_36m <- term_adj_fctn(loans, 36)
loans_60m <- term_adj_fctn(loans, 60)

# combine adjusted datasets
#loans <- rbind(loans_12m, loans_36m, loans_60m)



loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
loans <- loans %>% dplyr::select(-2,-Occupation)


####### Little's MCAR Test ###################
cat("Little's MCAR test results")
#mcar_test(loans)


loans_cc <- loans  %>% na.omit()



##### Gamma Modeling ###########
loans_cc <- loans_cc |>
  mutate(event_time = Surv(time, status))


split   <- initial_split(loans_cc, prop = 0.7, strata = status)
loans_tr  <- training(split)
loans_val <- testing(split)

### Survival plot
loans_cc <- loans_cc |> mutate(InquiriesLast6Months_bin = ifelse(InquiriesLast6Months == 0, 0,
                                              ifelse(InquiriesLast6Months == 1 | InquiriesLast6Months == 2,1,2)))

fit.surv <- survfit(Surv(time, status) ~ CreditScoreRange, data = loans_cc)

pdf(file="Survival plot_cc.pdf")
ggsurvplot(fit.surv, conf.int = FALSE, 
           xlab = "Time (in days)",
           ylab = "Survival probability",
           linetype =  "strata",
           ggtheme =  theme_minimal(),
           legend = "right",
	   cex.lab = 1.2,
   	   cex.axis = 1.2,
           legend.title = "Inquiries last 6 months")
dev.off()


gg_spec <- 
  survival_reg(dist = "gengamma") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
gg_spec

ggamma_fit <- gg_spec |> fit(event_time ~ DebtToIncomeRatio + BankcardUtilization + BorrowerRate + OpenCreditLines + InquiriesLast6Months +
				CreditScoreRange + IsBorrowerHomeowner + Term,
				inits = c(8,1.2,-1.1,-0.4,0.3,-5,0.02,-0.04,-0.07,-0.03,-0.1,-0.06,0.4,0.5),
				fixedpars = c(3),
				data = loans_tr)

ggamma_fit1 <- gg_spec |> fit(event_time ~ DebtToIncomeRatio + BankcardUtilization + BorrowerRate + OpenCreditLines + InquiriesLast6Months +
				CreditScoreRange + IsBorrowerHomeowner + Term,
				inits = c(8,1.2,-1.1,-0.4,0.3,-5,0.02,-0.04,-0.07,-0.03,-0.1,-0.06,0.4,0.5),
				fixedpars = c(3),
				data = loans_cc)
 

ggamma_fit
cat("Results from training set \n")
tidy(ggamma_fit)
glance(ggamma_fit)
cat("Results from CC set \n")
tidy(ggamma_fit1)
glance(ggamma_fit1)


time_points <- sort(unique(loans_val$time))

#ggam_surv <- censored::predict(ggamma_fit, new_data = loans_val, type = "survival", eval_time = time_points)
#ggam_surv

ggamma_pred <- parsnip::augment(ggamma_fit, loans_val, eval_time = time_points)

brier_Sc_gg <-
  ggamma_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_Sc_gg$.estimate)

IBS_gg <- ggamma_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_gg

roc_scores_gg <-
  ggamma_pred |> 
  roc_auc_survival(truth = event_time, .pred)


Con_gg <- ggamma_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)

Con_gg


###### Cross validation ######


loans_folds <- vfold_cv(loans_cc, v = 5, strata = status)

loans_folds$splits[[1]] |> analysis() |> dim()

cat("Cross-validation metrics \n")
gg_cross_val <- gg_spec |> fit_resamples(event_time ~ DebtToIncomeRatio + OpenCreditLines + BankcardUtilization + BorrowerRate + InquiriesLast6Months +
					CreditScoreRange + IsBorrowerHomeowner + Term,
					inits = c(8,1.2,-1.1,-0.4,0.3,-5,0.02,-0.04,-0.07,-0.03,-0.1,-0.06,0.4,0.5),
					fixedpars = c(3),
					resamples = loans_folds, 
					eval_time = sort(unique(loans_cc$time)),
					metrics = metric_set(concordance_survival,brier_survival,brier_survival_integrated))

gg_metric = collect_metrics(gg_cross_val)

tail(gg_metric)

cat("The IBS is = ")
collect_metrics(gg_cross_val) |> filter(.metric == "brier_survival_integrated")

cat("The c-index is = ")
collect_metrics(gg_cross_val) |> filter(.metric == "concordance_survival")



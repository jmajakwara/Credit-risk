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
#library(SurvMetrics)
library(prodlim)
library(eha)
library(pec)
#library(rms)
library(ggplot2)


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
loans$IsBorrowerHomeowner <- as.factor(loans$IsBorrowerHomeowner)
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
loans$Occupation <- as.factor(loans$Occupation)
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

time_points <- sort(unique(loans_val$time))
time_points <- time_points[1:(length(time_points) - 2)]

cox_pred <- augment(cox_fit, loans_val, eval_time = time_points)

roc_cox <-
  cox_pred |> 
  roc_auc_survival(truth = event_time, .pred)

exp_spec <- 
  survival_reg(dist = "exp") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
exp_spec


exp_fit <- exp_spec |> fit(event_time ~ BorrowerRate + OpenCreditLines +  InquiriesLast6Months + DebtToIncomeRatio + BankcardUtilization + CreditScoreRange,
						data = loans_tr)

exp_pred <- augment(exp_fit, loans_val, eval_time = time_points)

roc_exp <-
  exp_pred |> 
  roc_auc_survival(truth = event_time, .pred)


g_spec <- 
  survival_reg(dist = "gamma") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
g_spec


gamma_fit <- g_spec |> fit(event_time ~  BorrowerRate + CreditScoreRange +  BankcardUtilization + DebtToIncomeRatio + OpenCreditLines + 
					CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months, 
					data = loans_tr)

gamma_pred <- augment(gamma_fit, loans_val, eval_time = time_points)

roc_gam <-
  gamma_pred |> 
  roc_auc_survival(truth = event_time, .pred)

gg_spec <- 
  survival_reg(dist = "gengamma.orig") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
gg_spec
ggamma_fit <- gg_spec |> 
  			fit(event_time ~ BorrowerRate +  OpenCreditLines + InquiriesLast6Months +  
                                        BankcardUtilization + DebtToIncomeRatio + IsBorrowerHomeowner + CreditScoreRange + Term,
			data = loans_tr)

ggamma_pred <- augment(ggamma_fit, loans_val, eval_time = time_points)

roc_ggam <-
  ggamma_pred |> 
  roc_auc_survival(truth = event_time, .pred)


log_spec <- 
  survival_reg(dist = "lnorm") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
log_spec


log_fit <- log_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + BorrowerRate +
                                          	BankcardUtilization + DebtToIncomeRatio +   CurrentlyInGroup + CreditScoreRange + Term, 
						data = loans_tr)

log_pred <- augment(log_fit, loans_val, eval_time = time_points)

roc_log <-
  log_pred |> 
  roc_auc_survival(truth = event_time, .pred)


logl_spec <- 
  survival_reg(dist = "llogis") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
logl_spec


logl_fit <- logl_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + Term +
                                        BankcardUtilization + DebtToIncomeRatio + CurrentlyInGroup + CreditScoreRange, 
					data = loans_tr)

logl_pred <- augment(logl_fit, loans_val, eval_time = time_points)

roc_logl <-
  logl_pred |> 
  roc_auc_survival(truth = event_time, .pred)


wei_spec <- 
  survival_reg(dist = "weibull") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
wei_spec


wei_fit <- wei_spec %>% fit(event_time ~ OpenCreditLines + InquiriesLast6Months + BankcardUtilization + DebtToIncomeRatio + BorrowerRate  +
                                         CreditScoreRange + Term, 
					data = loans_tr)

wei_pred <- augment(wei_fit, loans_val, eval_time = time_points)

roc_wei <-
  wei_pred |> 
  roc_auc_survival(truth = event_time, .pred)


Model = c("Cox", "Exponential", "Gamma", "Generalized_Gamma", "Lognormal", "Loglogistic", "Weibull")

# Create a data frame to store Brier scores and time points
roc_data <- data.frame(
  Time = time_points,
  Cox = roc_cox$.estimate,
  Exponential = roc_exp$.estimate,
  Gamma = roc_gam$.estimate,
  Generalized_Gamma = roc_ggam$.estimate,
  Lognormal = roc_log$.estimate,
  Loglogistic = roc_logl$.estimate,
  Weibull = roc_wei$.estimate
)

# Melt the data frame to long format
roc_data_long <- pivot_longer(roc_data, cols = -Time, names_to = "Model", values_to = "ROC_AUC")

# Create the plot using ggplot2
pdf(file="ROC AUC for models.pdf")
ggplot(roc_data_long, aes(x = Time, y = ROC_AUC, color = Model)) +
  geom_line() +
 # geom_hline(yintercept = 1 / 2, col = "red", lty = 2) +
  labs(x = "Time (in days)",
       y = "ROC AUC") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),  # Increase the font size for axis text
        axis.title = element_text(size = 14)) # Adjust axis title size if necessary

dev.off()



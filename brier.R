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

cox_fit <- cox_spec |> fit(Surv(time, status) ~ BorrowerRate +  OpenCreditLines + InquiriesLast6Months + AmountDelinquent +  BankcardUtilization + DebtToIncomeRatio + 
                                        IsBorrowerHomeowner + CreditScoreRange + Term,
					data = loans_tr)



time_points <- sort(unique(loans_val$time))
time_points <- time_points[1:(length(time_points) - 2)]

cox_pred <- augment(cox_fit, loans_val, eval_time = time_points)

brier_cox <-
  cox_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_cox$.estimate)

exp_spec <- 
  survival_reg(dist = "exp") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
exp_spec


exp_fit <- exp_spec |> fit(event_time ~ BorrowerRate + OpenCreditLines +  InquiriesLast6Months + DebtToIncomeRatio +  BankcardUtilization + CreditScoreRange,
						data = loans_tr)

exp_pred <- augment(exp_fit, loans_val, eval_time = time_points)
brier_exp <-
  exp_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_exp$.estimate)

g_spec <- 
  survival_reg(dist = "gamma") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
g_spec


gamma_fit <- g_spec |> fit(event_time ~  BorrowerRate + CreditScoreRange +  BankcardUtilization + DebtToIncomeRatio + OpenCreditLines + 
					CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months, 
					data = loans_tr)

gamma_pred <- augment(gamma_fit, loans_val, eval_time = time_points)
brier_gam <-
  gamma_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_gam$.estimate)

gg_spec <- 
  survival_reg(dist = "gengamma.orig") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
gg_spec
ggamma_fit <- gg_spec |> 
  			fit(event_time ~ BankcardUtilization + DebtToIncomeRatio + OpenCreditLines + IsBorrowerHomeowner + 
			CreditScoreRange +   InquiriesLast6Months  + BorrowerRate + Term,
			data = loans_tr)


ggamma_pred <- augment(ggamma_fit, loans_val, eval_time = time_points)

brier_ggam <-
  ggamma_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_ggam$.estimate)

log_spec <- 
  survival_reg(dist = "lnorm") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
log_spec


log_fit <- log_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + BorrowerRate +
                                          	BankcardUtilization + DebtToIncomeRatio +   CurrentlyInGroup + CreditScoreRange + Term, 
						data = loans_tr)


log_pred <- augment(log_fit, loans_val, eval_time = time_points)
brier_log <-
  log_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_log$.estimate)


logl_spec <- 
  survival_reg(dist = "llogis") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
logl_spec


logl_fit <- logl_spec |> fit(event_time ~ OpenCreditLines + InquiriesLast6Months + Term +
                                        BankcardUtilization + DebtToIncomeRatio + CurrentlyInGroup + CreditScoreRange, 
					data = loans_tr)

logl_pred <- augment(logl_fit, loans_val, eval_time = time_points)
brier_logl <-
  logl_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_logl$.estimate)

wei_spec <- 
  survival_reg(dist = "weibull") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
wei_spec


wei_fit <- wei_spec %>% fit(event_time ~ OpenCreditLines + InquiriesLast6Months + BankcardUtilization + DebtToIncomeRatio + BorrowerRate  +
                                         CreditScoreRange + Term,,
				#	method = "Nelder-Mead",
					data = loans_tr)

wei_pred <- augment(wei_fit, loans_val, eval_time = time_points)

brier_wei <-
  wei_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_wei$.estimate)


Model = c("Cox", "Exponential", "Gamma", "Generalized_gamma", "Lognormal", "Loglogistic", "Weibull")

# Create a data frame to store Brier scores and time points
brier_data <- data.frame(
  Time = time_points,
  Cox = brier_cox$.estimate,
  Exponential = brier_exp$.estimate,
  Gamma = brier_gam$.estimate,
  Generalized_Gamma = brier_ggam$.estimate,
  Lognormal = brier_log$.estimate,
  Loglogistic = brier_logl$.estimate,
  Weibull = brier_wei$.estimate
)

# Pivot_longer the data frame to long format
brier_data_long <- pivot_longer(brier_data, cols = -Time, names_to = "Model", values_to = "Brier_Score")

# Create the plot using ggplot2
pdf(file="Brier score for distributions.pdf")
ggplot(brier_data_long, aes(x = Time, y = Brier_Score, color = Model)) +
  geom_line() +
  geom_hline(yintercept = 1 / 4, col = "red", lty = 2) +
  labs(x = "Time (in days)",
       y = "Brier score") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),  # Increase the font size for axis text
        axis.title = element_text(size = 14)) # Adjust axis title size if necessary
dev.off()


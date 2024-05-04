###########################################################################################
#                                Data Cleaning                                            #
###########################################################################################
choose.files()
filename = "loans_final_jj.csv"
loans <- read.csv(filename,header=TRUE,sep=",",na.strings = "NA")
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
# loans <- loans |> mutate(month = round(time/30.4375, digit=0))

# term_adj_fctn <- function(df, t) {
#  term  <-  df$Term == t
#  df <- df[term, ]
#  # plot frequncy graph
#  #barplot(table(df$month) / sum(table(df$month)))
#  maxtime <- t
#  max_term  <-  df$month <= maxtime
#  df <- df[max_term, ]}


#loans_12m <- term_adj_fctn(loans, 12)
#loans_36m <- term_adj_fctn(loans, 36)
#loans_60m <- term_adj_fctn(loans, 60)


# combine adjusted datasets
# loans <- rbind(loans_12m, loans_36m, loans_60m)
#loans$time <- round(loans$time/30.417,0)

# Removing cases whose debt to income ratio is more than 100
loans <- loans |> filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
str(loans)

loans <- loans |> dplyr::select(-ListingKey)#,-Occupation)

# loans <- loans |> mutate_if(is.character, as.factor)
# loans |>  summarize_all(funs(sum(is.na(.))))/dim(loans)[1]*100

loans_cc <- loans |> na.omit()
str(loans_cc)
missing_cases=(1-(dim(loans_cc)[1]/dim(loans)[1]))
missing_cases
# Split the data into a test and training set
# following https://www.tidymodels.org/start/recipes/#recipe
set.seed(1974)
data_split <- initial_split(loans_cc, prop = 0.70, strata = status)     
#Create data frames for the two sets:
loans_train <- training(data_split)
loans_test  <- testing(data_split)

loans_train_all <- loans[!(loans$LoanKey %in% c(loans_test$LoanKey)),]

#loans_train_all <- loans_train_all |> select(-1)

loans_train <- loans_train |> dplyr::select(time : MonthlyLoanPayment)
loans_test <- loans_test |> dplyr::select(time : MonthlyLoanPayment)

## Partioning of data with missingness
loans_missing <- loans[!(loans$LoanKey %in% c(loans_cc$LoanKey)),]


var_sel <- c("time","status","BorrowerRate","EmploymentStatusDuration","ProsperScore","OpenCreditLines",
             "TotalCreditLinespast7years","InquiriesLast6Months","Term","AmountDelinquent","TradesOpenedLast6Months","DelinquenciesLast7Years", "StatedMonthlyIncome", "RevolvingCreditBalance","PublicRecordsLast10Years", "BankcardUtilization","CurrentlyInGroup","OpenRevolvingAccounts","DebtToIncomeRatio","LoanOriginalAmount","TradesNeverDelinquent","PercentFunded", "EmploymentStatus", "LoanCurrentDaysDelinquent","IsBorrowerHomeowner","CreditScoreRange","CurrentDelinquencies")

loans_train <- loans_train |> dplyr::select(all_of(var_sel))




summary(survfit(Surv(time, status) ~ 1, data = loans_cc), times = c(1,365,median(loans_cc$time),3*365,max(loans_cc$time)))



###############################################################################################################
#                                       Fitting Models using CC                                               #
###############################################################################################################

loans_train$Term <- as.factor(loans_train$Term)
loans_test$Term <- as.factor(loans_test$Term)

## cox PH model

cox_final <- coxph(Surv(time, status) ~ BorrowerRate +  OpenCreditLines + InquiriesLast6Months + OpenRevolvingAccounts + 
                     AmountDelinquent + DelinquenciesLast7Years + BankcardUtilization + StatedMonthlyIncome +
                     DebtToIncomeRatio + LoanOriginalAmount + LoanCurrentDaysDelinquent + 
                     IsBorrowerHomeowner + CreditScoreRange + Term,x=T,y=T, data = loans_train)
cph_fit <- cph(Surv(time, status) ~ BorrowerRate +  OpenCreditLines + InquiriesLast6Months + OpenRevolvingAccounts + 
                     AmountDelinquent + DelinquenciesLast7Years + BankcardUtilization + StatedMonthlyIncome +
                     DebtToIncomeRatio + LoanOriginalAmount + LoanCurrentDaysDelinquent + 
                     IsBorrowerHomeowner + CreditScoreRange + Term,x=T,y=T, data = loans_train,surv=TRUE)

y <- summary(cox_final)
#y$concordance
D=glance(cox_final)
Surv_obj=with(loans_test,Surv(time,status))
# Calculate predicted hazard ratios
hr <- predict(cox_final,newdata=loans_test[,-c(1:2)], type = "risk")

# Calculate concordance index and Brier score
ppc <- predict(cph_fit,newdata = loans_test[,-c(1:2)])
pred_prob_cox <- predict(cox_final,newdata = loans_test,type="risk")
pred_surv_cox <- predict(cox_final,newdata = loans_test,type="survival")
#rcorrcens(Surv_obj~pred_prob_cox,outx=TRUE)
#rcorrcens(Surv_obj~ppc,outx=TRUE)
D_cox=rcorr.cens(pred_prob_cox,Surv_obj)
B1=concordance(Surv(time,status)~pred_prob_cox,data=loans_test)
cox_brier=Brier(Surv(loans_test$time,loans_test$status),pred_surv_cox,max(loans_test$time))

## Exponential model
exp_final <- survreg(Surv(time, status) ~ BorrowerRate + OpenCreditLines + OpenRevolvingAccounts + InquiriesLast6Months + 
                       AmountDelinquent + DelinquenciesLast7Years + BankcardUtilization + StatedMonthlyIncome +
                       DebtToIncomeRatio + LoanCurrentDaysDelinquent + IsBorrowerHomeowner + Term,
                       data = loans_train, dist = "exponential")
#summary(exp_final)
pred_prob_exp <- predict(exp_final,newdata = loans_test[,-c(1:2)],type="response")
D_exp=rcorr.cens(pred_prob_exp,Surv(loans_test$time,loans_test$status))
D1=glance(exp_final)
B2=concordance(Surv(time,status)~pred_prob_exp,data=loans_test)
exp_brier=Brier(exp_final,loans_test,max(loans_test$time))



wei_final <-survreg(Surv(time, status) ~ OpenCreditLines + InquiriesLast6Months + AmountDelinquent + StatedMonthlyIncome +
                      PublicRecordsLast10Years + BankcardUtilization + DebtToIncomeRatio + LoanCurrentDaysDelinquent + 
                      CurrentlyInGroup + CreditScoreRange + Term, data = loans_train, dist = "weibull")
#summary(wei_final)
pred_prob_wei <- predict(wei_final,newdata = loans_test[,-c(1:2)],type="response")
D_wei=rcorr.cens(pred_prob_wei,Surv(loans_test$time,loans_test$status))
D2=glance(wei_final)
B3=concordance(Surv(time,status)~pred_prob_wei,data=loans_test)
wei_brier=Brier(wei_final,loans_test,max(loans_test$time))


## Lognormal model
log_final <- survreg(Surv(time, status) ~  BorrowerRate + OpenCreditLines + InquiriesLast6Months + BankcardUtilization + 
                       DebtToIncomeRatio + LoanCurrentDaysDelinquent +  StatedMonthlyIncome + 
                       CurrentlyInGroup + CreditScoreRange + Term, data = loans_train, dist = "lognormal")

#summary(log_final)
pred_prob_log <- predict(log_final,newdata = loans_test[,-c(1:2)],type="response")
D_log=rcorr.cens(pred_prob_log,Surv(loans_test$time,loans_test$status))
D3=glance(log_final)
B4=concordance(Surv(time,status)~pred_prob_log,data=loans_test)
log_brier=Brier(log_final,loans_test,max(loans_test$time))


## Gamma model

gamma_final <- flexsurv::flexsurvreg(Surv(time, status) ~ BorrowerRate + CreditScoreRange +  Term + BankcardUtilization + LoanCurrentDaysDelinquent + DebtToIncomeRatio + OpenCreditLines + CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months,
                         data = loans_train, dist = "gamma")#,rate=0.3,hess.control=list(tol.solve=.Machine$double.eps,tol.evalues=1e-10))

#tidy(gamma_model)
D4=glance(gamma_final)

################################################################
########## Using tidymodels package ############################
################################################################
loans_cc_dat <- loans_cc |>
  mutate(event_time = Surv(time, status))
str(loans_cc_dat)

loans_cc_dat <- loans_cc_dat |>
  dplyr::select(time,status,event_time,  BorrowerRate, CreditScoreRange,  Term, BankcardUtilization, LoanCurrentDaysDelinquent, DebtToIncomeRatio, OpenCreditLines, CurrentlyInGroup, TradesNeverDelinquent, InquiriesLast6Months)

split   <- initial_split(loans_cc_dat, prop = 0.8, strata = status)
loans_tr  <- training(split)
loans_val <- testing(split)

g_spec <- 
  survival_reg(dist = "gamma") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
g_spec


gamma_fit <- g_spec |> fit(event_time ~  BorrowerRate + CreditScoreRange +  Term + BankcardUtilization + LoanCurrentDaysDelinquent + DebtToIncomeRatio + OpenCreditLines + CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months, data = loans_tr)

gamma_fit
tidy(gamma_fit)
glance(gamma_fit)

time_points <- sort(unique(loans_tr$time))
gamma_pred <- augment(gamma_fit, loans_val, eval_time = time_points)
brier_scores <-
  gamma_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_scores$.estimate)

brier_scores %>% 
  ggplot(aes(.eval_time, .estimate)) + 
  geom_hline(yintercept = 1 / 4, col = "red", lty = 1) +
  geom_line() +
  geom_point() + 
  labs(x = "time", y = "Brier score")

IBS_g <- gamma_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_g

roc_scores <-
  gamma_pred |> 
  roc_auc_survival(truth = event_time, .pred)
roc_scores |> 
  ggplot(aes(.eval_time, .estimate)) + 
  geom_hline(yintercept = 1 / 2, col = "red", lty = 1) +
  geom_line() +
  geom_point() + 
  labs(x = "time", y = "ROC AUC")



gamma_cindex <- gamma_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)
gamma_cindex

### Cross-validation gamma
loans_folds <- vfold_cv(loans_tr, v = 3, strata = status)

loans_folds$splits[[1]] |> analysis() |> dim()

g_cross_val <- g_spec |> fit_resamples(event_time ~ BorrowerRate + CreditScoreRange +  Term + BankcardUtilization + LoanCurrentDaysDelinquent + DebtToIncomeRatio + OpenCreditLines + CurrentlyInGroup + TradesNeverDelinquent + InquiriesLast6Months, resamples = loans_folds, eval_time = time_points, metrics = metric_set(concordance_survival,brier_survival,brier_survival_integrated))

g_metric = collect_metrics(gg_cross_val)
g_metric



pred_prob_gam=augment(gamma_final,data=loans_train,newdata=loans_test[,-c(1:2)],type.predict = "response")
B5=concordance(Surv(time,status)~pred_prob_gam$.pred_time,data=loans_test)
b <-  concordance(gamma_pred$event_time~gamma_pred$.pred_time, data=loans_val)
b
D_log=rcorr.cens(pred_prob_gam$.pred_time,Surv(loans_test$time,loans_test$status))

sim_times <- matrix(rgamma(nrow(loans_test)*1000, shape = tidy(gamma_final)$estimate["shape"], scale = 1/tidy(gamma_final)$estimate["rate"]), nrow = nrow(loans_test))

probs1 <- rowMeans(sim_times > loans_test$time)
probs <- summary(gamma_final,newdata = loans_test,type = "survival",ci=FALSE,B=0,tidy=TRUE,t=median(loans_test$time))
Surv_obj <- with(loans_test, Surv(time, status))
brier=Brier(Surv_obj,probs$est, median(loans_test$time))

gamma <- c(D4$logLik,D4$df, D4$AIC, D4$BIC, B5$concordance ,sqrt(B5$var),brier)

## Loglogistic model
ll_final <- survreg(Surv(time, status) ~ OpenCreditLines + InquiriesLast6Months + AmountDelinquent + StatedMonthlyIncome +
                      BankcardUtilization + DebtToIncomeRatio + LoanCurrentDaysDelinquent + 
                      CurrentlyInGroup + CreditScoreRange + Term, data = loans_train, dist = "loglogistic")#OpenRevolvingAccounts  +
#summary(ll_final)
pred_prob_ll <- predict(ll_final,newdata = loans_test[,-c(1:2)],type="response")
D_ll=rcorr.cens(pred_prob_ll,Surv(loans_test$time,loans_test$status))
D5=glance(ll_final)
B6=concordance(Surv(time,status)~pred_prob_ll,data=loans_test)
ll_brier=Brier(ll_final,loans_test,max(loans_test$time))
#pre_sb<-predictSurvProb2survreg(ll_final,loans_test,max(loans_test))
#rcorr.cens(pre_sb,Surv(loans_test$time,loans_test$status))
#concordance(Surv(time,status)~pre_sb,data=loans_test)

####### Generalised gamma ########################
gg_spec <- 
  survival_reg(dist = "gengamma") |>
  set_engine("flexsurv") |>
  set_mode("censored regression") 
gg_spec
ggamma_model2 <- gg_spec |> 
  fit(event_time ~ BankcardUtilization + LoanCurrentDaysDelinquent + DebtToIncomeRatio + OpenCreditLines + CurrentlyInGroup + CreditScoreRange +   InquiriesLast6Months + TradesNeverDelinquent + BorrowerRate, data = loans_tr, method="Nelder-Mead")#    + Term 

ggamma_model2
tidy(ggamma_model2)
glance(ggamma_model2)

time_points <- sort(unique(loans_tr$time))
ggamma_pred <- augment(ggamma_model2, loans_val, eval_time = time_points)

brier_Sc_gg <-
  ggamma_pred |> 
  brier_survival(truth = event_time, .pred)

quantile(brier_Sc_gg$.estimate)

brier_Sc_gg %>% 
  ggplot(aes(.eval_time, .estimate)) + 
  geom_hline(yintercept = 1 / 4, col = "red", lty = 1) +
  geom_line() +
  geom_point() + 
  labs(x = "time", y = "Brier score")

IBS_gg <- ggamma_pred |> brier_survival_integrated(truth = event_time, .pred)

IBS_gg

roc_scores_gg <-
  ggamma_pred |> 
  roc_auc_survival(truth = event_time, .pred)

roc_scores_gg |> 
  ggplot(aes(.eval_time, .estimate)) + 
  geom_hline(yintercept = 1 / 2, col = "red", lty = 1) +
  geom_line() +
  geom_point() + 
  labs(x = "time", y = "ROC AUC")


Con_gg <- ggamma_pred |>  concordance_survival(truth = event_time, estimate = .pred_time)

Con_gg

Cc <-  concordance(object=ggamma_pred$event_time ~ ggamma_pred$.pred_time, data=loans_val)

Cc


###### Cross validation ######

# To put 60% into training, 20% in validation, and 20% in testing:
ames_val_split <- initial_validation_split(ames, prop = c(0.6, 0.2))
ames_val_split
ames_train <- training(ames_val_split)
ames_test <- testing(ames_val_split)
ames_val <- validation(ames_val_split)

str(loans_tr)
loans_folds <- vfold_cv(loans_tr, v = 3, strata = status)

loans_folds$splits[[1]] |> analysis() |> dim()

gg_cross_val <- gg_spec |> fit_resamples(event_time ~ BankcardUtilization + LoanCurrentDaysDelinquent + DebtToIncomeRatio + OpenCreditLines + CurrentlyInGroup + CreditScoreRange +   InquiriesLast6Months + TradesNeverDelinquent + BorrowerRate, resamples = loans_folds, eval_time = time_points, metrics = metric_set(concordance_survival,brier_survival,brier_survival_integrated))

gg_metric = collect_metrics(gg_cross_val)
gg_metric

rlang::last_trace(drop = FALSE)
?yardstick::metric_set
?fit_resamples






## Calculation of BS and IBS

# Perform this on train dataset  ---------------
log_fit <- psm(Surv(time, status) ~  BorrowerRate + OpenCreditLines + InquiriesLast6Months + BankcardUtilization + DebtToIncomeRatio +  LoanCurrentDaysDelinquent +  StatedMonthlyIncome + CurrentlyInGroup + CreditScoreRange + Term, data = loans_train, dist = "lognormal")
Wei_fit <- psm(Surv(time, status) ~ OpenCreditLines + InquiriesLast6Months + AmountDelinquent + StatedMonthlyIncome +
                 PublicRecordsLast10Years + BankcardUtilization + DebtToIncomeRatio + LoanCurrentDaysDelinquent + 
                 CurrentlyInGroup + CreditScoreRange + Term, data = loans_train, dist = "weibull")
exp_fit <- psm(Surv(time, status) ~ BorrowerRate + OpenCreditLines + OpenRevolvingAccounts + InquiriesLast6Months + 
                 AmountDelinquent + DelinquenciesLast7Years + BankcardUtilization + StatedMonthlyIncome +
                 DebtToIncomeRatio + LoanCurrentDaysDelinquent + IsBorrowerHomeowner + Term,
               data = loans_train, dist = "exponential")
ll_fit <- psm(Surv(time, status) ~ OpenCreditLines + InquiriesLast6Months + AmountDelinquent + StatedMonthlyIncome +
                      BankcardUtilization + DebtToIncomeRatio + LoanCurrentDaysDelinquent + 
                      CurrentlyInGroup + CreditScoreRange + Term, data = loans_train, dist = "loglogistic")

Int_brier <- pec::pec(
  object=list("cox.model"=cox_final,
              "Exponential.model"=exp_fit,
              "lognormal.model"=log_fit,
              "loglogistic.model"=ll_fit,
              "Weibull.model"=Wei_fit),
  data = loans_test, 
  formula = Surv(time, status) ~ 1, 
  splitMethod = "none")
#crps(Int_brier)
#plot(Int_brier,xlab = "Time (days)")

coxPH <- round(c(D$logLik,length(summary(cox_final)),D$AIC,D$BIC, 1-D_cox[1], sqrt(B1$var),crps(Int_brier)[2,1]),digits=3)
names(coxPH) <-  c(expression(ell),"p","AIC","BIC","C--index", "se (C)","IBS")
exponential <- round(c(D1$logLik,D1$df,D1$AIC,D1$BIC,D_exp[1],sqrt(B2$var),crps(Int_brier)[3,1]),digits=3)
lognormal <- round(c(D3$logLik,D3$df,D3$AIC,D3$BIC,D_log[1], sqrt(B4$var),crps(Int_brier)[4,1]),digits=3)
Weibull <- round(c(D2$logLik,D2$df,D2$AIC,D2$BIC,D_wei[1], sqrt(B3$var),crps(Int_brier)[6,1]),digits=3)
loglogistic <- round(c(D5$logLik,D5$df,D5$AIC,D5$BIC,D_ll[1], sqrt(B6$var),crps(Int_brier)[5,1]),digits=3)

metrics <- rbind(coxPH,exponential,Weibull,gamma,lognormal,loglogistic)
colnames(metrics) <-  c(expression(ell),"p","AIC","BIC","c--index", "se (c)","IBS")

#tic("Validation for model calibration")
score <- riskRegression::Score(list("Model 1: Cox PH" =cph_fit,
                                    "Model 2: Weibull"=Wei_fit,
                                    "Model 3: Exponential"=exp_fit,
                                    "Model 4: Log-normal"=log_fit,
                                    "Model 5: Loglogistic-normal"=ll_fit), 
                               formula = Hist(time, status) ~ 1, 
                               data = loans_test, times = seq(180,max(loans_test$time),90),
                               split.method = "none", B=0,
                               metrics = c("brier","auc"),
                               cens.model="km",
                               plots = c("ROC","Calibration"),
                               summary = c("risks","ipa","ibs")
)
#toc()
#score
#score$AUC
#score$Brier
#plotAUC(score, ylim = c(0.8, 1.2), cex = .75)


# Tables of metrics for comparing models fitted using CC and best model estimates
#  knitr::kable(metrics,digits = c(0, 0, 0, 0, 3, 4, 4),col.names = c("$\\ell$", "$p$", "$AIC$", "$BIC$", "$C$--index", "$se(C)$", "$IBS$"),escape = TRUE,format = "simple",booktabs = TRUE,caption = "Comparison of Models using complete cases")


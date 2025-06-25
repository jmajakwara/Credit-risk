rm(list=ls())
library(MASS)
library(caret)
library(reticulate)
library(data.table)
library(survival)
library(ggplot2)
library(Hmisc)
library(BART)
library(rsample)
library(SurvMetrics)
library(prodlim)
library(pec)
library(rms)
library(cuRe)
library(flexsurv)
library(flexsurvcure)
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
loans$RevolvingCreditBalance <- as.numeric(loans$RevolvingCreditBalance)
loans$AvailableBankcardCredit <- as.numeric(loans$AvailableBankcardCredit)
loans$LoanOriginalAmount <- as.numeric(loans$LoanOriginalAmount)
loans$ListingCategory <- as.factor(loans$ListingCategory)
loans$EmploymentStatus <- as.factor(loans$EmploymentStatus)
loans$CreditScoreRange <- as.factor(loans$CreditScoreRange)
loans$Occupation <- as.factor(loans$Occupation)
#loans <- loans  %>% mutate_if(is.character, as.factor)
loans$CurrentlyInGroup <- as.factor(loans$CurrentlyInGroup)
loans$ProsperScore <- ifelse(loans$ProsperScore == 11,10,loans$ProsperScore)
loans$ProsperScore <- as.factor(loans$ProsperScore)
loans$Term <- as.factor(loans$Term)

# convert days to months


loans <- loans %>% filter(DebtToIncomeRatio < 1 | is.na(DebtToIncomeRatio))
loans <- loans %>% dplyr::select(-2,-Occupation,-ProsperScore)

loans_cc <- loans  %>% na.omit()

loans_cc <- loans_cc %>% select(time, status, OpenCreditLines, InquiriesLast6Months, AmountDelinquent,  BankcardUtilization, DebtToIncomeRatio,  BorrowerRate,
				LoanCurrentDaysDelinquent, CurrentlyInGroup, CreditScoreRange, Term, TradesNeverDelinquent,IsBorrowerHomeowner)
loans <- loans %>% select(-ProsperRating,-LenderYield,-CurrentCreditLines)



data_split <- initial_split(loans_cc, prop = 0.7, strata = status)     
#Create data frames for the two sets:
loans_train <- training(data_split)
loans_test  <- testing(data_split)

loans_train_all <- loans[!(loans$LoanKey %in% c(loans_test$LoanKey)),]


loans_train$Term <- as.factor(loans_train$Term)
loans_test$Term <- as.factor(loans_test$Term)

loans_test <- loans_test  %>%  select(time:Term)
loans_train <- loans_train  %>%  select(time:Term)
loans_train_all <- loans_train_all  %>%  select(time:Term)


###### Lognormal Modeling ########

loans_12 <- loans_cc %>% filter(Term == 12)
loans_12 <- as.data.frame(loans_12)
loans_36 <- loans_cc %>% filter(Term == 36)
loans_36 <- as.data.frame(loans_36)
loans_60 <- loans_cc %>% filter(Term == 60)
loans_60 <- as.data.frame(loans_60)

cat('The Generalised-gamma cure Model using flexsurvcure package  \n')



				q = c(seq(-3, -0.6, by=0.1), seq(-0.55, 1.15, by=0.1))
ggam <- matrix(NA,ncol=8,nrow=length(q))

		for (i in seq_along(q)) {

ggam_cc <- flexsurvcure::flexsurvcure(Surv(time,status) ~ InquiriesLast6Months + CreditScoreRange, 
							anc = list(mu = ~ OpenCreditLines  + BorrowerRate + Term  + BankcardUtilization + TradesNeverDelinquent +
							DebtToIncomeRatio + IsBorrowerHomeowner),
    							data = loans_cc,
    							dist = 'gengamma',
    							inits = c(0.45, 6.05,0.85, q[i]),
    							fixedpars = 4,
    							link = 'logistic',
    							mixture = TRUE)
res <- c(q[i], glance(ggam_cc)[["N"]], glance(ggam_cc)[["events"]],
            glance(ggam_cc)[["censored"]], glance(ggam_cc)[["df"]],
            glance(ggam_cc)[["logLik"]], glance(ggam_cc)[["AIC"]],
            glance(ggam_cc)[["BIC"]])


print(res)

colnames(ggam) <- c("q","n","events","censored","df","logLik","AIC","BIC")

ggam[i,] <- res
				}
cat("The value of q corresponding to maximum loglikelihood value \n")
print(ggam[which.max(ggam[, "logLik"]), "q"])

cat("The maximum loglikelihood value \n")
print(max(ggam[, "logLik"]))

print(ggam)

ggam[which.max(ggam[, "logLik"]), "q"]
max(ggam[, "logLik"])

pdf(file = "Profile likelihood of ggamma1 cure.pdf")

# Create ggplot object
gg <- ggplot(ggam, aes(x = q, y = logLik)) +
  geom_line(linewidth = 0.5) +  
  annotate("segment", x = min(ggam[,"q"]), y = max(ggam[,"logLik"]), 
           xend = ggam[which.max(ggam[, "logLik"]), "q"], 
           yend = max(ggam[,"logLik"]), linewidth = 0.4, 
           linetype = "dotted", color = "blue") + 
  annotate("segment", x = ggam[which.max(ggam[, "logLik"]), "q"], 
           y = max(ggam[, "logLik"]), 
           xend = ggam[which.max(ggam[, "logLik"]), "q"],
           yend = min(ggam[, "logLik"]) - 100, 
           linewidth = 0.4, linetype = "dotted", color = "blue") +
  annotate("segment", x = ggam[which.max(ggam[, "logLik"]), "q"], 
           y = min(ggam[, "logLik"]), 
           arrow = arrow(length = unit(0.3, "cm")), 
	   xend = ggam[which.max(ggam[, "logLik"]), "q"], 
           yend = min(ggam[, "logLik"]) - 150, 
           linewidth = 0.4, linetype = "solid", color = "blue") +  
  labs(x = "Shape parameter (q)",
       y = "Profile log-likelihood value") +
  scale_y_continuous(limits = c(min(ggam[, "logLik"])-100, max(ggam[, "logLik"])+100)) +
  theme_minimal() + 
  theme(legend.position = "right")
# Print plot
print(gg)
# Close PDF device
dev.off()


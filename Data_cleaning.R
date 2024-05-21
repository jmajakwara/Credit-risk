##---- Cleaning ---------------------------------------------------------

library(tidyverse)
library(caret)
library(data.table)
library(survival)

filename= "https://s3.amazonaws.com/udacity-hosted-downloads/ud651/prosperLoanData.csv"
loans.data <-  read.csv(filename, header = TRUE, sep = ",", na.strings="NA")
dim(loans.data)
str(loans.data)
## Converting a factor variable which is recorded as integer
loans.data[, which(colnames(loans.data) == "Term")]<-as.factor(loans.data$Term)
loans.data[, which(colnames(loans.data) == "ProsperRating..numeric.")]<-as.factor(loans.data$ProsperRating..numeric.)
#table(loans.data$LoanStatus, useNA = "always")
table(loans.data$ProsperScore, useNA = "always")
dim(loans.data)
max(loans.data$LoanOriginationDate)


## Removing duplicates
loan_nd<-loans.data[!duplicated(loans.data$LoanKey),]


## creating status variable for censoring
loans<-loan_nd %>% mutate(status = ifelse(loan_nd$LoanStatus=="Defaulted" | loan_nd$LoanStatus=="Chargedoff" |
                loan_nd$LoanStatus=="Past Due (>120 days)",1,0)) 

loans[, which(colnames(loans) == "status")]<-as.factor(loans$status)

table(loans$status)
## adding the final date to Closed Date variable
loans$ClosedDate <- as.Date(loans$ClosedDate, format = "%Y-%m-%d")
loans$ClosedDate[is.na(loans$ClosedDate)] <- as.Date("2014-11-03")


## creating the time-to-event variable
loans$start<-as.Date(loans$LoanOriginationDate)
loans$end<-as.Date(loans$ClosedDate)
loans$time<-as.numeric(difftime(loans$end,loans$start,units="days"))
#table(filter(loans, time<0)$time)
#length(filter(loans, time<0)$time)
#table(filter(loans, time<0)$start)
#table(filter(loans, time<0)$end)
loans<-loans[-loans$time < 0,] #so that there is no time less than 0


## Converting date variables to be in date format
loans[,which(colnames(loans) == "ListingCreationDate")]<-as.Date(loans$ListingCreationDate)
loans[,which(colnames(loans) == "LoanOriginationDate")]<-as.Date(loans$LoanOriginationDate)
loans[,which(colnames(loans) == "ClosedDate")]<-as.Date(loans$ClosedDate)
loans[,which(colnames(loans) == "FirstRecordedCreditLine")]<-as.Date(loans$FirstRecordedCreditLine)


## removing variables that have nothing to do with PD
# LoanKey and ListingKey maintained for identification when imputing
id_var<-c(which(colnames(loans)=="ListingNumber"),which(colnames(loans)=="ListingCreationDate"),
          which(colnames(loans)=="ClosedDate"),which(colnames(loans)=="GroupKey"),which(colnames(loans)=="DateCreditPulled"),
          which(colnames(loans) =="LoanFirstDefaultedCycleNumber"),which(colnames(loans)=="LoanNumber"),
          which(colnames(loans)=="MemberKey"),which(colnames(loans)=="start"),which(colnames(loans)=="end"))

loans<-select(loans,-all_of(id_var))  
# can also use loans<-loans[,-id_var]

loans <- mutate_if(loans, is.character, as.factor)


## Dealing with loans from "2009-08-01"
loans_filtered<-filter(loans, LoanOriginationDate > as.Date("2009-07-31"))
#dim(loans_filtered)
#min(loans_filtered$LoanOriginationDate)


## Replacing zeros in Income variable with NA for those Employed and Full-time on Employment Status and DebtToIncomeRatio
#table(filter(loans_filtered,StatedMonthlyIncome==0)$EmploymentStatus)

loans_filtered$StatedMonthlyIncome<-ifelse(loans_filtered$StatedMonthlyIncome==0 & loans_filtered$EmploymentStatus=="Employed" | 
         loans_filtered$StatedMonthlyIncome==0 & loans_filtered$EmploymentStatus=="Full-time", NA,loans_filtered$StatedMonthlyIncome)

loans_filtered$DebtToIncomeRatio <- ifelse(loans_filtered$DebtToIncomeRatio==0, NA,loans_filtered$DebtToIncomeRatio)


#table(filter(loans_filtered,is.na(StatedMonthlyIncome))$EmploymentStatus)

## Calculating percentage of missingness

# sapply(loans_filtered1, function(x) sum(is.na(x))/n*100) this can also be used
missingness <- colMeans(is.na(loans_filtered))*100
#sort(missingness)


## variables with high missingness > 76%
high_miss<-c("TotalProsperLoans","TotalProsperPaymentsBilled", "OnTimeProsperPayments" ,"ProsperPaymentsLessThanOneMonthLate",
            "ProsperPaymentsOneMonthPlusLate", "ProsperPrincipalBorrowed", "ProsperPrincipalOutstanding",
            "ScorexChangeAtTimeOfListing", "CreditGrade")

## Variables that have same information
rep_var<-c("FirstRecordedCreditLine","IncomeRange","LoanOriginationDate","LoanStatus",
           "LoanOriginationQuarter", "ProsperRating..Alpha.")


## Removing variables with high missingness >50% and same information
loans_final<-select(loans_filtered,-all_of(c(high_miss,rep_var)))  

loans_final <- loans_final %>% rename(c("ProsperRating" = "ProsperRating..numeric.",
                                        "ListingCategory" = "ListingCategory..numeric.",
                                        "TradesNeverDelinquent" = "TradesNeverDelinquent..percentage."))
## Combining credit scores lower and upper range into  credit score range
loans_final <- loans_final %>% 
                 unite(CreditScoreRange,CreditScoreRangeLower,CreditScoreRangeUpper,sep = "-")

loans_final[,which(colnames(loans_final) == "CreditScoreRange")]<-as.factor(loans_final$CreditScoreRange)

table(loans_final$CreditScoreRange)
# categorising credit range into Poor, Fair, Good, Very Good and Excellent

loans_final <- loans_final %>% 
  mutate(
    CreditScoreRange=case_match(CreditScoreRange,c('600-619','620-639','640-659') ~ "Fair",
                                c('660-679','680-699','700-719','720-739') ~ "Good", c('740-759','760-779','780-799') ~ "Very good",
                                c('800-819','820-839','840-859','860-879','880-899') ~ "Excellent")
         ) 

loans_final$CreditScoreRange  <- factor(as.factor(loans_final$CreditScoreRange),ordered = TRUE,
                                    levels=c("Fair","Good","Very good","Excellent"))

str(loans_final)
## Arranging variables
var <- c("LoanKey","ListingKey","time","status","BorrowerRate","LenderYield","ProsperScore","EmploymentStatusDuration",
         "CurrentCreditLines","OpenCreditLines","TotalCreditLinespast7years","OpenRevolvingAccounts",
         "OpenRevolvingMonthlyPayment","InquiriesLast6Months","TotalInquiries","RevolvingCreditBalance",
         "CurrentDelinquencies", "AmountDelinquent","DelinquenciesLast7Years","PublicRecordsLast10Years",
         "PublicRecordsLast12Months","BankcardUtilization","AvailableBankcardCredit","DebtToIncomeRatio",
         "LoanOriginalAmount","PercentFunded","StatedMonthlyIncome","TradesNeverDelinquent",
         "LoanCurrentDaysDelinquent","IncomeVerifiable","ProsperRating","ListingCategory","EmploymentStatus",
         "Occupation","CurrentlyInGroup","IsBorrowerHomeowner","CreditScoreRange","Term")
length(var)
loans_final <- loans_final %>% select(all_of(var),everything())

## Final data to be used for analysis
write.csv(loans_final,file="C:\\Users\\Jacob\\OneDrive - University of Witwatersrand\\RESEARCH\\Credit Risk Modeling\\Imputation\\Data\\loans_final_jj1.csv", row.names = FALSE)
str(loans_final)

## Using Loans when Term==36 months
loans36<-filter(loans_final, Term=="36")
n36=dim(loans36)[1]
missing36<- summarise_all(loans36,~sum(is.na(.))/n36*100 -> m36)
#table(filter(loans36,StatedMonthlyIncome<1160)$EmploymentStatus)

sort(missing36)
dim(loans36)
str(loans36)

## Finding Pearson correlation for numerical variables
loans_36 <- loans36 %>% select(where(is.numeric))
loans36_complete_cases <- filter(loans_36,complete.cases(loans_36)==TRUE) #CC
Corr_CC <- cor(loans36_complete_cases) #calculating correlations for continuous complete cases
highly_correlated <- findCorrelation(Corr_CC, cutoff = 0.9, names = TRUE,exact=FALSE) # function in caret package
highly_correlated


# Function to check variables with higher correlations
corr_check <- function(dataset, threshold = 0.7) {
  
          # Calculate the correlation matrix
        cor_matrix <- cor(dataset)
  
          # Find the indices of the highly correlated variables
        high_cor <- which(abs(cor_matrix) > threshold & upper.tri(cor_matrix, diag = FALSE), arr.ind = TRUE)
  
          # Print the names of the highly correlated variables
        if (length(high_cor) > 0) {
              cat("The following variables have correlations higher than the threshold:\n")
            for (i in 1:nrow(high_cor)) {
               cat(paste0(" . ", rownames(cor_matrix)[high_cor[i, 1]], " and ", colnames(cor_matrix)[high_cor[i, 2]], "\n"))
                                        }
                                   } else {
                                    cat("No variables have correlations higher than the threshold.")
                                           }
                                                    }

corr_check(loans36_complete_cases,0.9)






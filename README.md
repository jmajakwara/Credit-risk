This research was done using personal loan data from Prosper, a P2P lending institution in the US. The data is publicly available and can be downloaded from https://s3.amazonaws.com/udacity-hosted-downloads/ud651/prosperLoanData.csv.

The study entailed fitting the generalised gamma regression cure rate model to complete cases (CC) data, then using MICE to impute missing values on the variable "debt-to-income ratio" and remodelling again. This is done to underscore the need to use a principled approach to data imputation in order to prevent unbiased estimates, ensure completeness of analysis, and comply with data standards that require datasets to meet specific requirements for missing values.

The files are organised as follows:
1. Data_cleaning.R is code for importing the original data and apply pre-processing techniques
2. The data utilised after Data_cleaning.R is named loans_final_data.csv
3. Multicollinerity.R investigate variables that are collinear before applying variable selection
4. CC_models.R compares the models fitted on complete cases, and
5. Imputed_ggam.R is the code for imputing and modeling using generalised gamma.

# HONORS RESEARCH: RETENTION MODEL

# Seth Cooper
# Fall Semester, 2022
# Department of Economics and Business Administration

# Study: Effects of Instructional Staff Characteristics on Student Outcomes
# Planned Method: Stepwise Regression
# Dependent Variable: Student Retention
# Data Source(s): College Scorecard and IPEDS

################################################################################

# Installing and Loading Packages

library(dplyr) # Data wrangling 
library(purrr) # Quick repeated regressions and neat results
library(readr) # Data reading
library(broom) # Tidy results into easy to read formats
library(olsrr) # Stepwise and residual functions

################################################################################

# Data Import

# main_data <-  
#   read.csv(
#     "C:/Users/coope/OneDrive/Desktop/Fall22/Honors_Research/Data/CSVs/Working.Data.10.20.2022.csv", 
#     header = T, sep = ",") # Imports CSV file for primary data

main_data <-  
  read.csv(
    "C:/Users/coope/OneDrive/Desktop/School (PC)/Honors_Research/Data/CSVs/Working.Data.10.20.2022.csv", 
    header = T, sep = ",") # Imports CSV file for primary data

attach(main_data) # Removes need for specifying data frame when calling variables.

################################################################################

# Retention Univariate Regressions

ivs <- colnames(main_data)[13:ncol(main_data)] # Select names of column names 
names(ivs) <- ivs # Puts all indep. vars. into a list 

ret_glance <- main_data %>%
  select(ivs) %>% 
  map(~ lm(Retention ~ .x, data = main_data)) %>%
  map_df(glance, .id='variable') %>% 
  select(variable, r.squared, p.value, statistic)

ret_tidy <- main_data %>%
  select(ivs) %>% 
  map(~ lm(Retention ~ .x, data = main_data)) %>%
  map_df(tidy, .id='variable') %>% 
  select(variable,term,estimate)

ret_coef <- ret_tidy[seq(2,nrow(ret_tidy),2),]

ret_reg <- cbind(ret_coef,ret_glance[2:ncol(ret_glance)])
ret_results <- ret_reg %>% 
  select(!term)

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
} # A function that copies console to clipboard

write.excel(ret_results) # Apply created function to outputs from regs

################################################################################

# Creating the Final Model for Retention

ret_step_data <- main_data %>% # Data frame, all variables and retention
  select(Retention, ivs) %>% 
  select(-PCTFacFemale,-PCTFacNotWhite,-PCTFacRace.EthnicUnk,
         -PCTFacTwoMoreRace.Ethnic,-PCTFacMale,-PCTFacWhite,-PCTFacNRA,
         -InstrPerStu) %>% 
  filter(Retention > 0)

ret_model <- lm(Retention ~ ., data = ret_step_data)
# This model is fed into the stepwise function and the regression is completed
# based on the designated dependent and independent variables

ret_aic_back <- ols_step_backward_aic(ret_model,details = T) 
# ^^ Conducts backwards stepwise reg. w/ AIC as drop criteria

ret_aic_back # Shows variables dropped from backwards steps

back_pred_elim <- ret_aic_back$predictors # List of dropped predictors

ret_model_data <- ret_step_data %>% # Data frame of chosen predictors and retention
  select(-back_pred_elim)

# Note, everything aside from "back_pred_elim" was dropped due to multi-collinearity or 
# because it had a p-value > 0.05 -> More info in markdown

final_ret_model <- lm(Retention ~ ., data = ret_model_data) # Creates actual model

################################################################################

# Evaluating the "Ideal" Retention Model

summary(final_ret_model) # Summary Stats.

coll_test <- ols_coll_diag(final_ret_model) # Various collinearity tests

coll_test$vif_t # VIF test results

Eigenvalue <- coll_test$eig_cindex$Eigenvalue
ConditionIndex <- coll_test$eig_cindex$`Condition Index`

CITest <- cbind(Eigenvalue,ConditionIndex)
CITest # Displays results from condition index test 

################################################################################

# Creating the Second Model for Retention


ret_model_data_2 <- ret_model_data %>% 
  select(-PCTFTTenureTrk)

final_ret_model_2 <- lm(Retention ~ ., data = ret_model_data_2) # Creates actual model

################################################################################

# Evaluating the Second Retention Model

summary(final_ret_model_2) # Summary Stats.

coll_test_2 <- ols_coll_diag(final_ret_model_2) # Various collinearity tests

coll_test_2$vif_t # VIF test results

Eigenvalue_2 <- coll_test_2$eig_cindex$Eigenvalue
ConditionIndex_2 <- coll_test_2$eig_cindex$`Condition Index`

CITest_2 <- cbind(Eigenvalue_2,ConditionIndex_2)
CITest_2 # Displays results from condition index test 

################################################################################

# Creating the Third Model for Retention

ret_model_data_3 <- ret_model_data_2 %>% 
  select(-AcadmicSupportWagesPerStudent,-PCTFTNotTenure)

final_ret_model_3 <- lm(Retention ~ ., data = ret_model_data_3) # Creates actual model

################################################################################

# Evaluating the Third Retention Model

summary(final_ret_model_3) # Summary Stats.


coll_test <- ols_coll_diag(final_ret_model) # Various collinearity tests

coll_test$vif_t # VIF test results

Eigenvalue <- coll_test$eig_cindex$Eigenvalue
ConditionIndex <- coll_test$eig_cindex$`Condition Index`

CITest <- cbind(Eigenvalue,ConditionIndex)
CITest # Displays results from condition index test 



################################################################################

# Write model results to excel CSV

# tidy_ret <- tidy(final_ret_model_3)
# tidy_ret
# write.csv(tidy_ret, 
#           "C:/Users/jac110/Desktop/Honors_Research/R Files/Retention Model/ret_summary.csv")
# 
# 
# ols_plot_resid_fit(final_ret_model_3)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


# Creating the Graduation Model

grad_step_data <- main_data %>% # Data frame, all variables and retention
  select(X6yrGradRate, ivs) %>% 
  select(-PCTFacFemale,-PCTFacNotWhite,-PCTFacRace.EthnicUnk,
         -PCTFacTwoMoreRace.Ethnic,-PCTFacMale,-PCTFacWhite,-PCTFacNRA,
         -InstrPerStu) %>% 
  filter(X6yrGradRate > 0)

grad_model <- lm(X6yrGradRate ~ ., data = grad_step_data)
# This model is fed into the stepwise function and the regression is completed
# based on the designated dependent and independent variables

grad_aic_back <- ols_step_backward_aic(grad_model,details = T) 
# ^^ Conducts backwards stepwise reg. w/ AIC as drop criteria

grad_aic_back # Shows variables dropped from backwards steps

grad_pred_elim <- grad_aic_back$predictors # List of dropped predictors

grad_model_data <- grad_step_data %>% # Data frame of chosen predictors and retention
  select(-grad_pred_elim)

# Note, everything aside from "back_pred_elim" was dropped due to multi-collinearity or 
# because it had a p-value > 0.05 -> More info in markdown

grad_model <- lm(X6yrGradRate ~ ., data = grad_model_data) # Creates actual model

################################################################################

summary(grad_model) # Summary Stats.

grad_coll_test <- ols_coll_diag(grad_model) # Various collinearity tests

grad_coll_test$vif_t # VIF test results

Grad_Eigenvalue <- grad_coll_test$eig_cindex$Eigenvalue
Grad_ConditionIndex <- grad_coll_test$eig_cindex$`Condition Index`

Grad_CITest <- cbind(Grad_Eigenvalue,Grad_ConditionIndex)
Grad_CITest # Displays results from condition index test 


################################################################################

# Creating the Second Model for Graduation


grad_model_data_2 <- grad_model_data %>% 
  select(-PCTAssoProf,-PCTInstructors)

grad_model_2 <- lm(X6yrGradRate ~ ., data = grad_model_data_2) # Creates actual model

################################################################################

# Evaluating the Second Graduation Model

summary(grad_model_2) # Summary Stats.

coll_test_2 <- ols_coll_diag(final_ret_model_2) # Various collinearity tests

coll_test_2$vif_t # VIF test results

Eigenvalue_2 <- coll_test_2$eig_cindex$Eigenvalue
ConditionIndex_2 <- coll_test_2$eig_cindex$`Condition Index`

CITest_2 <- cbind(Eigenvalue_2,ConditionIndex_2)
CITest_2 # Displays results from condition index test 


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


# Creating the Earnings Model

earn_step_data <- main_data %>% # Data frame, all variables and retention
  select(MDEarnings8yr, ivs) %>% 
  select(-PCTFacFemale,-PCTFacNotWhite,-PCTFacRace.EthnicUnk,
         -PCTFacTwoMoreRace.Ethnic,-PCTFacMale,-PCTFacWhite,-PCTFacNRA,
         -InstrPerStu) %>% 
  filter(MDEarnings8yr > 0)

earn_model <- lm(MDEarnings8yr ~ ., data = earn_step_data)
# This model is fed into the stepwise function and the regression is completed
# based on the designated dependent and independent variables

earn_aic_back <- ols_step_backward_aic(earn_model,details = T) 
# ^^ Conducts backwards stepwise reg. w/ AIC as drop criteria

earn_aic_back # Shows variables dropped from backwards steps

earn_pred_elim <- earn_aic_back$predictors # List of dropped predictors

earn_model_data <- earn_step_data %>% # Data frame of chosen predictors and retention
  select(-earn_pred_elim)

# Note, everything aside from "back_pred_elim" was dropped due to multi-collinearity or 
# because it had a p-value > 0.05 -> More info in markdown

earn_model <- lm(MDEarnings8yr ~ ., data = earn_model_data) # Creates actual model

################################################################################

summary(earn_model) # Summary Stats.

earn_coll_test <- ols_coll_diag(earn_model) # Various collinearity tests

earn_coll_test$vif_t # VIF test results

earn_Eigenvalue <- earn_coll_test$eig_cindex$Eigenvalue
earn_ConditionIndex <- earn_coll_test$eig_cindex$`Condition Index`

earn_CITest <- cbind(earn_Eigenvalue,earn_ConditionIndex)
earn_CITest # Displays results from condition index test 


################################################################################

# Creating the Second Model for Graduation


earn_model_data_2 <- earn_model_data %>% 
  select(-PCTNoRank,-PCTFTStaff,-PCTFTTenured)

earn_model_2 <- lm(MDEarnings8yr ~ ., data = earn_model_data_2) # Creates actual model

################################################################################

# Evaluating the Second Graduation Model

summary(earn_model_2) # Summary Stats.

earn_coll_test_2 <- ols_coll_diag(earn_model_2) # Various collinearity tests

earn_coll_test_2$vif_t # VIF test results

earn_Eigenvalue_2 <- earn_coll_test_2$eig_cindex$Eigenvalue
earn_ConditionIndex_2 <- earn_coll_test_2$eig_cindex$`Condition Index`

earn_CITest_2 <- cbind(earn_Eigenvalue_2,earn_ConditionIndex_2)
earn_CITest_2 # Displays results from condition index test 


################################################################################

# Creating the Second Model for Graduation


earn_model_data_3 <- earn_model_data_2 %>% 
  select(-PCTFTNotTenure,-PCTAssisProf,-PCTInstructors)

earn_model_3 <- lm(MDEarnings8yr ~ ., data = earn_model_data_3) # Creates actual model

################################################################################

# Evaluating the Second Graduation Model

summary(earn_model_3) # Summary Stats.

earn_coll_test_2 <- ols_coll_diag(earn_model_2) # Various collinearity tests

earn_coll_test_2$vif_t # VIF test results

earn_Eigenvalue_2 <- earn_coll_test_2$eig_cindex$Eigenvalue
earn_ConditionIndex_2 <- earn_coll_test_2$eig_cindex$`Condition Index`

earn_CITest_2 <- cbind(earn_Eigenvalue_2,earn_ConditionIndex_2)
earn_CITest_2 # Displays results from condition index test 

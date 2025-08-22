# File: models.R
# Author: Hunter Moricz
# Date: July, 2024
# Description: Trains and evaluates multiple churn prediction models 
# (logistic regression, trees, random forest, neural nets), compares 
# performance with lift charts, and explores satisfaction as a target.
# ------------------------------------------------------------------------------
# ------------------------------ File Setup ------------------------------------
# Load Packages
library(tidyverse)
library(car)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)

# Source Files
source("Helper Functions Scripts/BCA_functions_source_file.R")
source("Helper Functions Scripts/utils_graphing.R")
source("Helper Functions Scripts/utils_modelling.R")

# Load the data by sourcing the wrangling file
source("data_preparation.R")

# ------------------------------ Modelling Setup -------------------------------

# Save a table that removes variables that are entirely irrelevant to or too complicated for the modeling process
ModelingData <- select(SingleTable, -Customer_ID, -Count, -Country, -Senior,
                       -State, -City, -Zip_Code, -Lat_Long, -Age_Category,
                       -Quarter, -Customer_Status, -Churn_Category, -Churn_Reason)

### Create a modelling dataframe with even proportions of churners and non-churners
# Randomly Sample an even number of churners and non-churners -> equal-proportion modelling
set.seed(136) # Set a random seed
sample0 <- filter(ModelingData, Churn_Value_Factor == 0) %>% slice_sample(n = 1800)
sample1 <- filter(ModelingData, Churn_Value_Factor == 1) %>% slice_sample(n = 1800)
# Put the two samples into one dataframe and randomly shuffle the rows
ModelingData_ProportionalSample <- bind_rows(sample0, sample1) %>% 
  slice_sample(prop = 1) %>% 
  select(-Churn_Value, -Churn_Label)
  
# All the variables in the modelling dataframe
variables <- colnames(ModelingData)
# The key target variable(s)
targets <- c("Churn_Label", "Churn_Value", "Churn_Value_Factor")
# List of predictors to analyze/model
predictor_list <- variables[!(variables %in% targets)]

# ----------------------- Logistic Regression Modelling ------------------------
# See which predictors perform the best when modelling with churn as the target

# Run a logistic regression model for each predictor and using stepwise regression
MisMat <- runall_models(ModelingData_ProportionalSample, predictor_list, 10)
AvgMisMat <- makeAverageMisclassTibble(MisMat)
print(AvgMisMat, n = Inf)

# Model has a hit rate of approximately 94%
# Stepwise Logistic Regression Predictor Selection
# - Satisfaction Score
# - Online Security
# - Contract
# - Dependents
# - Number of Referrals
# - Referred a friend
# - Monthly Charge
# - New Customer
# - Zip Code Population


# ------------------------- Selecting a Model for Churn ------------------------
# Set a new random seed
set.seed(737)

# Training-Test Split
LiftChartData <- training_validation_split(ModelingData)
LC_TrainingData <- getTrainingData(LiftChartData)
LC_ValidationData <- getValidationData(LiftChartData)
actual <- LC_ValidationData$Churn_Value
LC_TrainingData <- select(LC_TrainingData, -Churn_Value, -Churn_Value_Factor)
LC_ValidationData <- select(LC_ValidationData, -Churn_Value, -Churn_Value_Factor)

### Logistic Regression Model
LogisticModel <- glm(Churn_Label ~ Satisfaction_Score + Online_Security + 
                       Contract + Dependents + Number_of_Referrals + 
                       Referred_a_Friend + Monthly_Charge + New_Customer + 
                       ZipCode_Population, 
                     data = LC_TrainingData, 
                     family = binomial(logit))
summary(LogisticModel)

round((hit_rate_logistic = 1 - getMisclassificationRate(LogisticModel, actual, newdata = LC_ValidationData)), 4) * 100 # = 95.06%

### Tree Model
TreeModel <- rpart(Churn_Label ~ Satisfaction_Score + Contract + 
                     Online_Security + Monthly_Charge + Age + 
                     Total_Revenue, 
                   data = LC_TrainingData, 
                   cp = 0.009, 
                   model = TRUE)

printcp(TreeModel)
plotcp(TreeModel, upper = "splits")
TreeModel$cptable

rpart.plot(TreeModel, type = 1, extra = 2,
           fallen.leaves = TRUE, uniform = FALSE, digits = 3, 
           yes.text = "true", no.text = "false", cex = 0.7)
# Important Predictors:
# - Satisfaction_Score
# - Contract
# - Online_Security
# - Monthly_Charge
# - Total_Revenue
# - Age

Tree_predictions <- round(predict(TreeModel, newdata = LC_ValidationData)[,2])
round((hit_rate_tree = 1 - mean(Tree_predictions != actual)), 4) * 100 # = 95.29%

# Tree Model setting satisfaction score == 3
TreeModel_SatEqThree <- rpart(Churn_Label ~ .,
                              data = filter(LC_TrainingData, Satisfaction_Score == 3), 
                              cp = 0.05, 
                              model = TRUE)

rpart.plot(TreeModel_SatEqThree, type = 1, extra = 2,
           fallen.leaves = TRUE, uniform = FALSE, digits = 3, 
           yes.text = "true", no.text = "false", cex = 0.7)
# Contract, Online_Security, Age, Monthly_Charge, and Total_Revenue become the most important predictors in this case

# Tree Model without satisfaction
TreeModel_noSatisfaction <- rpart(Churn_Label ~ .,
                                  data = select(LC_TrainingData, -Satisfaction_Score, -Satisfaction_Level), cp = 0.05, model = TRUE)

rpart.plot(TreeModel_noSatisfaction, type = 1, extra = 2,
           fallen.leaves = TRUE, uniform = FALSE, digits = 3, 
           yes.text = "true", no.text = "false", cex = 0.7)
# In this case, Contract, Age, Number_of_Referrals, and Internet_Type become the most important

### Random Forest Model
RFModel <- randomForest(formula = Churn_Label ~  Age + Age_Range + 
                          Dependents + Number_of_Dependents + City_Population + 
                          ZipCode_Population + Latitude + Longitude + 
                          Number_of_Referrals + Tenure_in_Months + 
                          New_Customer + Offer + 
                          Avg_Monthly_Long_Distance_Charges + 
                          Internet_Service + Internet_Type + 
                          Avg_Monthly_GB_Download + Online_Security + 
                          Premium_Tech_Support + 
                          Streaming_Music + 
                          Contract + Paperless_Billing + Payment_Method + 
                          Monthly_Charge + Total_Charges + 
                          Total_Long_Distance_Charges + 
                          Total_Revenue + CLTV + Satisfaction_Score + Satisfaction_Level, 
                        data = LC_TrainingData, importance = TRUE, ntree = 2000, mtry = 6)
RFModel
importance(RFModel, type = 2)

# Plot importance
varImpPlot(RFModel,type = 2, main = "Importance Plot")
# Satisfaction, Contract, Tenure_in_Months, Number_of_Referrals, and Internet_Type are the most important in the RF model

# Generate Partial Dependence Plot for Satisfaction_Score
partialPlot(RFModel,
            pred.data = data.frame(LC_ValidationData),
            x.var = Satisfaction_Score,
            sub = "Validation Set", # a subtitle
            which.class = "Yes") # target level probability prediction
# Note: Partial Plot function does not work with a tibble

RF_predictions <- round(predict(RFModel, newdata = LC_ValidationData, type = "prob")[, 2])
round((hit_rate_rf = 1 -  mean(RF_predictions != actual)), 4) * 100 # = 96.2%

### Neural Network
NeuralNet <- Nnet(Churn_Label ~ Age + Age_Range + 
                    Dependents + Number_of_Dependents + City_Population + 
                    ZipCode_Population + Latitude + Longitude + 
                    Number_of_Referrals + Tenure_in_Months + 
                    New_Customer + Offer + 
                    Avg_Monthly_Long_Distance_Charges + 
                    Internet_Service + Internet_Type + 
                    Avg_Monthly_GB_Download + Online_Security + 
                    Premium_Tech_Support + 
                    Streaming_Music + 
                    Contract + Paperless_Billing + Payment_Method + 
                    Monthly_Charge + Total_Charges + 
                    Total_Long_Distance_Charges + 
                    Total_Revenue + CLTV + Satisfaction_Score + Satisfaction_Level,
                  data = LC_TrainingData,
                  decay = 0.10,
                  size = 2)

NeuralNet$value
summary(NeuralNet)

NN_predictions <- round(predict(NeuralNet, newdata = LC_ValidationData))
round((hit_rate_nn = 1 -  mean(NN_predictions != actual)), 4) * 100 # = 95.12%


### LIFT CHART(s)
# Validation Cumulative Lift Chart for all of the above Models
lift.chart(modelList = c("LogisticModel", "TreeModel", "RFModel", "NeuralNet"),
           data = LC_ValidationData,
           targLevel = "Yes", trueResp = 0.265,
           type = "cumulative", sub = "Validation Set")

# Validation Cumulative Lift Chart for Logistic Regression only
lift.chart(modelList = c("LogisticModel"),
           data = LC_ValidationData,
           targLevel = "Yes", trueResp = 0.265,
           type = "cumulative", sub = "Validation Set")

hit_rates <- c(hit_rate_logistic, hit_rate_tree, hit_rate_rf, hit_rate_nn)
names(hit_rates) <- c("Logistic", "Tree", "Random Forest", "Neural Network")
print(hit_rates)


# ------------- Predicting Churn without Known Satisfaction Levels -------------
# Our next task is to be able to predict churn without knowing customer satisfaction levels
# We should only use variables pertaining to demographics and behaviours

### Stepwise Model without the satisfaction variable
initial_noSat <- glm(data = select(LC_TrainingData, -Satisfaction_Score, -Satisfaction_Level), 
                     formula = Churn_Label ~ 1, family = binomial(logit))
final_noSat <- glm(data = select(LC_TrainingData, -Satisfaction_Score, -Satisfaction_Level), 
                   formula = Churn_Label ~ ., family = binomial(logit))
step_model_noSat <- step(object = initial_noSat, scope = list(upper = final_noSat), 
                         k = log(nrow(LC_TrainingData)))
# The stepwise model above selected the predictors:
# - Contract
# - Internet_Type
# - Number of Referrals
# - Referred_a_Friend
# - Dependents
# - New_Customer
# - Payment Method
# - Age
# - Steaming_TV
# - Paperless_Billing
# - Tenure_in_Months
# - City_Population
# - LargeCity
# - Streaming_Music
# - Premium_Tech_Support
# - Online_Security

### Logistic Regression Model
LogisticModel_noSat <- glm(Churn_Label ~ Contract + Internet_Type + Number_of_Referrals + 
                             Referred_a_Friend + Dependents + New_Customer + 
                             Payment_Method + Age + Streaming_TV + Paperless_Billing + 
                             Tenure_in_Months + City_Population + LargeCity + Streaming_Music + 
                             Premium_Tech_Support + Online_Security, 
                           data = LC_TrainingData, family = binomial(logit))

round((hit_rate_noSat = 1 - getMisclassificationRate(LogisticModel_noSat, actual, newdata = LC_ValidationData)), 4) * 100

summary(LogisticModel_noSat)

### Tree Model
TreeModel_noSat <- rpart(Churn_Label ~ Contract + Age + Number_of_Referrals + Internet_Type,
                         data = select(LC_TrainingData, -Satisfaction_Score, -Satisfaction_Level), cp = 0.053, model = TRUE)

TreeModel_noSat

printcp(TreeModel_noSat)
plotcp(TreeModel_noSat, upper = "splits")
TreeModel_noSat$cptable

Tree_nosat_predictions <- round(predict(TreeModel_noSat, newdata = LC_ValidationData)[,2])
round((hit_rate_tree_noSat = 1 -  mean(Tree_nosat_predictions != actual)), 4) * 100

rpart.plot(TreeModel_noSat, type = 1, extra = 2,
           fallen.leaves = TRUE, uniform = FALSE, digits = 3, 
           yes.text = "true", no.text = "false", cex = 0.7)
# Important Predictors:
# - Contract
# - Age
# - Number of Referrals
# - Internet_Type

### Random Forest Model
RFModel_noSat <- randomForest(Churn_Label ~  Age + Age_Range + 
                                Dependents + Number_of_Dependents + City_Population + 
                                ZipCode_Population + Latitude + Longitude + 
                                Referred_a_Friend + Number_of_Referrals + Tenure_in_Months + 
                                New_Customer + Offer + 
                                Avg_Monthly_Long_Distance_Charges + 
                                Internet_Service + Internet_Type + 
                                Avg_Monthly_GB_Download + Online_Security + 
                                Premium_Tech_Support + 
                                Contract + Paperless_Billing + Payment_Method + 
                                Monthly_Charge + Total_Charges + 
                                Total_Extra_Data_Charges + Total_Long_Distance_Charges + 
                                Total_Revenue + CLTV, 
                              data = LC_TrainingData, importance = TRUE, ntree = 2000, mtry = 6)

importance(RFModel_noSat, type = 2)
varImpPlot(RFModel_noSat,type = 2, main = "Importance Plot")
# Finds that the most important variables are Contract, Monthly_Charge, Tenure_in_Months, Number_of_Referrals

RF_notsat_predictions <- round(predict(RFModel_noSat, newdata = LC_ValidationData, type = "prob")[, 2])
round((hit_rate_rf_noSat = 1 -  mean(RF_notsat_predictions != actual)), 4) * 100

### Neural Network
NeuralNet_noSat <- Nnet(Churn_Label ~  Age + Age_Range + 
                          Dependents + Number_of_Dependents + City_Population + 
                          ZipCode_Population + Latitude + Longitude + 
                          Referred_a_Friend + Number_of_Referrals + Tenure_in_Months + 
                          New_Customer + Offer + 
                          Avg_Monthly_Long_Distance_Charges + 
                          Internet_Service + Internet_Type + 
                          Avg_Monthly_GB_Download + Online_Security + 
                          Premium_Tech_Support + 
                          Contract + Paperless_Billing + Payment_Method + 
                          Monthly_Charge + Total_Charges + 
                          Total_Extra_Data_Charges + Total_Long_Distance_Charges + 
                          Total_Revenue + CLTV,
                        data = LC_TrainingData,
                        decay = 0.10,
                        size = 2)

NN_nosat_predictions <- round(predict(NeuralNet_noSat, newdata = LC_ValidationData))
round((hit_rate_nn_noSat = 1 -  mean(NN_nosat_predictions != actual)), 4) * 100

### Lift Chart
lift.chart(modelList = c("LogisticModel_noSat", "TreeModel_noSat", "RFModel_noSat", "NeuralNet_noSat"),
           data = LC_ValidationData,
           targLevel = "Yes", trueResp = 0.265,
           type = "cumulative", sub = "Validation Set")

# Hit Rates
hit_rates_noSat <- c(hit_rate_noSat, hit_rate_tree_noSat, hit_rate_rf_noSat, hit_rate_nn_noSat)
names(hit_rates_noSat) <- c("Logistic", "Tree", "Random Forest", "Neural Network")
print(hit_rates_noSat)

# ----- Predicting Churn of New Customers without Satisfaction Information -----
# Set a new seed
set.seed(444)

NewCustomerData_noSat <- filter(ModelingData, New_Customer == "Yes") %>% 
  select(-Satisfaction_Level, -Satisfaction_Score, -New_Customer)

NewCustomerData_noSat <- training_validation_split(NewCustomerData_noSat)

NewCust_TrainingData <- getTrainingData(NewCustomerData_noSat) %>% 
  select(-Churn_Value, -Churn_Value_Factor)

NewCust_ValidationData <- getValidationData(NewCustomerData_noSat)
actual <- NewCust_ValidationData$Churn_Value
NewCust_ValidationData <- select(NewCust_ValidationData, -Churn_Value, -Churn_Value_Factor)

### Stepwise Logistic Regression Model
initial_NewCust <- glm(data = NewCust_TrainingData, 
                       formula = Churn_Label ~ 1, family = binomial(logit))

final_NewCust <- glm(data = NewCust_TrainingData, 
                     formula = Churn_Label ~ . , 
                     family = binomial(logit))

step_model_NewCust <- step(object = initial_NewCust, scope = list(upper = final_NewCust), 
                           k = log(nrow(NewCust_TrainingData)))
# Stepwise Model Predictor Selection:
# - Internet Type
# - Contract
# - Number_of_Referrals
# - Referred_a_Friend
# - Tenure_in_Months
# - Payment_Method
# - Dependents
# - Age
# - Streaming_Music
# - City_Population
# - LargeCity
# - Paperless_Billing

### Logistic Regression Model
LogisticModel_NewCust <- glm(Churn_Label ~ Internet_Type + Contract + Number_of_Referrals +  
                               Referred_a_Friend + Tenure_in_Months + Payment_Method + Dependents + 
                               Age + Streaming_Music + City_Population + LargeCity + 
                               Paperless_Billing, 
                             data = NewCust_TrainingData, family = binomial(logit))

round((hit_rate_NewCust = 1 - getMisclassificationRate(LogisticModel_NewCust, actual, newdata = NewCust_ValidationData)), 4) * 100

summary(LogisticModel_NewCust)

### Tree Model
TreeModel_NewCust <- rpart(Churn_Label ~ Internet_Type,
                           data = NewCust_TrainingData, cp = 0.31, model = TRUE)
TreeModel_NewCust
printcp(TreeModel_NewCust)
plotcp(TreeModel_NewCust, upper = "splits")
TreeModel_NewCust$cptable

rpart.plot(TreeModel_NewCust, type = 1, extra = 2,
           fallen.leaves = TRUE, uniform = FALSE, digits = 3, 
           yes.text = "true", no.text = "false", cex = 0.7)
# Only considers Internet Type

### Random Forest Model
RFModel_NewCust <- randomForest(Churn_Label ~ Age + Age_Range + 
                                  Number_of_Dependents + City_Population + 
                                  ZipCode_Population + Latitude + Longitude + 
                                  Number_of_Referrals + Tenure_in_Months + 
                                  Offer + 
                                  Avg_Monthly_Long_Distance_Charges + 
                                  Internet_Service + Internet_Type + 
                                  Avg_Monthly_GB_Download + Online_Security + 
                                  Premium_Tech_Support + Streaming_TV + 
                                  Streaming_Music + Unlimited_Data + 
                                  Contract + Paperless_Billing + Payment_Method + 
                                  Monthly_Charge + Total_Charges + 
                                  Total_Long_Distance_Charges + 
                                  Total_Revenue + CLTV, 
                                data = NewCust_TrainingData, importance = TRUE, ntree = 10000, mtry = 5)
importance(RFModel_NewCust, type = 2)
varImpPlot(RFModel_NewCust,type = 2, main = "Importance Plot")

predictions_rf_newCust <- unname(predict(RFModel_NewCust, newdata = NewCust_ValidationData))
predictions_rf_newCust <- ifelse(predictions_rf_newCust == "Yes", 1, 0)

round((hit_rate_rf_NewCust = 1 - mean(actual != predictions_rf_newCust)), 4) * 100

### Neural Net Model
NeuralNet_NewCust <- Nnet(Churn_Label ~ Age + Age_Range + 
                            Number_of_Dependents + City_Population + 
                            ZipCode_Population + Latitude + Longitude + 
                            Number_of_Referrals + Tenure_in_Months + 
                            Offer + 
                            Avg_Monthly_Long_Distance_Charges + 
                            Internet_Service + Internet_Type + 
                            Avg_Monthly_GB_Download + Online_Security + 
                            Premium_Tech_Support + Streaming_TV + 
                            Streaming_Music + Unlimited_Data + 
                            Contract + Paperless_Billing + Payment_Method + 
                            Monthly_Charge + Total_Charges + 
                            Total_Long_Distance_Charges + 
                            Total_Revenue + CLTV,
                          data = NewCust_TrainingData,
                          decay = 0.10,
                          size = 4)

### Lift Charts New Customer
lift.chart(modelList = c("LogisticModel_NewCust", "RFModel_NewCust"),
           data = LC_ValidationData,
           targLevel = "Yes", trueResp = 0.265,
           type = "cumulative", sub = "Validation Set")


lift.chart(modelList = c("LogisticModel_NewCust", "TreeModel_NewCust", "RFModel_NewCust", "NeuralNet_NewCust"),
           data = LC_ValidationData,
           targLevel = "Yes", trueResp = 0.265,
           type = "cumulative", sub = "Validation Set")

# The random forest model works the best for this case of predicting churn of new customers;
# however, we chose to use the logistic regression model as our final model as it is more interpretable for stakeholders.


# -------------------- Modelling Satisfaction as the Target --------------------
# We will now treat Satisfaction_Score as our target variable; we wish to detect
# what variables in our dataset are influencing customer satisfaction.

# Set a new random seed
set.seed(754) 

# This will be the data to model with, taking out variables not needed for the modelling process
SatisfactionDataset <- select(SingleTable, -Customer_ID, -Count, -Country, 
                              -State, -City, -Zip_Code, -Lat_Long, -Quarter, 
                              -Senior, -Age_Category, -Satisfaction_Level, 
                              -Customer_Status, -Churn_Label, -Churn_Value, 
                              -Churn_Value_Factor, -Churn_Category, 
                              -Churn_Reason)

# Randomly sample to get an equal number of satisfaction values for each score
sample1 <- filter(SatisfactionDataset, Satisfaction_Score == 1) %>% slice_sample(n = 518)
sample2 <- filter(SatisfactionDataset, Satisfaction_Score == 2) %>% slice_sample(n = 518)
sample3 <- filter(SatisfactionDataset, Satisfaction_Score == 3) %>% slice_sample(n = 518)
sample4 <- filter(SatisfactionDataset, Satisfaction_Score == 4) %>% slice_sample(n = 518)
sample5 <- filter(SatisfactionDataset, Satisfaction_Score == 5) %>% slice_sample(n = 518)
# Put the two samples into one dataframe and randomly shuffle the rows
SatisfactionDataset <- bind_rows(sample1, sample2, sample3, sample4, sample5) %>% slice_sample(prop = 1)

predictor_list_satisfaction_modelling <- colnames(SatisfactionDataset)
predictor_list_satisfaction_modelling <- predictor_list_satisfaction_modelling[!(predictor_list_satisfaction_modelling %in% c("Satisfaction_Score"))]

MSPE_matrix <- runall_satisfaction_models(SatisfactionDataset, predictor_list_satisfaction_modelling, trials = 10)
AvgMSPEs <- makeAverageMSPETibble(MSPE_matrix)
print(AvgMSPEs, n = Inf)
# The stepwise regression model performs the best, with Contract and Internet_Type
# as solo predictors not falling too far behind.

### Step Modelling
makeStepModel_satisfaction(SatisfactionDataset)
# Stepwise Modelling Predictor Selection:
# - Contract
# - Internet Type
# - Number of Referrals
# - Online_Security
# - Referred_a_Friend
# - New_Customer
# - Dependents
# - Payment_Method
# - Premium_Tech_Support
# - Age
# - City_Population
# - LargeCity

# Remove: 
# * Number_of_Referrals and Referred_a_Friend --> reverse causation
SatisfactionDataset <- select(SatisfactionDataset, -Number_of_Referrals, -Referred_a_Friend)

makeStepModel_satisfaction(SatisfactionDataset)

# Remove: 
# * Dependents Variables --> not a controllable variable for us
# * Population Variables --> not controllable and very small affect
# * Age --> not controllable + small affect
SatisfactionDataset <- select(SatisfactionDataset, -Dependents, -Number_of_Dependents, -ZipCode_Population, -City_Population, -LargeCity, -Age)

makeStepModel_satisfaction(SatisfactionDataset)
# Results:
# - Contract
# - Internet_Type
# - Online_Security
# - New Customer
# - Payment_Method
# - Premium_Tech_Support (Negligible Effect)
# - Avg_Monthly_GB_Download (Negligible Effect)

# We can influence customer satisfaction through improving our internet service,
# payment methods, and online security. Getting customers on longer contracts will
# also reduce our churn rates.

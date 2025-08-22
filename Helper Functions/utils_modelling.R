# File: utils_modelling.R
# Author: Hunter Moricz
# Date: July, 2024
# Description: Utility functions for building churn and satisfaction prediction 
# models. Includes training/validation splitting, logistic and linear regression
# helpers, stepwise model selection, and summary functions to evaluate 
# misclassification rates and prediction errors.
# ------------------------------------------------------------------------------
# -------------------------- Modelling Setup Functions -------------------------
# Training-Validation Split
training_validation_split <- function(data, seed = 100) {
  data$Set <- create.samples(data, est = 0.75, val = 0.25, rand.seed = seed)
  return(data)
}
# Get training data function
getTrainingData <- function(data) {
  training <- filter(data, Set == "Estimation")
  training <- select(training, -Set)
  return(training)
}
# Get validation data function
getValidationData <- function(data) {
  validation <- filter(data, Set == "Validation")
  validation <- select(validation, -Set)
  return(validation)
}
# --------------------------- Churn Modelling ----------------------------------
# Misclassification rate function -> for logistic regression models
getMisclassificationRate <- function(model, actual, newdata) {
  predictions <- round(predict(model, newdata = newdata, type = "response"))
  #print(table(actual, predictions))
  misclassification_rate <- mean(predictions != actual)
  return(misclassification_rate)
}

# Matrix of Model Misclassification rates
# Includes a model for each variable in the predictor list plus an additional spot for the step model
makeMatrix <- function(predictor_list, trials = 1) {
  misclass_matrix <- matrix(nrow = length(predictor_list) + 1, ncol = trials)
  trial_names <- trialnames(trials)
  colnames(misclass_matrix) <- trial_names
  rownames(misclass_matrix) <- c(predictor_list, "Step")
  return(misclass_matrix)
}

# Get trial names
trialnames <- function(trials) {
  trial_names <- c()
  for(i in seq(trials)) {
    trial_names <- c(trial_names, paste("Trial", i))
  }
  return(trial_names)
}

# Logistic Regression Model
run_model <- function(training_data, predictor) {
  model <- glm(formula = paste("Churn_Value_Factor ~", predictor), 
               data = training_data, family = binomial(logit))
}

# Function to run all logistic regression models for each predictor
runall_models <- function(data, predictor_list, trials) {
  # Set up matrix
  misclass_matrix <- makeMatrix(predictor_list, trials)
  # Loop for each trial
  for(trial in 1:trials) {
    # Set up
    data <- training_validation_split(data, sample(1:1000000, 1))
    training <- getTrainingData(data)
    validation <- getValidationData(data)
    data <- select(data, -Set)
    actual <- validation$Churn_Value_Factor
    
    # Make a model for each predictor
    for(predictor_index in seq(length(predictor_list))) {
      model <- run_model(training_data = training, predictor = predictor_list[predictor_index])
      misclassification_rate <- getMisclassificationRate(model, actual, newdata = validation)
      misclass_matrix[predictor_index, trial] <- misclassification_rate
    }
    
    # Include step model
    step_model <- makeStepModel(data)
    misclassification_rate_step <- getMisclassificationRate(step_model, actual, newdata = validation)
    misclass_matrix[length(predictor_list)+1, trial] <- misclassification_rate_step
  }
  return(misclass_matrix)
}

# Stepwise Model - Logistic Regression
makeStepModel <- function(data) {
  initial <- glm(data = data, formula = Churn_Value_Factor ~ 1, family = binomial(logit))
  final <- glm(data = data, 
               formula = Churn_Value_Factor ~ ., family = binomial(logit))
  step_model <- step(object = initial, scope = list(upper = final), k = log(nrow(data)))
  return(step_model)
}

# Get the mean missclassification rate of each model
getMeanMisclassificationRates <- function(misclassification_matrix) {
  mean_rates <- c()
  num_rows <- nrow(misclassification_matrix)
  # Loop through each column to get the average rates
  for(r in 1:num_rows) {
    meanrate <- mean(misclassification_matrix[r,])
    mean_rates <- c(mean_rates, meanrate)
  }
  return(mean_rates)
}

# Record all results into a tibble
makeAverageMisclassTibble <- function(misclassification_matrix) {
  mean_rates <- getMeanMisclassificationRates(misclassification_matrix)
  misMatTibble <- tibble(Model = rownames(misclassification_matrix), Avg_Misclass_Rate = mean_rates)
  misMatTibble <- arrange(misMatTibble, Avg_Misclass_Rate)
  return(misMatTibble)
}

# --------------------------- Satisfaction Modelling ---------------------------
# Mean Squared Prediction Error Function
MSPE <- function(model, actual, newdata) {
  predictions <- predict(model, newdata = newdata)
  diff = predictions - actual
  diff_sq = diff^2
  mspe <- mean(diff_sq)
  return(mspe)
  
}

# Linear Model
run_Linearmodel <- function(training_data, predictor) {
  model <- lm(formula = paste("Satisfaction_Score ~", predictor), 
              data = training_data, family = binomial(logit))
}

# Like `runall_models` above, but for satisfaction score linear models
runall_satisfaction_models <- function(data, predictor_list, trials) {
  # Set up matrix
  mspe_matrix <- makeMatrix(predictor_list, trials)
  # Loop for each trial
  for(trial in 1:trials) {
    # Set up
    data <- training_validation_split(data, sample(1:1000000, 1))
    training <- getTrainingData(data)
    validation <- getValidationData(data)
    data <- select(data, -Set)
    actual <- validation$Satisfaction_Score
    
    # Make a model for each predictor
    for(predictor_index in seq(length(predictor_list))) {
      model <- run_Linearmodel(training_data = training, predictor = predictor_list[predictor_index])
      mspe <- MSPE(model, actual, newdata = validation)
      mspe_matrix[predictor_index, trial] <- mspe
    }
    
    # Include step model
    step_model <- makeStepModel_satisfaction(data)
    mspe_step <- MSPE(step_model, actual, newdata = validation)
    mspe_matrix[length(predictor_list)+1, trial] <- mspe_step
  }
  return(mspe_matrix)
}

# Step model - linear regression
makeStepModel_satisfaction <- function(data) {
  initial <- lm(data = data, formula = Satisfaction_Score ~ 1)
  final <- lm(data = data, 
              formula = Satisfaction_Score ~ .)
  step_model <- step(object = initial, scope = list(upper = final), k = log(nrow(data)))
  return(step_model)
}

# Record satisfaction score MSPE results into a tibble
makeAverageMSPETibble <- function(mspe_matrix) {
  mean_rates <- getMeanMisclassificationRates(mspe_matrix)
  misMatTibble <- tibble(Model = rownames(mspe_matrix), Avg_MSPE = mean_rates)
  misMatTibble <- arrange(misMatTibble, Avg_MSPE)
  return(misMatTibble)
}
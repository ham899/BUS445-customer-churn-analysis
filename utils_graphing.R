# File: utils_graphing.R
# Author: Hunter Moricz
# Date: July, 2024
# Description: Utility functions for visualizing and modelling churn predictors
# with quick plots and simple logistic regression for categorical and 
# numeric variables.
# ------------------------------------------------------------------------------
# ------------------------- Graphing Functions ---------------------------------
# Predictor and Target need to be factors
graph_cat <- function(data, predictor, target) {
  # Bar chart of counts per group
  p1 <- ggplot(data, aes(x = !!sym(predictor))) +
    geom_bar() +
    theme_classic()
  print(p1)
  # Bar chart again but separated by target factor levels
  p2 <- ggplot(data, aes(!!sym(predictor), fill = !!sym(target))) +
    geom_bar(position = "dodge") +
    scale_fill_hue(direction = -1) +
    theme_classic()
  print(p2)
  # Bar chart of proportions regarding target factor levels
  p3 <- ggplot(data, aes(!!sym(predictor), fill = !!sym(target))) +
    geom_bar(position = "fill") +
    scale_fill_hue(direction = -1) +
    theme_classic()
  print(p3)
}
# Predictor needs to be numeric and target needs to be a numeric binary variable
graph_num <- function(data, predictor, target) {
  # Histogram
  p1 <- ggplot(data, aes(!!sym(predictor))) +
    geom_histogram() +
    theme_classic()
  print(p1)
  # Summary Stats
  print(summary(data[[predictor]]))
  # Boxplots
  p2 <- ggplot(data, aes(as.factor(!!sym(target)), !!sym(predictor))) +
    stat_boxplot(geom = "errorbar") + 
    geom_boxplot() +
    xlab(target) +
    theme_classic()
  print(p2)
  # Logistic Graph
  p3 <- ggplot(data, aes(!!sym(predictor), !!sym(target))) +
    geom_jitter() +
    stat_smooth(method="glm", 
                color="forestgreen", 
                se=FALSE, 
                method.args = list(family=binomial)) +
    theme_classic()
  print(p3)
}

# ----------------------------- Preliminary Modelling --------------------------
# For the following functions, 
# the predictor and target arguments need to be inputted as strings:

# Predictor needs to be categorical and target can be a binary numeric or factor
LogitModel_cat <- function(data, predictor, target) {
  model <- glm(formula = paste(target, "~", predictor), 
               data = data, family = binomial(logit))
  print(summary(model))
  print(Anova(model))
}
# Predictor needs to be numeric and target can be a binary numeric or factor
LogitModel_num <- function(data, predictor, target) {
  model <- glm(formula = paste(target, "~", predictor), 
               data = data, family = binomial(logit))
  print(summary(model))
}

# Predictor needs to be categorical and target needs to be a binary factor
explore_cat <- function(data, predictor, target) {
  # The graph_cat needs factors to work with
  graph_cat(data, predictor, target)
  LogitModel_cat(data, predictor, target)
}
# Predictor needs to be numeric and target needs to be a numeric binary
explore_num <- function(data, predictor, target) {
  # graph num needs a numeric predictor and a numeric binary variable
  graph_num(data, predictor, target)
  LogitModel_num(data, predictor, target)
}

# Create a function where you don't have to think about whether the predictor
# is categorical or numeric
explore_variable_churn <- function(data, predictor) {
  if(is.factor(data[[predictor]])) {
    explore_cat(data, predictor, "Churn_Value_Factor")
  }
  if(is.numeric(data[[predictor]])) {
    explore_num(data, predictor, "Churn_Value")
  }
}
# Create a function that will analyze all the desired predictors in a dataset regarding churn
# predictor_list should be a vector of the predictors as strings
analyze_dataset_churn <- function(data, predictor_list) {
  for(predictor in predictor_list) {
    explore_variable_churn(data, predictor)
    print("----------------------------------------------------------------------------------")
  }
}
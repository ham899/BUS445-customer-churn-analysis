# File: exploratory_data_analysis.R
# Author: Hunter Moricz
# Date: July, 2024
# Description: Performs exploratory data analysis (EDA) of the cleaned Telco 
# churn datasets. Summarizes churn patterns across demographics, location, 
# services, and status datasets. Produces descriptive statistics and 
# visualizations to guide the analysis, modelling, actionable insights, and 
# business recommendations.
# ------------------------------------------------------------------------------
# -------------------------------- Setup ---------------------------------------
# Load necessary packages
library(tidyverse)
library(car)

# Load the helper functions
source("Helper Functions Scripts/utils_graphing.R")

# Load the data by sourcing the wrangling file
source("data_preparation.R")

# ------------------------------- Functions ------------------------------------
# A function to evaluate the counts, proportions, and churn rates of demographic variables
demo_summary_churn <- function(cat) {
  print(paste0("Results for ", cat, ":"))
  demo_summary <- Demographics %>% group_by(!!sym(cat)) %>%
    summarize(Total_Count = n(), Proportion = Total_Count / nrow(Demographics))
  print(demo_summary)
  churn_summary <- filter(Demographics, Churn_Value == 1) %>% group_by(!!sym(cat)) %>%
    summarize(Churn_Count = n(), Proportion = Churn_Count / nrow(Demographics))
  print(churn_summary)
  combined_summary <- inner_join(select(demo_summary, -Proportion), select(churn_summary, -Proportion)) %>% 
           mutate(Churn_Rate = Churn_Count/Total_Count)
  print(combined_summary)
}

#------------------------- Demographics Dataset --------------------------------
# Use custom function to fully explore the dataset
analyze_dataset_churn(Demographics, colnames(Demographics)[3:10])

## Demographic Variable Analytics
# Demographic variables
demo_vars <- c("Gender", "Age_Category", "Senior", "Married", "Dependents")
# Print results for counts, proportions, and churn rates of the demographic variables
for (demo in demo_vars) {
  demo_summary_churn(demo)
}

# Summarize demographic variables by age category, married, and dependents
(Demo_summary_ageCat <- Demographics %>% group_by(Age_Category, Married, Dependents) %>%
    summarize(Total_Count = n()) %>% arrange(desc(Total_Count)))
(Demog_Churners_ageCat <- filter(Demographics, Churn_Value == 1) %>% group_by(Age_Category, Married, Dependents) %>%
  summarize(Churn_Count = n()) %>% arrange(desc(Churn_Count)))
inner_join(Demo_summary_ageCat, Demog_Churners_ageCat) %>% mutate(Churn_Rate = Churn_Count / Total_Count) %>% arrange(desc(Total_Count)) %>% arrange(desc(Churn_Rate))

# Summarize demographic variables by senior, married, and dependents
(Demo_summary_Seniors <- Demographics %>% group_by(Senior, Married, Dependents) %>%
    summarize(Total_Count = n()) %>% arrange(desc(Total_Count)))
(Demog_Churners_ageCat <- filter(Demographics, Churn_Value == 1) %>% group_by(Senior, Married, Dependents) %>%
  summarize(Churn_Count = n()) %>% arrange(desc(Churn_Count)))
inner_join(Demo_summary_Seniors, Demog_Churners_ageCat) %>% mutate(Churn_Rate = Churn_Count / Total_Count) %>% arrange(desc(Total_Count)) %>% arrange(desc(Churn_Rate))

## Notes based on preliminary analysis for the demographics dataset: 
# - Roughly even number of male and females who churn at similar rates
# - The older ages tend to churn at a slightly higher rate
# - Seniors churn at a higher rate
# - Unmarried people tend to churn at a slightly higher rate as well
# - Customers with dependents don't often churn
# - Seniors who are unmarried and without dependents have the highest churn rate

#------------------------------ Location Dataset -------------------------------
# Use custom function to fully explore the dataset
analyze_dataset_churn(Location, colnames(Location)[c(6, 7, 9, 11, 12)])

# Get counts of customers in each city and the proportions of our total customer base in each city
(City_Summary_Customers <- Location %>% group_by(City) %>% 
    summarize(Number_of_Customers = n(), Proportion_of_Customers = Number_of_Customers / nrow(Location)) %>%
    arrange(desc(Number_of_Customers)))
# Most customers are located in LA, San Diego, San Jose, Sacramento, and San Francisco

# Get counts of the number of customer who churned in each city and the proportion of churners in a city out of the total number of churners
(City_Summary_Churners <- filter(Location, Churn_Value == 1) %>% group_by(City) %>%
    summarise(Number_of_Churners = n(), Proportion_of_Total_Churners = Number_of_Churners / nrow(filter(Location, Churn_Value == 1))) %>% arrange(desc(Proportion_of_Total_Churners)))
# There were high rates of churn in San Diego and LA

# Summarize the churn rate values in city and sort by Churn_Rate
inner_join(City_Summary_Customers, City_Summary_Churners) %>% 
    select(City, Number_of_Customers, Number_of_Churners) %>%
    mutate(Churn_Rate = Number_of_Churners / Number_of_Customers) %>% 
    filter(Number_of_Customers > 10) %>%
    arrange(desc(Churn_Rate)) %>% head(10)
# San Diego, Fallbrook, and Temecula had the highest churn rates

# Same but sort by Number of Customers -> Our biggest customer bases by city and examine their churn rates
inner_join(City_Summary_Customers, City_Summary_Churners) %>% 
    select(City, Number_of_Customers, Number_of_Churners) %>%
    mutate(Churn_Rate = Number_of_Churners / Number_of_Customers) %>%
    arrange(desc(Number_of_Customers)) %>% head(5)
# San Diego is the most concerning city by size and churn rate

## MAPS
# Map where all our customers are located
ggplot(data = map_data("state", region = "california")) +
  geom_polygon(aes(x = long, y = lat), fill = "white", colour = "black") +
  geom_point(data = Location, mapping = aes(Longitude, Latitude, colour = "Customer"), alpha = 0.06) +
  scale_colour_manual(name = "Legend", breaks = c("Customer"), values = c("Customer" = "purple")) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.7), position = "inside")) +
  coord_fixed() +
  theme_void() +
  theme(legend.position.inside = c(0.8, 0.8)) +
  ggtitle(label = "Map of All Customers") +
  theme(plot.title = element_text(hjust = 0.5))

# Map where our customers churned
ggplot(data = map_data("state", region = "california")) +
  geom_polygon(aes(x = long, y = lat), fill = "white", colour = "black") +
  geom_point(data = filter(Location, Churn_Value == 1), mapping = aes(Longitude, Latitude, colour = "Churned Customer"), 
             alpha = 0.1) +
  scale_color_manual(name = "Legend", breaks = c("Churned Customer"), values = c("Churned Customer" = "red")) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.7), position = "inside")) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = c(0.8, 0.8)) +
  ggtitle(label = "Map of Customers who Churned") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot both churners and non-churners on the same map
ggplot(data = map_data("state", region = "california")) +
  geom_polygon(aes(x = long, y = lat), fill = "white", colour = "black") +
  geom_point(data = filter(Location, Churn_Value == 0), mapping = aes(x=Longitude, y=Latitude, colour = "Stayed Customer"), alpha = 0.2) +
  geom_point(data = filter(Location, Churn_Value == 1), mapping = aes(Longitude, Latitude, colour = "Churned Customer"), alpha = 0.15) +
  scale_color_manual(name = "Legend", breaks = c("Stayed Customer", "Churned Customer"), values = c("Stayed Customer" = "blue", "Churned Customer" = "red")) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.7), position = "inside")) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = c(0.8, 0.8)) +
  ggtitle(label = "Map of Both Customers who Stayed and Churned") +
  theme(plot.title = element_text(hjust = 0.5))

## Notes based on preliminary analysis for the location dataset:
# - City population has a slight effect where larger populations lead to more churn
# - Zip Code population also has a slight increase in churn rate
# - Latitude and Longitude do not have a significant effect on churn
# - Focus more on retaining customers in urban areas where more customers are located, San Diego in particular
# - Investigate why customers are leaving at high rates in places like San Diego and Fallbrook

#-------------------------- Services Dataset -----------------------------------
# Use custom function to fully explore the dataset
analyze_dataset_churn(Services, colnames(Services)[4:31])

## Notes based on preliminary analysis:
# - Friend referrals indicate less likely to churn
# - More referrals tends towards less churn
# - Longer Tenure customers are less likely to churn
# - New Customers are much more likely to churn
# - Offer E has the highest churn rate out of any other offer
# - Customers with our internet service are more likely to churn
# - Specifically, customers with Fiber Optic internet are significantly more likely to churn
# - Customers with online security are slightly less likely to churn
# - Customers with premium tech support are more likely to stay
# - Customers with our streaming services are slightly more likely to churn
# - Customers with unlimited data are more likely to churn
# - Customers with month-to-month contracts are difficult to retain
# - Paperless billing has a higher churn rate
# - Credit card payments have the lowest churn rate out of all the payment methods
# - Customers with higher monthly charges tend to churn more

#------------------------------- Status Dataset --------------------------------
# Use custom function to fully explore the dataset
analyze_dataset_churn(Status, colnames(Status)[c(4, 5, 10)])

## Counts of churned customers
Status %>% select(Customer_ID, Churn_Label) %>% group_by(Churn_Label) %>%
    summarize(Count = n(), Proportion = Count/nrow(Status))

## Categories of reasons for customers churning
# Bar chart of counts of customer reasons for churning
ggplot(Status, aes(Churn_Category)) + 
  geom_bar() +
  theme_classic()
# Most customers did not churn

# Same bar chart but only looking at those who did churn
ggplot(filter(Status, Churn_Category != "Did Not Churn"), aes(Churn_Category)) + 
  geom_bar() +
  theme_classic()
# The most common reasons customers churned relates to competitors

# Churn counts and proportions for each churn category
(Churn_Category_summary <- filter(Status, Churn_Category != "Did Not Churn") %>% group_by(Churn_Category) %>%
    summarize(Count = n(), Proportion = Count/nrow(filter(Status, Churn_Category != "Did Not Churn")))) %>% arrange(desc(Count))
# Almost half of the customers who churned gave the reason of it being because of competitors

## Pie Charts of churn categories
pie(c(0.4499732, 0.1680043, 0.1621188, 0.1128946, 0.1070091), c("45%", "17%" , "16%", "11%", "11%"),
    col = c("Blue", "Purple", "Red", "Green", "Grey"),
    main = "Customer Churn Reasons")
legend("topright", c("Competitor", "Attitude", "Dissatisfaction", "Price", "Other"), fill = c("Blue", "Purple", "Red", "Green", "Grey"))

ggplot(Churn_Category_summary, aes(x="", y=Proportion, fill = Churn_Category)) +
  geom_bar(stat = "identity", width = 1, color="grey") +
  coord_polar("y", start = 0) +
  theme_classic() +
  theme_void() +
  scale_fill_brewer(palette = "Set1") +
  geom_text(y = c(0.06, 0.17, 0.29, 0.63, 0.92), label = c("11.3%", "10.7%", "16.2%", "45.0%", "16.8%"), color = "white", size = 4) +
  ggtitle("Pie Chart of Churn Reasons") +
  theme(plot.title = element_text(hjust = 0.5))

# Removing other from the analysis as most of the other reasons are legitimate reasons
(Churn_Category_summary <- filter(Status, Churn_Category != "Did Not Churn", Churn_Category != "Other") %>% group_by(Churn_Category) %>%
    summarize(Count = n(), Proportion = Count/nrow(filter(Status, Churn_Category != "Did Not Churn", Churn_Category != "Other")))) %>% arrange(desc(Count))

pie(c(0.5038945, 0.1881366, 0.1815458, 0.1264230), c("Competitor", "Attitude" , "Dissatisfaction", "Price"), 
    col = c("Blue", "Purple", "Red", "Green"),
    main = "Customer Churn Reasons - without Other category")
# Over half of are customers were lost because of competitor reasons

## CUSTOMER TEXT OF REASONS FOR CHURNING
(Competitor_Reasons <- Status %>% filter(Churn_Category == "Competitor") %>% group_by(Churn_Reason) %>%
   count() %>% arrange(desc(n)))
# Most common = Better devices and/or offers by competitors

(Attitude_Reasons <- Status %>% filter(Churn_Category == "Attitude") %>% group_by(Churn_Reason) %>%
    count() %>% arrange(desc(n)))
# Most common = Bad Experience with a support person

(Dissatisfaction_Reasons <- Status %>% filter(Churn_Category == "Dissatisfaction") %>% group_by(Churn_Reason) %>%
    count() %>% arrange(desc(n)))
# Most common = Product, Network, and Service Dissatisfaction

(Price_Reasons <- Status %>% filter(Churn_Category == "Price") %>% group_by(Churn_Reason) %>%
    count() %>% arrange(desc(n)))
# Most common = High prices and/or extra charges

(Other_Reasons <- Status %>% filter(Churn_Category == "Other") %>% group_by(Churn_Reason) %>%
    count() %>% arrange(desc(n)))
# Most common = The other category is either legitimate, unhelpful, or unclear

(Table_of_Reasons <- Status %>% filter(Churn_Reason != "Did Not Churn") %>% group_by(Churn_Reason) %>%
    count() %>% arrange(desc(n)))
# Most common = Competitor had better devices and/or offers

# Counts and proportions of churned, joined, and stayed customers
(NewlyJoinedCustomers <- Status %>% group_by(Customer_Status) %>%
    summarise(Count = n(), Proportion = Count / nrow(Status)))
# 26.5% of customers churned in Q3 while only 6.45% just joined

## Notes based on preliminary analysis:
# - If satisfaction score is known, it is an excellent indicator of whether a customer will stay or churn
# - Higher CLTV (value) customers are slightly less likely to churn
# - A common theme of leaving was due to a competitor, likely having better devices or prices
# - Focus on increasing satisfaction of certain products; for example, internet service

# ------------------------- ADDITIONAL INFO AND STATS --------------------------
# Churn rate of new customers vs. long-term customers
SingleTable %>% group_by(New_Customer) %>% summarise(mean(Churn_Value))
# Churn rate of new customers = 47.4%

# Reasons for new customers vs. long-term customers churning
filter(SingleTable, Churn_Value == 1) %>% group_by(New_Customer, Churn_Reason) %>% 
  summarize(Count = n()) %>% arrange(desc(Count)) %>% print(n = Inf)
# Most common is the better devices and offers by competitors

# Table of most revenue generating customers
SingleTable %>% arrange(desc(Total_Revenue)) %>% 
  select(Gender, Age, Married, Dependents, City, City_Population, 
         Tenure_in_Months, New_Customer, Phone_Service, Internet_Type, 
         Online_Security, Contract, Payment_Method, Satisfaction_Score, 
         Total_Revenue) %>% head(20)
# Examine patterns for which types of customers are bringing in the most revenue

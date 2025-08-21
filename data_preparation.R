# File: data_preparation.R
# Author: Hunter Moricz
# Date: July, 2024
# Description: Prepares the Telco churn datasets for analysis and modelling by 
# standardizing the column names, converting variables into factors where 
# appropriate, imputing missing values, and creating engineered features. 
# Outputs cleaned versions of each dataset and consolidates them into a single 
# table ready for exploratory analysis and modelling.
# ------------------------------------------------------------------------------
# ------------------------------- Setup ----------------------------------------
# Load Packages and Functions
library(tidyverse)
source("Helper Functions Scripts/BCA_functions_source_file.R")

# ----------------------------- Defined Functions ------------------------------
# A function to add underscore to a dataframe's column names if there are spaces
underscore_spaces_colnames <- function(df) {
  names(df) <- str_replace_all(names(df), " ", "_")
}

# ------------------------------MAIN SCRIPT ------------------------------------
# Read in the files and save copies of the raw data
Demographics_original <- read_csv("Telco Customer Data/Telco_customer_churn_demographics.csv")
Location_original <- read_csv("Telco Customer Data/Telco_customer_churn_location.csv")
Population_original <- read_csv("Telco Customer Data/Telco_customer_churn_population.csv")
Services_original <- read_csv("Telco Customer Data/Telco_customer_churn_services.csv")
Status_original <- read_csv("Telco Customer Data/Telco_customer_churn_status.csv")

# Store data for manipulation in new tibbles
Demographics <- Demographics_original
Location <- Location_original
Population <- Population_original
Services <- Services_original
Status <- Status_original

### Rename Columns
# Replace any names with spaces to have underscores instead
names(Demographics) <- underscore_spaces_colnames(Demographics_original)
names(Location) <- underscore_spaces_colnames(Location_original)
names(Population) <- underscore_spaces_colnames(Population_original)
names(Services) <- underscore_spaces_colnames(Services_original)
names(Status) <- underscore_spaces_colnames(Status_original)
# Rename 'Population' in `Population` to 'ZipCode_Population'
Population <- rename(Population, ZipCode_Population = Population)

### Convert character variables into factors
Demographics <- mutate(Demographics, 
                       Gender = as.factor(Gender), 
                       Married = as.factor(Married), 
                       Dependents = as.factor(Dependents)
                       )
Services <- mutate(Services, 
                   Quarter = as.factor(Quarter), 
                   Referred_a_Friend = as.factor(Referred_a_Friend), 
                   Offer = as.factor(Offer), 
                   Phone_Service = as.factor(Phone_Service), 
                   Multiple_Lines = as.factor(Multiple_Lines), 
                   Internet_Service = as.factor(Internet_Service), 
                   Internet_Type = as.factor(Internet_Type), 
                   Online_Security = as.factor(Online_Security), 
                   Online_Backup = as.factor(Online_Backup), 
                   Device_Protection_Plan = as.factor(Device_Protection_Plan), 
                   Premium_Tech_Support = as.factor(Premium_Tech_Support), 
                   Streaming_TV = as.factor(Streaming_TV), 
                   Streaming_Movies = as.factor(Streaming_Movies), 
                   Streaming_Music = as.factor(Streaming_Music), 
                   Unlimited_Data = as.factor(Unlimited_Data), 
                   Contract = as.factor(Contract), 
                   Paperless_Billing = as.factor(Paperless_Billing), 
                   Payment_Method = as.factor(Payment_Method)
                   )
Status <- mutate(Status, 
                 Quarter = as.factor(Quarter), 
                 Customer_Status = as.factor(Customer_Status), 
                 Churn_Label = as.factor(Churn_Label), 
                 )

### Imputation
# The missing values (NAs) in the `Churn_Category` and `Churn_Reason` columns 
# are due to the customer not actually churning.
## Check whether this claim is true:
nrow(filter(filter(Status, Churn_Value == 1), is.na(Churn_Category))) == 0
nrow(filter(filter(Status, Churn_Value == 1), is.na(Churn_Reason))) == 0
# Since churned customers always provide a reason, we can impute the missing 
# values in `churn_Category` and `Churn_Reason` with the string "Did Not Churn"
Status <- mutate(Status, 
                 Churn_Category = as.factor(ifelse(is.na(Churn_Category), "Did Not Churn", Churn_Category)), 
                 Churn_Reason = ifelse(is.na(Churn_Reason), "Did Not Churn", Churn_Reason)
                 )

### Variable Creation --> Create new variables to analyze in the EDA script
# Binning the ages of customers into a categorical variable
Demographics <- mutate(Demographics, Age_Range = binVariable(Age, bins = 6, method = "intervals", labels = c("19-29", "30-39", "40-49", "50-59", "60-69", "70-80"))) %>%
  select(Customer_ID:Age, Age_Range, Married:Number_of_Dependents)
# Senior Variable
Demographics <- mutate(Demographics, 
                       Age_Category = as.factor(binVariable(Demographics$Age, 
                                                  bins = 3, 
                                                  method = c("intervals"), 
                                                  labels = c("19-39", "40-59", "60-80")
                                                  )), 
                       Senior = as.factor(ifelse(Age >= 65, 1, 0))
                       )
# Add City to Population dataset
Population <- Population %>% left_join(select(Location, Zip_Code, City), by = "Zip_Code") %>% 
  unique()
# Impute newly created missing values (NA values) due to the left join
# We did not have at least one customer in all of the zip codes, 
# so the city values are unknown for some of the zip codes, 
# and we must insert these values manually.
# Sources: UnitedStatesZipCodes.org & World Population Review.com
# Links: https://www.unitedstateszipcodes.org/ca/ & https://worldpopulationreview.com/zips/california
Population[19, "City"] = "Los Angeles"
Population[224, "City"] = "Panorama City"
Population[259, "City"] = "Guasti"
Population[424, "City"] = "Twentynine Palms"
Population[426, "City"] = "Vidal"
Population[435, "City"] = "Apple Valley"
Population[444, "City"] = "Blue Jay"
Population[445, "City"] = "Bryn Mawr"
Population[448, "City"] = "Cima"
Population[472, "City"] = "Newberry Springs"
Population[483, "City"] = "Tecopa"
Population[654, "City"] = "Delano"
Population[678, "City"] = "Maricopa"
Population[680, "City"] = "Onyx"
Population[693, "City"] = "Tipton"
Population[698, "City"] = "Corcoran"
Population[729, "City"] = "Harmony"
Population[738, "City"] = "Paso Robles"
Population[763, "City"] = "Inyokern"
Population[775, "City"] = "Lone Pine"
Population[783, "City"] = "Ridgecrest"
Population[786, "City"] = "Trona"
Population[819, "City"] = "Mendota"
Population[864, "City"] = "Big Sur"
Population[1096, "City"] = "Hollister"
Population[1159, "City"] = "Altaville"
Population[1246, "City"] = "Westley"
Population[1248, "City"] = "Yosemite National Park"
Population[1259, "City"] = "Camp Meeker"
Population[1264, "City"] = "Clearlake Park"
Population[1306, "City"] = "Stewarts Point"
Population[1307, "City"] = "Talmage"
Population[1317, "City"] = "Eureka"
Population[1328, "City"] = "Crescent City"
Population[1361, "City"] = "Whitethorn"
Population[1372, "City"] = "Coloma"
Population[1424, "City"] = "Robbins"
Population[1485, "City"] = "Sacramento"
Population[1496, "City"] = "Artois"
Population[1543, "City"] = "Rackerby"
Population[1584, "City"] = "Fort Jones"
Population[1592, "City"] = "Hayfork"
Population[1609, "City"] = "Montague"
Population[1644, "City"] = "Lake City"
Population[1671, "City"] = "Truckee"

# CITY POPULATION VARIABLE
# Summarize the populations of cities
City_Populations <- Population %>% group_by(City) %>% summarize(City_Population = sum(ZipCode_Population)) %>% arrange(desc(City_Population))
# Add a variable of the population of the cities to the population dataset
Population <- inner_join(Population, City_Populations)

# LARGE CITY VARIABLE
Population <- mutate(Population, LargeCity = as.factor(ifelse(City_Population >= 1500000, "Yes", "No")))

# Add Zip Code, City Population, and Large City Variables to Location dataset 
# for further analysis regarding customer observations
Location <- inner_join(Location, select(Population, Zip_Code, ZipCode_Population, City_Population, LargeCity)) %>% 
  select(Customer_ID:City, City_Population, LargeCity, Zip_Code, ZipCode_Population, Lat_Long:Longitude)

# NEW CUSTOMER VARIABLE
# Creating a factor variable to indicate whether a customer is new or not
Services <- Services %>% mutate(New_Customer = as.factor(ifelse(Tenure_in_Months <= 12, "Yes", "No"))) %>%
  select(Customer_ID:Tenure_in_Months, New_Customer, Offer:Total_Revenue)

# CHURN FACTOR VARIABLE
# Add a factor variable for churn value and keep churn value numeric
Status <- mutate(Status, Churn_Value_Factor = as.factor(Churn_Value)) %>% select(Customer_ID:Churn_Value, Churn_Value_Factor, CLTV:Churn_Reason)

# SATISFACTION LEVEL
# Bin the satisfaction variables into high, medium, and low satisfaction groups
Status <- mutate(Status, Satisfaction_Level = factor(ifelse(Satisfaction_Score < 3, "Low", ifelse(Satisfaction_Score == 3, "Medium", "High")), levels = c("Low", "Medium", "High"))) %>% 
  select(Customer_ID:Satisfaction_Score, Satisfaction_Level, Customer_Status:Churn_Reason)

### MAKE A COMPLETE SINGLE TABLE WITH ALL THE VARIABLES FOR ANALYSIS
SingleTable <- inner_join(Demographics, Location) %>% 
  inner_join(Services) %>% inner_join(Status) %>% 
  select(Customer_ID:Total_Revenue, CLTV, Satisfaction_Score:Churn_Value_Factor, Churn_Category:Churn_Reason)

### ADD THE TARGET VARIABLE TO EACH DATASET
Demographics <- inner_join(Demographics, select(Status, Customer_ID, Churn_Value, Churn_Value_Factor))
Location <- inner_join(Location, select(Status, Customer_ID, Churn_Value, Churn_Value_Factor))
Services <- inner_join(Services, select(Status, Customer_ID, Churn_Value, Churn_Value_Factor))

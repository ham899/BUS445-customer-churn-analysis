# Customer Churn Analytics
*Exploratory and predictive analytics of customer churn for a fictional telecom company using R.*

## Overview

This project was a group project completed in the Simon Fraser University course **BUS445: Customer Analytics**. I have anonymized all other group members’ contributions to respect their privacy. All uploaded scripts are my own work; however, the uploaded slides are primarily the work of another group member.

We were provided five datasets containing key information for the fictional Californian telecommunications company **CaliConnect**. We think our professor likely obtained these datasets from [Kaggle](https://www.kaggle.com/, but since we are not sure, the datasets are not provided in this repository. The datasets were as follows:
- **Demographics:** customer demographic information
- **Location:** customer locations
- **Population:** zip code populations  
- **Services:** the services the customers are registered for
- **Status:** customer churn status 

The goal was to use **R** to analyze CaliConnect’s churn patterns in Q3 and produce business recommendations.  
Our two guiding analytics questions were:  
1. What variables are most associated with customer churn?  
2. Can we predict the likelihood that a customer will churn?  

To answer these, we applied exploratory data analysis and predictive analytics.  
For a concise overview of our process and findings, see the uploaded **presentation slides**.

---

## Scripts

Below are the scripts uploaded to this repository:
- `data_preparation.R` – Prepares the data for analysis and modelling. This file is sourced by the other main scripts.  
- `exploratory_data_analysis.R` – Explores each dataset and their variables’ relationships with customer churn.  
- `models.R` – Evaluates the performance and results of many different models for variable effects and churn prediction performance.  
- **Helper Functions**
  - `BCA_functions_source_file.R` – A course-provided package to support the analytics we were learning in class. See [BCA package documentation](https://cran.r-project.org/web/packages/BCA/index.html) for more details.  
  - `utils_graphing.R` – Functions to aid in the EDA process, producing visualizations and early modelling.  
  - `utils_modelling.R` – Functions to aid in the modelling processes contained in `models.R`.
 
---

## Requirements

The following packages need to be installed to run the above scripts:

```r
install.packages(c("tidyverse","car","mapdata","rpart","rpart.plot","randomForest","nnet"))
```

- Place the five datasets in a folder named `Telco Customer Data`.
- Place helper scripts in a folder named `Helper Functions Scripts`.

---

## Results

### Exploratory Data Analysis
Key findings:
- **Overall churn:** approximately 26.5% in Q3.  
- **Competitors** were the leading churn reason due to having better devices or offers.  
- **Geography:** Urban centers like **San Diego** show both large customer bases and high churn rates.  
- **Internet type:** **Fiber Optic** customers churn more and report lower satisfaction than other types.  
- **Tenure:** **New customers** (<= 12 months) were substantially more likely to churn than long-tenured customers.  
- **Contracts:** **Month-to-month** contracts show higher churn than longer terms.  
- **Add-ons:** **Online security** and **premium tech support** correlate with lower churn, while **paperless billing** and **higher monthly charges** correlate with higher churn.

### Churn Prediction Results
We evaluated multiple models (logistic regression, decision trees, random forest, and neural networks).  
Our final choice was a **logistic regression model** due to its strong performance and interpretability for stakeholders.
- Accuracy for all customers: **83%**  
- Accuracy for new customers: **78%** 

---

## Insights and Recommendations

Based on our analysis, we identified three key pain points driving churn:  
- Dissatisfaction with **Fiber Optic Internet service quality** 
- Higher churn among customers on **month-to-month contracts**
- High rates of **switching to competitors**, especially in urban areas  

**Our recommendations were:**  
1. **Improve Fiber Optic Internet** by investing in infrastructure, faster customer service response times, and potentially partnering with a credible manufacturer.
2. **Incentivize longer contracts** with discounts, loyalty programs, and complimentary add-ons like online security, which is associated with retention.  
3. **Mitigate competitor switching in high-churn regions** through targeted localized marketing campaigns, competitive pricing, community engagement, and leveraging the predictive model to proactively identify and retain at-risk customers.

---

## Acknowledgements

- Data and project context were provided through **SFU BUS445: Customer Analytics** coursework.  
- The `BCA_functions_source_file.R` helper functions were course-provided; documentation can be found [here](https://cran.r-project.org/web/packages/BCA/index.html).  
- Slides are included for context. All other contributors’ identities have been anonymized.


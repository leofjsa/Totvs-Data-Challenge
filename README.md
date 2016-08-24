# Read me  
*This file explains what was done to accomplish the 'Totvs Data Challenge'*  

Content:  
* Basic steps adopted in this data challenge  
* Available files in this repository  
  
## 1) BASIC STESP OF THIS DATA CHALLENGE:  
  
To accomplish this data challenge, some pre-processing was made and then 2 main analysis to answer the final questions, as explained below. More details are commented in the R script file.  
  
### 1.a) Pre-processing:  
1. Conversion from JSON format to structured tabular data  
2. Variables and data types identification  
3. Exclusion of variables 'without content' (the same value for every register)  
4. Data transformation (ex: from categorial variable to dummy variables)  
5. Outliers identificaion  
6. Exploratory analysis with data visualization  
  
  
### 1.b) Analysis 1 (fields that can help predict how much a customer will spend):  
* **ADOPTED MODEL:** linear regression, with stepwise method to select the independent variables. A training set was defined with random 70% of the original sample (30% for test), without resampling. Outliers were removed.   
* **CONCLUSIONS:**  
   - Variables derived from the field 'ide.dhEmi' (timestamp) can help predict customer spending (statistically significant)  
   - However, these variables by themselves are not enough. For a good prediction, other variables (not available in the sample) are necessary  
   - Other variables were tested, but not selected in the final model (due either to low prediction capacity or covariation)  
* **ASSUMPTION ON ORDER DETAILS (field 'dets' in original file):** with this information, it would be possible to precisely calculate customers spending. However, since this information would never be previously available for a prediction input, it was not considered in this analysis (for data extraction and parsing evaluation, a structured table with order details is available in the Github repository)  
  
  
### 1.c) Analysis 2 (sales forecast for the next week):  
* **ADOPTED MODEL:** since this is a very short time series (less than 3 weeks) with relevant seasonality, no sophisticated model could be applied. So a simple daily average per meal was made (eg: monday lunch) and then the averages were added up to a week total. The daily average per meal result is available in the Github repository (file 'Forecast - daily average per meal.txt'). Outliers were not removed, since they might occure again in the next week  
* **CONCLUSIONS:** the sales forecast for next week is **$34753**  
* **ASSUMPTION:** the restaurant will be open for dinner on thursday (it wasn't open in the first 2 weeks, but it was open in the 3rd week)  
  
  
## AVAILABLE FILES:  
* **R script.R:** script in R-language used in this data challenge  
* **OrderSummary.txt:** the input data used in the analysis. It contains one structured table that was made from de original raw data ('sample.txt') and some pre-processing. In this file, each register represents one transaction ('nota fiscal eletrônica'), without the order details  
* **OrderDetails.txt:** the resulting table from one specific field in the original raw data that represents the order details (product, quantity, etc.). This file was not used in the analysis (as explained above). It was generated only for evaluation purposes in this data chalenge  
* **Customer spending model.txt:** a summary of the linear regression model used  in the 1st analysis (fields that can help predict how much a customer will spend)  
* **Forecast - daily average per meal.txt:** summary of total sales per day of week and meal (lunch vs dinner), used in the 2nd analysis (sales forecast for the next week)  

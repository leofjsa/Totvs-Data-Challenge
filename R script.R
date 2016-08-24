######################################################################################################
## INITIALIZATIONS

# Libraries
library(jsonlite)
library(ggplot2)
library(dplyr)

# Folders and files
setwd('C:/Users/LeonardoF/Dropbox/Eureka/Testes_E_Desenvolvimentos/NF Eletronica Totvs')
cFileInput <- 'sample.txt'
cFileOutput_OrderDetails <- 'OrderDetails.txt'
cFileOutput_OrderSummary <- 'OrderSummary.txt'
cFileOutput_CustomerSpendingModel <- 'Customer spending model.txt'
cFileOutput_DailyAverage <- 'Forecast - daily average per meal.txt'

# Auxiliary function: outlier filter
# (this function returns a boolean vector identifying outliers (FALSE) and non-outliers (TRUE) values)
fFilterOutliers <- function(pData){
      Q1Q3_Limits <- quantile(pData, probs=c(.25, .75))     # Q1 and Q3 limits
      IQD <- Q1Q3_Limits[2] - Q1Q3_Limits[1]                # Interquartile distance
      vFilter <- pData >= Q1Q3_Limits[1] - 1.5 * IQD &      # Non-outlier limits
            pData <= Q1Q3_Limits[2] + 1.5 * IQD
      return(vFilter)
}



######################################################################################################
## PARSE AND EXTRACT THE DATA

# Read data from external JSON file
txtData <- readLines(cFileInput)

# Convert JSON to data-frame
sourceData <- fromJSON(txt = txtData, 
                       simplifyVector = FALSE,
                       simplifyDataFrame = TRUE, 
                       simplifyMatrix = TRUE,
                       flatten = TRUE)

# Overview: columns and data types
for (i in 1:ncol(sourceData)){
      print(paste(i, ') ', 
                  colnames(sourceData)[i], ': ',
                  class(sourceData[, i]), 
                  sep = ''))
}


# Generate data-frame with order details (variable 'dets')
dfOrderDetails <- data.frame(Seq = 1, sourceData$dets[[1]])
for (i in 2:length(sourceData$dets)) {
      dfOrderDetails <- rbind(dfOrderDetails, 
                              data.frame(Seq = i, sourceData$dets[[i]]))
}

# Save file with order details (variable 'dets')
write.table(x = dfOrderDetails, 
            file = cFileOutput_OrderDetails, 
            append = FALSE,
            quote = FALSE,
            sep = '\t', 
            dec = '.', 
            row.names = FALSE,
            col.names = TRUE)


# PARTIAL CONCLUSIONS: data types metadata (hand-made due to low quantity of columns)
vLists <- c(1:2)              # Columns of class 'List': columns 1 and 2
vCharacters <- c(4:14)        # Columns of class 'Character': 4 to 14
vNumeric <- c(3, 15:30)       # Columns of class 'Numeric': 3 and 15 to 30



######################################################################################################
## DATA EXPLORATION (identification of available variables)

# Exploration 1: understanding list variables
## Field 'dets': order details (a data-frame was generated above)
## Field 'versaoDocumento': always equal to 1
for (i in 1:length(sourceData$versaoDocumento)){
      if (sourceData$versaoDocumento[[i]] != 1) {
            print(i)
      }
}


# Exploration 2: Overview of different values per character variable
for (i in 1:length(vCharacters)){
      j <- j <- vCharacters[i]
      print(paste(j, colnames(sourceData)[j]))
      print(head(table(sourceData[, j])))
}


# Exploration 3:Overview of numeric variables range
summary(sourceData[, vNumeric])


# PARTIAL CONCLUSIONS FROM DATA EXPLORATION: 

## 1.a) Field 'dets' (list) contains order details
## 1.b) Field 'versaoDocumento' (list) is always equal to 1 (and, therefore, will not be used)

## 2.a) Field 'ide.dhEmi' (character) contains a timestamp, but it needs to be reformated
## 2.b) Field 'infAdic.infCpl' (character) contains the table number
## 2.c) The remaining character fields contains only one unique value (and, therefore, will not be used)

## 3.a) Field 'complemento.valorTotal' (numeric) contains customer spending (dependent variable)
## 3.b) The remaining numeric fields contain only 'zeros' or irrelevant data different from zero

## Variables to use in next steps: 
## X1. 'ide.dhEmi' (timestamp)
## X2. 'infAdic.infCpl' (table number)
## Y. 'complemento.valorTotal' (customer spending)

## Order details will not be used, since it wouldn't be available in any prediction scenario
## (it would make no sense to try to PREDICT customer spending or if order details are available)



######################################################################################################
## DATA TRANSFORMATION

# Timestamps (vector)
vTimestamp <- as.POSIXlt(gsub(pattern = 'T', replacement = ' ', 
                              x = gsub(pattern = '.000Z', replacement = '', 
                                       x = sourceData$ide.dhEmi)), 
                         tz = 'GMT')
min(vTimestamp)
max(vTimestamp)


# Day of week (ordered factor vector)
aux1 <- c(1:7)
aux2 <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
aux3 <- vTimestamp$wday
vDayOfWeek <- factor(aux2[aux3], levels = aux2)


# Day of week (data-frame of dummy variables)
dfDayOfWeek <- matrix(nrow = length(vDayOfWeek), ncol = length(aux2), dimnames = list(c(1:length(vDayOfWeek)), aux2))
for (i in 1:length(aux2)){
      dfDayOfWeek[, i] <- ifelse(vDayOfWeek == colnames(dfDayOfWeek)[i], 1, 0)
}


# Meal (lunch vs. dinner, vector)
hist(vTimestamp$hour)   # There's a clear visual separation for 2 time periods, that will be called 'lunch' and 'dinner'
vMealFactor <- factor(ifelse(vTimestamp$hour <= 16, 'Lunch', 'Dinner'), c('Lunch', 'Dinner'))


# Table number (2 vectors: numeric + factor numerically ordered)
vTableNumberNumeric <- as.numeric(as.numeric(substr(sourceData$infAdic.infCpl, 6, nchar(sourceData$infAdic.infCpl))))
vTableNumberFactor <- as.factor(as.numeric(substr(sourceData$infAdic.infCpl, 6, nchar(sourceData$infAdic.infCpl))))


# Outlier filter for 'complemento.valorTotal' (customer spending)
boxplot(sourceData$complemento.valorTotal)
vFilterOutlier <- fFilterOutliers(sourceData$complemento.valorTotal)
boxplot(sourceData$complemento.valorTotal[vFilterOutlier])


# Data-frame with transformed data, ready for analysis (including outliers)
dfDataForAnalysis1 <- data.frame(MesaFator = vTableNumberFactor,
                                 MesaNum = vTableNumberNumeric,
                                 DataCompleta = vTimestamp,
                                 DiaSemana = vDayOfWeek, 
                                 HoraCheia = vTimestamp$hour, 
                                 MealTXT = vMealFactor, 
                                 MealBIN = unclass(vMealFactor) - 1,
                                 ValorTotal = sourceData$complemento.valorTotal)
write.table(x = dfDataForAnalysis1, 
            file = cFileOutput_OrderSummary, 
            append = FALSE,
            quote = FALSE,
            sep = '\t', 
            dec = '.', 
            row.names = FALSE,
            col.names = TRUE)

# Data-frame with transformed data, ready for analysis (excluding outliers)
dfDataForAnalysis2 <- dfDataForAnalysis1[vFilterOutlier, ]



######################################################################################################
## FIELDS THAT CAN HELP PREDICT HOW MUCH A CUSTOMER WILL SPEND
## PART 1: VISUAL ANALYSIS for conceptual variable selection
## (in part 2, a machine learning method is applied for final variable selection)


# DAY OF WEEK visual test

## Bar-chart
dfDataSummary <- group_by(dfDataForAnalysis2, DiaSemana)
dfDataSummary <- summarise(dfDataSummary, TicketMedio = mean(ValorTotal))
myPlot <- ggplot(dfDataSummary, aes(DiaSemana, TicketMedio))
myPlot <- myPlot + geom_bar(stat = 'identity')
print(myPlot)

## Boxplot
myPlot <- ggplot(dfDataForAnalysis2, aes(DiaSemana, ValorTotal))
myPlot <- myPlot + geom_boxplot()
print(myPlot)


# MEAL (lunch vs. dinner) visual test

## Bar-chart
dfDataSummary <- group_by(dfDataForAnalysis2, MealTXT)
dfDataSummary <- summarise(dfDataSummary, TicketMedio = mean(ValorTotal))
myPlot <- ggplot(dfDataSummary, aes(MealTXT, TicketMedio))
myPlot <- myPlot + geom_bar(stat = 'identity')
print(myPlot)

## Boxplot
myPlot <- ggplot(dfDataForAnalysis2, aes(MealTXT, ValorTotal))
myPlot <- myPlot + geom_boxplot()
print(myPlot)


# TIME OF DAY visual test 

## Line-chart
dfDataSummary <- group_by(dfDataForAnalysis2, HoraCheia)
dfDataSummary <- summarise(dfDataSummary, TicketMedio = mean(ValorTotal))
myPlot <- ggplot(dfDataSummary, aes(HoraCheia, TicketMedio))
myPlot <- myPlot + geom_line(stat = 'identity')
print(myPlot)


# TABLE NUMBER visual test

## Bar-chart
dfDataSummary <- group_by(dfDataForAnalysis2, MesaFator)
dfDataSummary <- summarise(dfDataSummary, TicketMedio = mean(ValorTotal))
myPlot <- ggplot(dfDataSummary, aes(MesaFator, TicketMedio))
myPlot <- myPlot + geom_bar(stat = 'identity')
print(myPlot)

## Boxplot
myPlot <- ggplot(dfDataForAnalysis2, aes(MesaFator, ValorTotal))
myPlot <- myPlot + geom_boxplot()
print(myPlot)


# PARTIAL CONCLUSIONS: variables to use as input in part 2 (machine learning method)
## Day of week:   use
## Meal:          use
## Time of day:   use
## Table number:  do not use (although there is some variation, there is no trend in data and no 'business explanation')



######################################################################################################
## FIELDS THAT CAN HELP PREDICT HOW MUCH A CUSTOMER WILL SPEND
## PART 2: MACHINE LEARNING for final model


# Data-frame for modeling (output and selected input transformed variables only)
dfDataForModel <- cbind(data.frame(ValorTotal = dfDataForAnalysis2$ValorTotal, 
                                   Meal = dfDataForAnalysis2$MealTXT,
                                   HoraCheia = dfDataForAnalysis2$HoraCheia), 
                        dfDayOfWeek[vFilterOutlier, ])

# Split sample into training and testing datasets
set.seed(12345)
vTrain <- createDataPartition(y = dfDataForModel$ValorTotal, p = 0.7, list = FALSE)
dfTrain <- dfDataForModel[vTrain, ]
dfTest <- dfDataForModel[-vTrain, ]

# Linear regression: training (using stepwise to variable selection) and testing
modelFit <- step(lm(ValorTotal ~ ., data = dfTrain), direction = 'both')
modelPredict <- predict(modelFit, newdata = dfTest)

# Results visual evaluation
dfModelResults <- data.frame(Real = dfTest$ValorTotal, Proj = modelPredict)
qplot(x = Real, y = Proj, data = dfModelResults)

# Results analytical evaluation
summary(modelFit)
cor(modelPredict, dfTest$ValorTotal)

# CONCLUSIONS:

## Selected variables that can help predict customer spending: 'DAY OF WEEK' and 'TIME OF DAY'
## (both implicit in field 'ide.dhEmi' in the original JSON file)
## The variable selection can be verified in the model output (summary(modelFit)), where P-value < 0.05

## Not all input variables were used, due to high correlation among them

## Although the variables above can HELP PREDICT, they are no good predictors by themselves
## This can be verified by the relatively low correlation between real and projected values in the test dataset
## (it can also be visually verified by the high dispersion in the 'Real x Proj' plot)
## This result was expected after verification of high dispersion in visual tests



######################################################################################################
## SALES FORECAST FOR NEXT WEEK


# Data-frame for forecast (daily averages per meal, including outliers)
dfDataForForecast <- group_by(dfDataForAnalysis1, DiaSemana, MealTXT)
dfDataForForecast <- summarise(dfDataForForecast,
                               TotalSales = sum(ValorTotal), 
                               QtdDays = n_distinct(as.Date(DataCompleta)), 
                               SalesPerDay = TotalSales / QtdDays, 
                               QtdOrders = n())
print(dfDataForForecast)
write.table(x = dfDataForForecast, 
            file = cFileOutput_DailyAverage, 
            append = FALSE,
            quote = FALSE,
            sep = '\t', 
            dec = '.', 
            row.names = FALSE,
            col.names = TRUE)


# Forecast: sum of daily averages per meal
NextWeekForecast <- sum(dfDataForForecast$SalesPerDay)
print(paste('Forecast for next week:', NextWeekForecast))


######################################################################################################
## RELEASE MEMORY FROM NO LONGER NECESSARY VARIABLES

rm(aux1, aux2, aux3, i, j)
rm(vLists, vCharacters, vNumeric)
rm(txtData, cFileInput, cFileOutput_OrderDetails, cFileOutput_OrderSummary, 
   cFileOutput_CustomerSpendingModel, cFileOutput_DailyAverage)
rm(dfDayOfWeek, vDayOfWeek, vMealFactor, vTableNumberFactor, 
   vTableNumberNumeric, vFilterOutlier, vTimestamp)
rm(vTrain, dfTrain, dfTest)
rm(dfDataSummary, myPlot)
rm(NextWeekForecast)
rm(dfDataForAnalysis1, dfDataForAnalysis2, sourceData, dfOrderDetails)
rm(dfDataForModel, dfDataForForecast, modelFit, modelPredict, dfModelResults)

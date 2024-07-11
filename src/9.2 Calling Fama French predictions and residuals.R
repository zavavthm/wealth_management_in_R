#########################################################
#
# Getting predictions and residuals for Fama French models
#
#########################################################

#first we have to source the previous code with the FF model UDFs that give us predicted values
source("C:/Users/manik/Downloads/MBA/4th Sem (Jun - Jul)/Wealth Management in R/8.2 Fama French predictions and residuals - source this file before running the next one.R")

#we get 2 UDFs in our environment - each function has 3 inputs, ticker, from date and to date
#each function outputs a data frame with residuals, actuals and predictions

#########################################################
#
# Using Fama French 3 Factor model
#
#########################################################

#calling the Fama French 3F model UDF for WFC
WFC_FF3F <- fama_french_3F_pred_res(ticker="WFC", from_date='2020-01-02', to_date='2024-07-01')
WFC_FF3F$actuals
WFC_FF3F$model_pred

#calling the Fama French 3F model UDF for SPY
SPY_FF3F <- fama_french_3F_pred_res(ticker="SPY", from_date='2020-01-02', to_date='2024-07-01')
SPY_FF3F$actuals
SPY_FF3F$model_pred

#calling the Fama French 3F model UDF for TSLA
TSLA_FF3F <- fama_french_3F_pred_res(ticker="TSLA", from_date='2010-01-31', to_date='2021-02-20')
TSLA_FF3F$actuals
TSLA_FF3F$model_pred

#########################################################
#
# Using Fama French 5 Factor model  
#
#########################################################

#calling the Fama French 3F model UDF for TSLA
TSLA_FF5F <- fama_french_5F_pred_res(ticker="TSLA", from_date='2010-01-31', to_date='2021-02-20')
TSLA_FF5F$actuals
TSLA_FF5F$model_pred

#calling the Fama French 3F model UDF for WFC
WFC_FF5F <- fama_french_5F_pred_res(ticker="WFC", from_date='2020-01-02', to_date='2024-07-01')
WFC_FF5F$actuals
WFC_FF5F$model_pred

#calling the Fama French 3F model UDF for SPY
SPY_FF5F <- fama_french_5F_pred_res(ticker="SPY", from_date='2010-01-31', to_date='2021-02-20')
SPY_FF5F$actuals
SPY_FF5F$model_pred
####################################
# Investment Risk Optimization with R
#
# Created by Manik Malhotra (mmalhotra@student.hult.edu)
#
# HULT International Business School Summer-2 Elective
####################################

# if using log to the base 10 instead of natural log, then have to mention it in the code and in the report in footer

# install package quantmod - designed by Jeffery A. Ryan (german mathematician & Finance guy)
# quantmod - standard for all things investments for statistical analysis

#install.packages("quantmod")
library(quantmod)

# Step 1: Get data from yahoo api
getSymbols("WFC", src="yahoo")
getSymbols("AAPL", src="yahoo")
# it will create a time series object (xts) of the stock selected

# reloading data using generic names (stocks)
stock1 <- getSymbols("WFC", auto.assign = FALSE)
stock2 <- getSymbols("MSFT", auto.assign = FALSE)

# getting ETFs 
etf1 <- getSymbols("SPY", auto.assign = FALSE)

# fixed income
fixed_income1 <- getSymbols("AGG", auto.assign = FALSE)

# commodity
commodity1 <- getSymbols("GLD", auto.assign = FALSE)

# Step 2: joining the 5 investment options together
joined_prices <- merge.xts(stock1, stock2, etf1, fixed_income1, commodity1)

# Step 3: Just keeping the adjusted prices(6th, 12th, 18th, 24th, 30th columns) instead of all the columns
joined_adjusted_prices <- joined_prices[ , c(6,12,18,24,30)]

# better way to do:
joined_adjusted_prices <- joined_prices[ , seq(from=6, to=ncol(joined_prices), by=6)]

# Step 4: calculating returns on prices
# install.packages("dplyr")
library(dplyr)

# by default lag function takes the previous record. i.e. n <- 1
joined_returns <- as.data.frame(joined_adjusted_prices) %>% 
  mutate(WFC_ROR = log(WFC.Adjusted/lag(WFC.Adjusted))) %>%
  mutate(MSFT_ROR = log(MSFT.Adjusted/lag(MSFT.Adjusted))) %>%
  mutate(SPY_ROR = log(SPY.Adjusted/lag(SPY.Adjusted))) %>%
  mutate(AGG_ROR = log(AGG.Adjusted/lag(AGG.Adjusted))) %>%
  mutate(GLD_ROR = log(GLD.Adjusted/lag(GLD.Adjusted)))

# Step 5: changing the time window of stock RORs

n <- 250 #(250 because it's the approx number of working days in a year)

joined_returns_n <- as.data.frame(joined_adjusted_prices) %>% 
  mutate(WFC_ROR = log(WFC.Adjusted/lag(WFC.Adjusted, n))) %>%
  mutate(MSFT_ROR = log(MSFT.Adjusted/lag(MSFT.Adjusted, n))) %>%
  mutate(SPY_ROR = log(SPY.Adjusted/lag(SPY.Adjusted, n))) %>%
  mutate(AGG_ROR = log(AGG.Adjusted/lag(AGG.Adjusted, n))) %>%
  mutate(GLD_ROR = log(GLD.Adjusted/lag(GLD.Adjusted, n)))

# using dailyReturn function to get the daily returns of the stocks

stock1_returns <- dailyReturn(getSymbols(Symbols = "WFC", auto.assign = F))
stock2_returns <- dailyReturn(getSymbols(Symbols = "MSFT", auto.assign = F))
etf1_returns <- dailyReturn(getSymbols(Symbols = "SPY", auto.assign = F))
commodity1_returns <- dailyReturn(getSymbols(Symbols = "GLD", auto.assign = F))
fixed_income1_returns <- dailyReturn(getSymbols(Symbols = "AGG", auto.assign = F))

joined_returns_daily <- merge.xts(stock1_returns, stock2_returns, etf1_returns, fixed_income1_returns, commodity1_returns)

# using monthlyReturn function to get the daily returns of the stocks

stock1_m_returns <- monthlyReturn(getSymbols(Symbols = "WFC", auto.assign = F))
stock2_m_returns <- monthlyReturn(getSymbols(Symbols = "MSFT", auto.assign = F))
etf1_m_returns <- monthlyReturn(getSymbols(Symbols = "SPY", auto.assign = F))
commodity1_m_returns <- monthlyReturn(getSymbols(Symbols = "GLD", auto.assign = F))
fixed_income1_m_returns <- monthlyReturn(getSymbols(Symbols = "AGG", auto.assign = F))

joined_returns_monthly <- merge.xts(stock1_m_returns, stock2_m_returns, etf1_m_returns, commodity1_m_returns, fixed_income1_m_returns)

# adding a benchmark -- RUSSEL 1000 - VONE

benchmark_returns <- monthlyReturn(getSymbols("VONE", auto.assign = F))

joined_returns_monthly <- merge.xts(joined_returns_monthly, benchmark_returns)

## plotting histograms for WSF and SPY
#hist(joined_returns_monthly$monthly.returns)
#hist(joined_returns_monthly$monthly.returns.2)

# calculate sigma for last 12 months
time_index <- nrow(joined_returns_monthly)

# monthly standard deviation
sd(joined_returns_monthly$monthly.returns[time_index:(time_index-11)])

# we multiply it by sqrt of 12 to get the annualized standardized deviation
wfc_sigma <- sd(joined_returns_monthly$monthly.returns[time_index:(time_index-11)])*sqrt(12)
msft_sigma <- sd(joined_returns_monthly$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)
spy_sigma <- sd(joined_returns_monthly$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)
gld_sigma <- sd(joined_returns_monthly$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)
agg_sigma <- sd(joined_returns_monthly$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)
vone_sigma <- sd(joined_returns_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)


## Tracking error for last 12 months
wfc_te <- sd(joined_returns_monthly$monthly.returns[time_index:(time_index-11)]-joined_returns_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)

msft_te <- sd(joined_returns_monthly$monthly.returns.1[time_index:(time_index-11)]-joined_returns_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)

spy_te <- sd(joined_returns_monthly$monthly.returns.2[time_index:(time_index-11)]-joined_returns_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)

gld_te <- sd(joined_returns_monthly$monthly.returns.3[time_index:(time_index-11)]-joined_returns_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)

agg_te <- sd(joined_returns_monthly$monthly.returns.4[time_index:(time_index-11)]-joined_returns_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)

## Sharpe Ratio
riskfree <- 0.0001

# calculating stat expectation using mean() function
wfc_expected <- mean(joined_returns_monthly$monthly.returns[time_index:(time_index-11)])
msft_expected <- mean(joined_returns_monthly$monthly.returns.1[time_index:(time_index-11)])
spy_expected <- mean(joined_returns_monthly$monthly.returns.2[time_index:(time_index-11)])
gld_expected <- mean(joined_returns_monthly$monthly.returns.3[time_index:(time_index-11)])
agg_expected <- mean(joined_returns_monthly$monthly.returns.4[time_index:(time_index-11)])
vone_expected <- mean(joined_returns_monthly$monthly.returns.5[time_index:(time_index-11)])

sharpe_ratio_wfc <- (((1+wfc_expected)^12)-1-riskfree)/wfc_sigma
sharpe_ratio_msft <- (((1+msft_expected)^12)-1-riskfree)/msft_sigma
sharpe_ratio_spy <- (((1+spy_expected)^12)-1-riskfree)/spy_sigma
sharpe_ratio_gld <- (((1+gld_expected)^12)-1-riskfree)/gld_sigma
sharpe_ratio_agg <- (((1+agg_expected)^12)-1-riskfree)/agg_sigma
sharpe_ratio_vone <- (((1+vone_expected)^12)-1-riskfree)/vone_sigma

sharpe_ratio_wfc
sharpe_ratio_msft
sharpe_ratio_spy
sharpe_ratio_gld
sharpe_ratio_agg
sharpe_ratio_vone

## creating portfolio returns
wfc_weight <- 0.2
msft_weight <- 0.1
spy_weight <- 0.1
gld_weight <- 0.2
agg_weight <- 0.4
# these should always add upto 100%

# calculating monthly returns with weightage per stock
joined_returns_monthly <- as.data.frame(joined_returns_monthly) %>%
  mutate(portfolio = wfc_weight * monthly.returns + 
           msft_weight * monthly.returns.1 + 
           spy_weight * monthly.returns.2 + 
           gld_weight * monthly.returns.3 + 
           agg_weight * monthly.returns.4)

port_sigma <- sd(joined_returns_monthly$portfolio[time_index:(time_index-11)])*sqrt(12)

port_expected <- mean(joined_returns_monthly$portfolio[time_index:(time_index-11)])

sharpe_ratio_port <- (((1+port_expected)^12)-1-riskfree)/port_sigma

## calculating correlation
cor(joined_returns_monthly[time_index:(time_index-11), ])

## creating quasi capm models
last_12_months <- joined_returns_monthly[time_index:(time_index-11),]
# building CAPM to predict WFC using Russel 1000
wfc_reg <- lm(monthly.returns ~ monthly.returns.5, data=last_12_months)
summary(wfc_reg)

msft_reg <- lm(monthly.returns.1 ~ monthly.returns.5, data=last_12_months)
summary(msft_reg)

spy_reg <- lm(monthly.returns.2 ~ monthly.returns.5, data=last_12_months)
summary(spy_reg)

gld_reg <- lm(monthly.returns.3 ~ monthly.returns.5, data=last_12_months)
summary(gld_reg)

agg_reg <- lm(monthly.returns.4 ~ monthly.returns.5, data=last_12_months)
summary(agg_reg)

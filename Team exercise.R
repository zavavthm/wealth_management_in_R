## top 10 stocks of bridgewater

library(dplyr)

stock0 <- monthlyReturn(getSymbols(Symbols = "IVV", auto.assign = F))
stock1 <- monthlyReturn(getSymbols(Symbols = "IEMG", auto.assign = F))
stock2 <- monthlyReturn(getSymbols(Symbols = "GOOGL", auto.assign = F))
stock3 <- monthlyReturn(getSymbols(Symbols = "PG", auto.assign = F))
stock4 <- monthlyReturn(getSymbols(Symbols = "NVDA", auto.assign = F))
stock5 <- monthlyReturn(getSymbols(Symbols = "META", auto.assign = F))
stock6 <- monthlyReturn(getSymbols(Symbols = "JNJ", auto.assign = F))
stock7 <- monthlyReturn(getSymbols(Symbols = "WMT", auto.assign = F))
stock8 <- monthlyReturn(getSymbols(Symbols = "COST", auto.assign = F))
stock9 <- monthlyReturn(getSymbols(Symbols = "KO", auto.assign = F))

joined_returns_monthly_bw <- merge.xts(stock0,
                                   stock1,
                                   stock2,
                                   stock3,
                                   stock4,
                                   stock5,
                                   stock6,
                                   stock7,
                                   stock8,
                                   stock9)

# joined_returns_monthly <- as.data.frame(joined_returns_monthly)

time_index <- nrow(joined_returns_monthly_bw)

risk_stock_0 <- sd(joined_returns_monthly_bw$monthly.returns[time_index:(time_index-11)])*sqrt(12)
risk_stock_1 <- sd(joined_returns_monthly_bw$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)
risk_stock_2 <- sd(joined_returns_monthly_bw$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)
risk_stock_3 <- sd(joined_returns_monthly_bw$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)
risk_stock_4 <- sd(joined_returns_monthly_bw$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)
risk_stock_5 <- sd(joined_returns_monthly_bw$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)
risk_stock_6 <- sd(joined_returns_monthly_bw$monthly.returns.6[time_index:(time_index-11)])*sqrt(12)
risk_stock_7 <- sd(joined_returns_monthly_bw$monthly.returns.7[time_index:(time_index-11)])*sqrt(12)
risk_stock_8 <- sd(joined_returns_monthly_bw$monthly.returns.8[time_index:(time_index-11)])*sqrt(12)
risk_stock_9 <- sd(joined_returns_monthly_bw$monthly.returns.9[time_index:(time_index-11)])*sqrt(12)

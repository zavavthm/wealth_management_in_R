## top 10 stocks of bridgewater

library(dplyr)

stock1 <- monthlyReturn(getSymbols(Symbols = "IVV", auto.assign = F))
stock2 <- monthlyReturn(getSymbols(Symbols = "IEMG", auto.assign = F))
stock3 <- monthlyReturn(getSymbols(Symbols = "GOOGL", auto.assign = F))
stock4 <- monthlyReturn(getSymbols(Symbols = "PG", auto.assign = F))
stock5 <- monthlyReturn(getSymbols(Symbols = "NVDA", auto.assign = F))
stock6 <- monthlyReturn(getSymbols(Symbols = "META", auto.assign = F))
stock7 <- monthlyReturn(getSymbols(Symbols = "JNJ", auto.assign = F))
stock8 <- monthlyReturn(getSymbols(Symbols = "WMT", auto.assign = F))
stock9 <- monthlyReturn(getSymbols(Symbols = "COST", auto.assign = F))
stock10 <- monthlyReturn(getSymbols(Symbols = "KO", auto.assign = F))

joined_prices_monthly <- merge.xts(stock1,
                                   stock2,
                                   stock3,
                                   stock4,
                                   stock5,
                                   stock6,
                                   stock7,
                                   stock8,
                                   stock9,
                                   stock10)

joined_returns_monthly <- as.data.frame(joined_prices_monthly)

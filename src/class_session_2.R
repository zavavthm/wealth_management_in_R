# class session 2 
# for joined_prices data, we've daily granularity
# adjusted data column = 6, 12, 18

## data from 2007-01-03 to 2021-12-27
# weird:
# 1. some of the dates are missing


library(readr)
joined_prices = read_csv("C:/Users/manik/Downloads/MBA/4th Sem (Jun - Jul)/Wealth Management in R/joined_prices.csv")

View(joined_prices)

typeof(joined_prices)

joined_prices_df = as.data.frame(joined_prices)
joined_prices_df

typeof(joined_prices_df$Date)

joined_prices_df$Date = as.Date(joined_prices_df$Date, format="%Y-%m-%d")

max_date <- max(joined_prices_df$Date)
min_date <- min(joined_prices_df$Date)

max_date
min_date

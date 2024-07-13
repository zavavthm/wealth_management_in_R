##########################################################################
##########################################################################
##########################################################################
#
# Steps to accomplish the team project:
# 1. Idea is to get the monthly returns of the existing top 10 stocks
# 2. Risk free returns are set at 0.0001
# 3. Calculate Risk for the stocks
# 4. Calculate the Sharpe Ratio for all the stocks
# 5. Calculate the portfolio returns and sharpe ratio
# 6. Repeat the same steps for our recommended top 10 stocks - TE, expected returns, and sharpe ratio
# 7. Compare the returns
#
##########################################################################
##########################################################################
##########################################################################


##########################################################################
##########################################################################
#
# Code to get the percentage split of top 10 stocks in the portfolio
# for both existing and our new recommended stocks
#
##########################################################################
##########################################################################

library(stats)
library(dplyr)
library(scales)
library(tseries)
library(ggplot2)
library(reshape2)
library(quantmod)
library(quadprog)
library(data.table)


enddate <- "2024-07-10"
startdate <- "2016-08-22"

# Define tickers
# new portfolio stocks
tickers <- c("IVV", "SPY", "GOOGL", "GLD", "BAC", "META", "BSX", "WMT", "COST", "KO")

# old portfolio stocks # to run for old, comment the above line and uncomment next one
#tickers <- c("IVV", "IEMG", "GOOGL", "PG", "NVDA", "META", "JNJ", "WMT", "COST", "KO")


# Fetch data for all tickers and align to common date range
all_data <- lapply(tickers, function(ticker) {
  tryCatch({
    data <- getSymbols(ticker, src = "yahoo", from = startdate, to = enddate, auto.assign = FALSE)
    Cl(data) # Use the adjusted closing price
  }, error = function(e) {
    warning(paste("Error fetching data for", ticker))
    return(NULL)
  })
})

# Convert list to data frame and find common date range
pricinglist <- do.call(merge, all_data)
pricinglist <- na.omit(pricinglist) # Remove rows with NA values

# Ensure the data frame has the correct number of rows
t <- nrow(pricinglist)
nstocks <- ncol(pricinglist)

# Forecasting the next price using a backpropagation training algorithm in a neural network. 
# An Autoregressive Model of fourth order AR4 was used.

newpricingdataset <- pricinglist

# Creating a dataset with monthly ROR for each day using continuous compounding
dailyROR <- as.data.frame(matrix(ncol = ncol(newpricingdataset), nrow = nrow(newpricingdataset) - 25))
colnames(dailyROR) <- colnames(pricinglist)
for (c in 1:ncol(newpricingdataset)) {
  for (r in 1:(nrow(newpricingdataset) - 25)) {
    dailyROR[r, c] <- log(as.numeric(newpricingdataset[(r + 25), c]) / as.numeric(newpricingdataset[r, c]))
  }
}
# The most current expected return for n+25 (n is today) is in the last row of the above dataset

# Calculating Expected(R) for all securities 
averet <- as.matrix(dailyROR[nrow(dailyROR), ], nrow = 1)
# Calculating covariance matrix
rcov <- cov(dailyROR[(nrow(dailyROR) - 125):(nrow(dailyROR)), ]) # 125 stands for 6 trading months
target.r <- 1 / 1000
# Using solver to get to optimal weights

effFrontier <- function(averet, rcov, nports, shorts, wmax, wmin) {
  mxret <- max(averet)
  mnret <- -mxret
  n.assets <- ncol(averet)
  reshigh <- rep(wmax, n.assets)
  reslow <- rep(wmin, n.assets)
  min.rets <- seq(mnret, mxret, length.out = nports)
  vol <- rep(NA, nports)
  ret <- rep(NA, nports)
  pw <- data.frame(matrix(ncol = nports, nrow = n.assets))
  for (i in 1:nports) {
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x = averet, pm = min.rets[i], covmat = rcov, reshigh = reshigh, reslow = reslow, shorts = F), silent = T)
    if (!is.null(port.sol)) {
      vol[i] <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[i] <- averet %*% port.sol$pw
      pw[, i] <- port.sol$pw
    }
  }
  return(list(vol = vol, ret = ret, weights = pw))
}

maxSharpe <- function(averet, rcov, shorts = F, wmax = 0.2, min.weight = 0.01) {
  optim.callback <- function(param, averet, rcov, reshigh, reslow, shorts) { 
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x = averet, pm = param, covmat = rcov, reshigh = reshigh, reslow = reslow, shorts = shorts), silent = T)
    if (is.null(port.sol)) { ratio = 10^9 } else {
      m.return <- averet %*% port.sol$pw
      m.risk <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio <- m.return / m.risk
      assign("w", port.sol$pw, inherits = T)
    }
    return(ratio)
  }
  
  ef <- effFrontier(averet = averet, rcov = rcov, shorts = shorts, wmax = wmax, nports = 100, wmin = min.weight)
  n <- ncol(averet)
  reshigh <- rep(wmax, n)
  reslow <- rep(min.weight, n)
  
  max.sh <- which.max(ef$ret / ef$vol)
  
  if (is.na(ef$ret[max.sh - 1])) { lowerinterval <- ef$ret[max.sh] } else { lowerinterval <- ef$ret[max.sh - 1] }
  if (is.na(ef$ret[max.sh + 1])) { upperinterval <- ef$ret[max.sh] } else { upperinterval <- ef$ret[max.sh + 1] }
  
  w <- rep(0, ncol(averet))
  xmin <- optimize(f = optim.callback, interval = c(lowerinterval, upperinterval), 
                   averet = averet, rcov = rcov, reshigh = reshigh, reslow = reslow, shorts = shorts)
  return(w)
  return(xmin)
}

z <- maxSharpe(averet, rcov, shorts = F, wmax = 0.17)

print(z)

##########################################################################
##########################################################################
#
# Code to get the Internal Risk (sigma) for every stock,
#                 Tracking Error for every stock,
#                 Sharpe Ratio for every stock,
#                 Treynor Ratio for every stock,
#                 Creation of Quasi CAPM model (to compare stocks with the VONE benchmark),
#                 Calculating existing and updated portfolio returns & Sharpe Ratio
#
##########################################################################
##########################################################################

# get the monthly returns of existing top 10 stocks
ivv <- monthlyReturn(getSymbols(Symbols = "IVV", auto.assign = F))
iemg<- monthlyReturn(getSymbols(Symbols = "IEMG", auto.assign = F))
googl <- monthlyReturn(getSymbols(Symbols = "GOOGL", auto.assign = F))
pg <- monthlyReturn(getSymbols(Symbols = "PG", auto.assign = F))
nvda <- monthlyReturn(getSymbols(Symbols = "NVDA", auto.assign = F))
meta <- monthlyReturn(getSymbols(Symbols = "META", auto.assign = F))
jnj <- monthlyReturn(getSymbols(Symbols = "JNJ", auto.assign = F))
wmt <- monthlyReturn(getSymbols(Symbols = "WMT", auto.assign = F))
cost <- monthlyReturn(getSymbols(Symbols = "COST", auto.assign = F))
ko <- monthlyReturn(getSymbols(Symbols = "KO", auto.assign = F))

joined_monthly_returns_bw <- merge.xts(ivv,
                                       iemg,
                                       googl,
                                       pg,
                                       nvda,
                                       meta,
                                       jnj,
                                       wmt,
                                       cost,
                                       ko
                                       )

# Monthly Returns for new stocks 
spy <- monthlyReturn(getSymbols(Symbols = "SPY", auto.assign = F))
gld <- monthlyReturn(getSymbols(Symbols = "GLD", auto.assign = F))
bac <- monthlyReturn(getSymbols(Symbols = "BAC", auto.assign = F))
bsx <- monthlyReturn(getSymbols(Symbols = "BSX", auto.assign = F))

new_joined_monthly_returns_bw <- merge.xts(ivv,
                                           spy,
                                           googl,
                                           gld,
                                           bac,
                                           meta,
                                           bsx,
                                           wmt,
                                           cost,
                                           ko
                                           )

# adding a benchmark VONE for comparison
benchmark_returns <- monthlyReturn(getSymbols("VONE", auto.assign = F))

# joining the benchmark with the stock returns
joined_monthly_returns_bw <- merge.xts(joined_monthly_returns_bw, benchmark_returns)

# joining the benchmark with the new stock returns
new_joined_monthly_returns_bw <- merge.xts(new_joined_monthly_returns_bw, benchmark_returns)

# renaming the column names of the portfolio columns - existing stocks
colnames(joined_monthly_returns_bw) <- c("ivv","iemg","googl","pg","nvda","meta","jnj","wmt","cost","ko","vone")

# renaming the column names of the new portfolio columns
colnames(new_joined_monthly_returns_bw) <- c("ivv","spy","googl","gld","bac","meta","bsx","wmt","cost","ko","vone")

# getting the total number of rows in our data
time_index <- nrow(joined_monthly_returns_bw)

# getting the total number of rows of our new portfolio
time_index_new <- nrow(new_joined_monthly_returns_bw)

## calculating the internal risk (sigma) for top 10 stocks in BW portfolio
sigma_ivv <- sd(joined_monthly_returns_bw$ivv[time_index:(time_index-11)])*sqrt(12)
sigma_iemg <- sd(joined_monthly_returns_bw$iemg[time_index:(time_index-11)])*sqrt(12)
sigma_googl <- sd(joined_monthly_returns_bw$googl[time_index:(time_index-11)])*sqrt(12)
sigma_pg <- sd(joined_monthly_returns_bw$pg[time_index:(time_index-11)])*sqrt(12)
sigma_nvda <- sd(joined_monthly_returns_bw$nvda[time_index:(time_index-11)])*sqrt(12)
sigma_meta <- sd(joined_monthly_returns_bw$meta[time_index:(time_index-11)])*sqrt(12)
sigma_jnj <- sd(joined_monthly_returns_bw$jnj[time_index:(time_index-11)])*sqrt(12)
sigma_wmt <- sd(joined_monthly_returns_bw$wmt[time_index:(time_index-11)])*sqrt(12)
sigma_cost <- sd(joined_monthly_returns_bw$cost[time_index:(time_index-11)])*sqrt(12)
sigma_ko <- sd(joined_monthly_returns_bw$ko[time_index:(time_index-11)])*sqrt(12)

# calculating the internal risk (sigma) for the new BW portfolio stocks
sigma_spy <- sd(new_joined_monthly_returns_bw$spy[time_index_new:(time_index_new-11)])*sqrt(12)
sigma_gld <- sd(new_joined_monthly_returns_bw$gld[time_index_new:(time_index_new-11)])*sqrt(12)
sigma_bac <- sd(new_joined_monthly_returns_bw$bac[time_index_new:(time_index_new-11)])*sqrt(12)
sigma_bsx <- sd(new_joined_monthly_returns_bw$bsx[time_index_new:(time_index_new-11)])*sqrt(12)

## Tracking error of top 10 stocks of Bridgewater

te_ivv <- sd(joined_monthly_returns_bw$ivv[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_iemg <- sd(joined_monthly_returns_bw$iemg[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_googl <- sd(joined_monthly_returns_bw$googl[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_pg <- sd(joined_monthly_returns_bw$pg[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_nvda <- sd(joined_monthly_returns_bw$nvda[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_meta <- sd(joined_monthly_returns_bw$meta[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_jnj <- sd(joined_monthly_returns_bw$jnj[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_wmt <- sd(joined_monthly_returns_bw$wmt[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_cost <- sd(joined_monthly_returns_bw$cost[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)
te_ko <- sd(joined_monthly_returns_bw$ko[time_index:(time_index-11)]-joined_monthly_returns_bw$vone[time_index:(time_index-11)])*sqrt(12)


# Tracking error of the new BW portfolio stocks
te_spy <- sd(new_joined_monthly_returns_bw$spy[time_index_new:(time_index_new-11)]-new_joined_monthly_returns_bw$vone[time_index_new:(time_index_new-11)])*sqrt(12)
te_gld <- sd(new_joined_monthly_returns_bw$gld[time_index_new:(time_index_new-11)]-new_joined_monthly_returns_bw$vone[time_index_new:(time_index_new-11)])*sqrt(12)
te_bac <- sd(new_joined_monthly_returns_bw$bac[time_index_new:(time_index_new-11)]-new_joined_monthly_returns_bw$vone[time_index_new:(time_index_new-11)])*sqrt(12)
te_bsx <- sd(new_joined_monthly_returns_bw$bsx[time_index_new:(time_index_new-11)]-new_joined_monthly_returns_bw$vone[time_index_new:(time_index_new-11)])*sqrt(12)


## calculating stat expectation using mean() function

# Calculating Sharpe Ratio of individual stocks
riskfree <- 0.0001

# existing stocks
exp_returns_ivv <- mean(joined_monthly_returns_bw$ivv[time_index:(time_index-11)])
exp_returns_iemg <- mean(joined_monthly_returns_bw$iemg[time_index:(time_index-11)])
exp_returns_googl <- mean(joined_monthly_returns_bw$googl[time_index:(time_index-11)])
exp_returns_pg <- mean(joined_monthly_returns_bw$pg[time_index:(time_index-11)])
exp_returns_nvda <- mean(joined_monthly_returns_bw$nvda[time_index:(time_index-11)])
exp_returns_meta <- mean(joined_monthly_returns_bw$meta[time_index:(time_index-11)])
exp_returns_jnj <- mean(joined_monthly_returns_bw$jnj[time_index:(time_index-11)])
exp_returns_wmt <- mean(joined_monthly_returns_bw$wmt[time_index:(time_index-11)])
exp_returns_cost <- mean(joined_monthly_returns_bw$cost[time_index:(time_index-11)])
exp_returns_ko <- mean(joined_monthly_returns_bw$ko[time_index:(time_index-11)])

# new stocks
exp_returns_spy <- mean(new_joined_monthly_returns_bw$spy[time_index_new:(time_index_new-11)])
exp_returns_gld <- mean(new_joined_monthly_returns_bw$gld[time_index_new:(time_index_new-11)])
exp_returns_bac <- mean(new_joined_monthly_returns_bw$bac[time_index_new:(time_index_new-11)])
exp_returns_bsx <- mean(new_joined_monthly_returns_bw$bsx[time_index_new:(time_index_new-11)])


## Sharpe Ratio

# BW existing stocks Sharpe ratio 
sharpe_ratio_ivv <- (((1+exp_returns_ivv)^12)-1-riskfree)/sigma_ivv
sharpe_ratio_iemg <- (((1+exp_returns_iemg)^12)-1-riskfree)/sigma_iemg
sharpe_ratio_googl <- (((1+exp_returns_googl)^12)-1-riskfree)/sigma_googl
sharpe_ratio_pg <- (((1+exp_returns_pg)^12)-1-riskfree)/sigma_pg
sharpe_ratio_nvda <- (((1+exp_returns_nvda)^12)-1-riskfree)/sigma_nvda
sharpe_ratio_meta <- (((1+exp_returns_meta)^12)-1-riskfree)/sigma_meta
sharpe_ratio_jnj <- (((1+exp_returns_jnj)^12)-1-riskfree)/sigma_jnj
sharpe_ratio_wmt <- (((1+exp_returns_wmt)^12)-1-riskfree)/sigma_wmt
sharpe_ratio_cost <- (((1+exp_returns_cost)^12)-1-riskfree)/sigma_cost
sharpe_ratio_ko <- (((1+exp_returns_ko)^12)-1-riskfree)/sigma_ko

# new stocks sharpe ratio
sharpe_ratio_spy <- (((1+exp_returns_spy)^12)-1-riskfree)/sigma_spy
sharpe_ratio_gld <- (((1+exp_returns_gld)^12)-1-riskfree)/sigma_gld
sharpe_ratio_bac <- (((1+exp_returns_bac)^12)-1-riskfree)/sigma_bac
sharpe_ratio_bsx <- (((1+exp_returns_bsx)^12)-1-riskfree)/sigma_bsx


## creating quasi capm models

last_12_months <- joined_monthly_returns_bw[time_index:(time_index-11),]
last_12_months_new <- new_joined_monthly_returns_bw[time_index_new:(time_index_new-11),]

# building CAPM to predict stocks individually using Russel 1000

reg_ivv <- lm(ivv ~ vone, data=last_12_months)
reg_iemg <- lm(iemg ~ vone, data=last_12_months)
reg_googl <- lm(googl ~ vone, data=last_12_months)
reg_pg <- lm(pg ~ vone, data=last_12_months)
reg_nvda <- lm(nvda ~ vone, data=last_12_months)
reg_meta <- lm(meta ~ vone, data=last_12_months)
reg_jnj <- lm(jnj ~ vone, data=last_12_months)
reg_wmt <- lm(wmt ~ vone, data=last_12_months)
reg_cost <- lm(cost ~ vone, data=last_12_months)
reg_ko <- lm(ko ~ vone, data=last_12_months)

reg_spy <- lm(spy ~ vone, data=last_12_months_new)
reg_gld <- lm(gld ~ vone, data=last_12_months_new)
reg_bac <- lm(bac ~ vone, data=last_12_months_new)
reg_bsx <- lm(bsx ~ vone, data=last_12_months_new)

# old stocks
summary(reg_ivv)
summary(reg_iemg)
summary(reg_googl)
summary(reg_pg)
summary(reg_nvda)
summary(reg_meta)
summary(reg_jnj)
summary(reg_wmt)
summary(reg_cost)
summary(reg_ko)

# new stocks
summary(reg_spy)
summary(reg_gld)
summary(reg_bac)
summary(reg_bsx)

## Treynor Ratio

trey_ivv <- (((1+exp_returns_ivv)^12)-1-riskfree)/reg_ivv$coefficients[2]
trey_iemg <- (((1+exp_returns_iemg)^12)-1-riskfree)/reg_iemg$coefficients[2]
trey_googl <- (((1+exp_returns_googl)^12)-1-riskfree)/reg_googl$coefficients[2]
trey_pg <- (((1+exp_returns_pg)^12)-1-riskfree)/reg_pg$coefficients[2]
trey_nvda <- (((1+exp_returns_nvda)^12)-1-riskfree)/reg_nvda$coefficients[2]
trey_meta <- (((1+exp_returns_meta)^12)-1-riskfree)/reg_meta$coefficients[2]
trey_jnj <- (((1+exp_returns_jnj)^12)-1-riskfree)/reg_jnj$coefficients[2]
trey_wmt <- (((1+exp_returns_wmt)^12)-1-riskfree)/reg_wmt$coefficients[2]
trey_cost <- (((1+exp_returns_cost)^12)-1-riskfree)/reg_cost$coefficients[2]
trey_ko <- (((1+exp_returns_ko)^12)-1-riskfree)/reg_ko$coefficients[2]

trey_spy <- (((1+exp_returns_spy)^12)-1-riskfree)/reg_spy$coefficients[2]
trey_gld <- (((1+exp_returns_gld)^12)-1-riskfree)/reg_gld$coefficients[2]
trey_bac <- (((1+exp_returns_bac)^12)-1-riskfree)/reg_bac$coefficients[2]
trey_bsx <- (((1+exp_returns_bsx)^12)-1-riskfree)/reg_bsx$coefficients[2]


# Treynor ratio - old stocks
print(trey_ivv)
print(trey_iemg)
print(trey_googl)
print(trey_pg)
print(trey_nvda)
print(trey_meta)
print(trey_jnj)
print(trey_wmt)
print(trey_cost)
print(trey_ko)

# Treynor ratio - new stocks
print(trey_spy)
print(trey_gld)
print(trey_bac)
print(trey_bsx)

#--------------------------------------------------------------------------

## creating portfolio returns (OLD BW PORTFOLIO)
# Assuming that entire portfolio has only top 10 stocks, so splitting
# the weightage accordingly to make it whole 100%
# weightage provided considering there are the only stocks in the portfolio
ivv_w <- 0.17
iemg_w <- 0.17
googl_w <- 0.17
pg_w <- 0.01
nvda_w <- 0.01
meta_w <- 0.071
jnj_w <- 0.01
wmt_w <- 0.168
cost_w <- 0.119
ko_w <- 0.102

# calculating monthly returns with weightage per stock
joined_monthly_returns_bw <- as.data.frame(joined_monthly_returns_bw) %>%
  mutate(portfolio = ivv_w * ivv + 
           iemg_w * iemg + 
           googl_w * googl + 
           pg_w * pg +
           nvda_w * nvda +
           meta_w * meta +
           jnj_w * jnj +
           wmt_w * wmt +
           cost_w * cost +
           ko_w * ko)

port_sigma <- sd(joined_monthly_returns_bw$portfolio[time_index:(time_index-11)])*sqrt(12)

port_expected <- mean(joined_monthly_returns_bw$portfolio[time_index:(time_index-11)])

sharpe_ratio_port <- (((1+port_expected)^12)-1-riskfree)/port_sigma

## calculating correlation
cor(joined_monthly_returns_bw[time_index:(time_index-11), ])

#-------------------------------------------------------------------------

## creating portfolio returns (NEW RECOMMENDED BW PORTFOLIO)
# Assuming that entire portfolio has only top 10 stocks, so splitting
# the weightage accordingly to make it whole 100%
ivv_w <- 0.17
spy_w <- 0.049
googl_w <- 0.17
gld_w <- 0.17
bac_w <- 0.07
meta_w <- 0.049
bsx_w <- 0.056
wmt_w <- 0.082
cost_w <- 0.123
ko_w <- 0.061

# calculating monthly returns with weightage per stock
new_joined_monthly_returns_bw <- as.data.frame(new_joined_monthly_returns_bw) %>%
  mutate(portfolio = ivv_w * ivv + 
           spy_w * spy + 
           googl_w * googl + 
           gld_w * gld +
           bac_w * bac +
           meta_w * meta +
           bsx_w * bsx +
           wmt_w * wmt +
           cost_w * cost +
           ko_w * ko)

new_port_sigma <- sd(new_joined_monthly_returns_bw$portfolio[time_index_new:(time_index_new-11)])*sqrt(12)

new_port_expected <- mean(new_joined_monthly_returns_bw$portfolio[time_index_new:(time_index_new-11)])

sharpe_ratio_port_new <- (((1+new_port_expected)^12)-1-riskfree)/new_port_sigma

## calculating correlation
cor(new_joined_monthly_returns_bw[time_index_new:(time_index_new-11), ])


##########################################################################
##########################################################################
##########################################################################
##########################################################################
#
# Code to draw all the charts/graphs/plots used in the presentation
#
##########################################################################
##########################################################################
##########################################################################
##########################################################################


## BAR GRAPH comparing the portfolio returns. (old and new)
#  --------------------------------------------------------------

returns <- c(exp_returns_spy, port_expected, new_port_expected)
colnames <- c("S&P500", "OLD_PORT", "NEW_PORT")

df <- data.frame(returns, colnames)

# Define the color palette
colors <- c("S&P500" = "grey", "OLD_PORT" = "lightblue", "NEW_PORT" = "blue")

# Plot the returns with facets
ggplot(df, aes(x = colnames, y = returns, fill = colnames)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Comparison of Port returns with S&P500",
       x = "Portfolios",
       y = "Annualized Expected Returns") +
  theme_minimal() +
  theme(legend.position = "none")



## BAR GRAPH comparing the sharpe ratio of the portfolios (old and new)
#  --------------------------------------------------------------

sr <- c(sharpe_ratio_spy, sharpe_ratio_port, sharpe_ratio_port_new)
colnames <- c("S&P500", "OLD_PORT", "NEW_PORT")

df <- data.frame(sr, colnames)

# Define the color palette
colors <- c("S&P500" = "grey", "OLD_PORT" = "lightblue", "NEW_PORT" = "blue")

# Plot the Sharpe Ratios with facets
ggplot(df, aes(x = colnames, y = sr, fill = colnames)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Comparison of Sharpe Ratio with S&P500",
       x = "Portfolios",
       y = "Sharpe Ratios") +
  theme_minimal() +
  theme(legend.position = "none")



## Chart showing the Sharpe Ratio of all the stocks (old and new)
#  --------------------------------------------------------------

# List of all stock symbols (existing and new)
symbols <- c("IVV", "IEMG", "GOOGL", "PG", "NVDA", "META", "JNJ", "WMT", "COST", "KO", "SPY", "GLD", "BAC", "BSX")

# stocks in existing BW portfolio (previous)
previous_stocks <- c("IEMG", "PG", "NVDA", "JNJ")

# New stocks
new_stocks <- c("SPY", "GLD", "BAC", "BSX")

# Fetch data for all symbols
data_list <- lapply(symbols, function(sym) getSymbols(sym, auto.assign = FALSE))

# Calculate monthly returns for each symbol
returns_list <- lapply(data_list, monthlyReturn)
names(returns_list) <- symbols

# Combine returns into a single data frame
combined_returns <- do.call(merge, returns_list)
colnames(combined_returns) <- symbols

# Define the risk-free rate
risk_free_rate <- 0.0001

# Function to calculate Sharpe Ratio
sharpe_ratio <- function(stock_returns, risk_free_rate) {
  excess_returns <- stock_returns - risk_free_rate / 12  # Convert annual risk-free rate to monthly
  return(mean(excess_returns, na.rm = TRUE) / sd(excess_returns, na.rm = TRUE) * sqrt(12))  # Annualize Sharpe Ratio
}

# Calculate Sharpe Ratios for the specified stocks
sharpe_ratios <- data.frame(
  Stock = symbols,
  SharpeRatio = sapply(symbols, function(sym) {
    sharpe_ratio(combined_returns[, sym], risk_free_rate)
  })
)

# Add a column to indicate whether the stock is previous, new, or selected
sharpe_ratios$Facet <- ifelse(sharpe_ratios$Stock %in% previous_stocks, "Previous", "Current")
sharpe_ratios$Type <- ifelse(sharpe_ratios$Stock %in% new_stocks, "New", "Selected")

# Define the color palette
colors <- c("IVV" = "blue", "IEMG" = "grey", "GOOGL" = "blue", 
            "PG" = "grey", "NVDA" = "grey", "META" = "blue", 
            "JNJ" = "grey", "WMT" = "blue", "COST" = "blue", 
            "KO" = "blue", "SPY" = "lightblue", "GLD" = "lightblue", 
            "BAC" = "lightblue", "BSX" = "lightblue")

# Plot the Sharpe Ratios with facets
ggplot(sharpe_ratios, aes(x = Stock, y = SharpeRatio, fill = Stock)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Sharpe Ratios of Selected Stocks",
       x = "Stock",
       y = "Annualized Sharpe Ratio") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(~ Facet, scales = "free_x", space = "free")


## Scatter plot visualizing the Returns per unit risk (i.e. Sharpe Ratio) for every stock
#  --------------------------------------------------------------

# Function to calculate expected return (annualized) and standard deviation (annualized)
calc_stats <- function(stock_returns, risk_free_rate) {
  monthly_excess_returns <- stock_returns - risk_free_rate / 12
  expected_return <- mean(stock_returns, na.rm = TRUE) * 12
  std_dev <- sd(stock_returns, na.rm = TRUE) * sqrt(12)
  sharpe_ratio <- mean(monthly_excess_returns, na.rm = TRUE) / sd(monthly_excess_returns, na.rm = TRUE) * sqrt(12)
  return(c(ExpectedReturn = expected_return, StdDev = std_dev, SharpeRatio = sharpe_ratio))
}

# Calculate stats for each stock
stats <- sapply(symbols, function(sym) {
  calc_stats(combined_returns[, sym], risk_free_rate)
})

# Convert to data frame for plotting
stats_df <- data.frame(t(stats))
stats_df$Stock <- rownames(stats_df)

# Add a column to indicate whether the stock is previous, new, or selected
stats_df$Type <- ifelse(stats_df$Stock %in% previous_stocks, "Previous", 
                        ifelse(stats_df$Stock %in% new_stocks, "New", "Selected"))

# Define a color palette
colors <- c("Selected" = "blue", "Previous" = "grey", "New" = "lightblue")

# Plot the scatter plot
ggplot() +
  geom_point(data = stats_df %>% filter(Type == "Previous"), aes(x = StdDev, y = ExpectedReturn, size = SharpeRatio, color = Type), show.legend = FALSE) +
  geom_point(data = stats_df %>% filter(Type != "Previous"), aes(x = StdDev, y = ExpectedReturn, size = SharpeRatio, color = Type), show.legend = FALSE) +
  geom_text(data = stats_df, aes(x = StdDev, y = ExpectedReturn, label = Stock), vjust = 1.5, hjust = 1.5) +
  scale_color_manual(values = colors) +
  labs(title = "Stocks Sharpe Ratios Comparison",
       x = "Annualized Standard Deviation (Risk)",
       y = "Annualized Expected Return",
       size = "Sharpe Ratio") +
  theme_minimal()



# Comparison of JNJ with BSX. Why we chose BSX over it visually
#  --------------------------------------------------------------

ticker1 <- "BAC"
ticker2<- "SPY"
ticker3<- "GLD"
mydf1 <- as.data.frame(getSymbols(ticker1, auto.assign=FALSE))
mydf2 <- as.data.frame(getSymbols(ticker2, auto.assign=FALSE))
mydf3 <- as.data.frame(getSymbols(ticker3, auto.assign=FALSE))

combined_df <- cbind(mydf1[,4], mydf2[,4], mydf3[,4])
dt <- as.data.frame(combined_df)

# renaming column names
colnames(dt) <- c(ticker1, ticker2, ticker3)
dt$date = as.Date(rownames(mydf1))

# transposing df
dt <- melt(dt, id="date")
colnames(dt) <- c("date", "ticker","price")
dt <- data.table(dt)

# create indexed values
dt[, idx_price := price/price[1], by = ticker]

# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility (Risk)") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.001)) +
  scale_x_continuous(label = percent, limits = c(0, 0.02))

################################################
#### Two risky assets portfolio
################################################

#-----------------------------------------------

# efficient frontier 2 stocks
# load the data
ticker1_select <- "SPY" #which of the 3 do you want to use
ticker2_select <- "BSX"
ticker3_select <- "GLD"

mydf1 <- as.data.frame(monthlyReturn(getSymbols(ticker1_select, auto.assign=FALSE)))
mydf2 <- as.data.frame(monthlyReturn(getSymbols(ticker2_select, auto.assign=FALSE)))
mydf3 <- as.data.frame(monthlyReturn(getSymbols(ticker3_select, auto.assign=FALSE)))


combined_df <- cbind(mydf1[,1], mydf2[,1], mydf3[,1])

df <- as.data.frame(combined_df)
colnames(df) <- c("x","y", "z")
df$date = as.Date(rownames(mydf1))
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)

# III) covariance
cov_xy <- cov(df$x, df$y)

# create 1000 portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights,
                         wy = 1 - x_weights)

# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,
                   sd_p = sqrt(wx^2 * sd_x^2 +
                                 wy^2 * sd_y^2 +
                                 2 * wx * (1 - wx) * cov_xy))]
two_assets



# lastly plot the values
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets - SPY & BSX") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)



# Visualizing the efficient frontier for 3 stocks BSX, SPY, GLD
#  --------------------------------------------------------------

######################################################
### Three risky assets portfolio:
######################################################
# load the data

# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)

# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]

# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("SPY, BAC and GLD") +
  xlab("Volatility (Risk)") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)


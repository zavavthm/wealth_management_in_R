# Load necessary libraries
library(quantmod)
library(ggplot2)
library(dplyr)

# List of stock symbols
symbols <- c("IVV", "IEMG", "GOOGL", "PG", "NVDA", "META", "JNJ", "WMT", "COST", "KO", "SPY", "GLD", "USDT-USD", "BSX")

# Previous stocks
previous_stocks <- c("IEMG", "PG", "NVDA", "JNJ")

# New stocks
new_stocks <- c("SPY", "GLD", "USDT-USD", "BSX")

# Fetch data for all symbols
data_list <- lapply(symbols, function(sym) getSymbols(sym, auto.assign = FALSE))

# Calculate monthly returns for each symbol
returns_list <- lapply(data_list, monthlyReturn)
names(returns_list) <- symbols

# Combine returns into a single data frame
combined_returns <- do.call(merge, returns_list)
colnames(combined_returns) <- symbols

# Define the risk-free rate
risk_free_rate <- 0.01 / 100

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

# Define a color palette
colors <- c("IVV" = "blue", "IEMG" = "grey", "GOOGL" = "blue", 
            "PG" = "grey", "NVDA" = "grey", "META" = "blue", 
            "JNJ" = "grey", "WMT" = "blue", "COST" = "blue", 
            "KO" = "blue", "SPY" = "lightblue", "GLD" = "lightblue", 
            "USDT-USD" = "lightblue", "BSX" = "lightblue")

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


#######################################

#######################################

#######################################

# Load necessary libraries
library(quantmod)
library(ggplot2)
library(dplyr)

# List of stock symbols
symbols <- c("IVV", "IEMG", "GOOGL", "PG", "NVDA", "META", "JNJ", "WMT", "COST", "KO", "SPY", "GLD", "USDT-USD", "BSX")

# Previous stocks
previous_stocks <- c("IEMG", "PG", "NVDA", "JNJ")

# New stocks
new_stocks <- c("SPY", "GLD", "USDT-USD", "BSX")

# Fetch data for all symbols
data_list <- lapply(symbols, function(sym) getSymbols(sym, auto.assign = FALSE))

# Calculate monthly returns for each symbol
returns_list <- lapply(data_list, monthlyReturn)
names(returns_list) <- symbols

# Combine returns into a single data frame
combined_returns <- do.call(merge, returns_list)
colnames(combined_returns) <- symbols

# Define the risk-free rate
risk_free_rate <- 0.01 / 100

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
  labs(title = "Expected Return vs. Standard Deviation with Sharpe Ratios",
       x = "Annualized Standard Deviation (Risk)",
       y = "Annualized Expected Return",
       size = "Sharpe Ratio") +
  theme_minimal()

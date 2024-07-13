# Wealth Management Final Coding Individual Assignment

## **Task**: To provide full risk and return analysis of the portfolio and make recommendations on rebalancing for an Ultra High Net Worth client, based in Palo Alto, CA. Your investor has the following assets:

| Ticker | Name | Current % allocation | Asset class | Comment(s) |
| --- | --- | --- | --- | --- |
| IXN | https://hedgefollow.com/stocks/IXN | 17.5% | Equity | seeks to track the investment results of an index composed of global equities in the technology sector |
| QQQ | https://hedgefollow.com/stocks/QQQ | 22.1% | Equity | Seeks to track the NASDAQ-100 Index that is made of 100 largest non-financial companies listed on NASDAQ |
| IEF | https://hedgefollow.com/stocks/IEF | 28.5% | Fixed Income | Seeks to tracks the investment results of an index composed of U.S. Treasury bonds with remaining maturities between seven and ten years |
| VNQ | https://hedgefollow.com/stocks/VNQ | 8.9% | Real Assets | Seeks to track the performance of the MSCI US Investable Market Real Estate 25/50 Index |
| GLD | https://hedgefollow.com/stocks/GLD | 23% | Commodities | seeks to track an index: LBMA London Gold Market Fixing Price PM Index. The GLD ETF provides physical exposure, so by buying it you actually own parts of all the underlying holdings |

### Answer the following helper questions and make recommendations.:

1. What is the most recent 12M*, 18M, 24M (months) return for each of the securities (and for the entire portfolio)?
2. What are the correlations between your assets? Are there any interesting correlations?
3. What is the most recent 12M sigma (risk) for each of the securities (and for the entire portfolio)?
4. Based on the previous 3 questions, which holdings would you sell, which holdings would you buy?
5. How will your portfolio risk and expected returns change after rebalancing (selling and buying)?
6. Can you build an efficient frontier for this portfolio (select 3 assets with similar high sharpe)? What can you say based on the efficient frontier?

## Answers:

1. Most Recent Returns for the each of the securities and the entire portfolio:

| Asset | IXN | QQQ | IEF | VNQ | GLD | Portfolio |
| --- | --- | --- | --- | --- | --- | --- |
| 12 Months Return | 2.76% | 2.25% | -0.04% | 0.40% | 1.76% |  |
| 18 Months Return | 3.26% | 3.02% | -0.22% | -0.08% | 1.29% |  |
| 24 Months Return | 2.4% | 2.06% | -0.39% | -0.31% | 1.37% |  |

# Code for Expected returns of the individual securities:

```r
## expected returns of the assets (12 months)
exp_returns_ixn <- mean(joined_monthly_returns$ixn[time_index:(time_index-11)])
exp_returns_qqq <- mean(joined_monthly_returns$qqq[time_index:(time_index-11)])
exp_returns_ief <- mean(joined_monthly_returns$ief[time_index:(time_index-11)])
exp_returns_vnq <- mean(joined_monthly_returns$vnq[time_index:(time_index-11)])
exp_returns_gld <- mean(joined_monthly_returns$gld[time_index:(time_index-11)])

## expected returns of the assets (18 months)
exp_returns_ixn_18 <- mean(joined_monthly_returns$ixn[time_index:(time_index-17)])
exp_returns_qqq_18 <- mean(joined_monthly_returns$qqq[time_index:(time_index-17)])
exp_returns_ief_18 <- mean(joined_monthly_returns$ief[time_index:(time_index-17)])
exp_returns_vnq_18 <- mean(joined_monthly_returns$vnq[time_index:(time_index-17)])
exp_returns_gld_18 <- mean(joined_monthly_returns$gld[time_index:(time_index-17)])

## expected returns of the assets (24 months)
exp_returns_ixn_24 <- mean(joined_monthly_returns$ixn[time_index:(time_index-23)])
exp_returns_qqq_24 <- mean(joined_monthly_returns$qqq[time_index:(time_index-23)])
exp_returns_ief_24 <- mean(joined_monthly_returns$ief[time_index:(time_index-23)])
exp_returns_vnq_24 <- mean(joined_monthly_returns$vnq[time_index:(time_index-23)])
exp_returns_gld_24 <- mean(joined_monthly_returns$gld[time_index:(time_index-23)])
```
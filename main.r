# .....................................................................
#
# Small script to show how to use the functions.
#
# .....................................................................
options(digits=4, width=70)

#.....................................................
#1. get prices and returns
source(file="rfin.returns.r")
data <- rfin.returns.get(c("ba", "msft", "sbux", "aapl"), from="2010-01-01")
rfin.returns.plot(data$returns)

#.....................................................
#2. estimate covariances, create "min variance" portfolio
source(file="rfin.portfolio.r")
monthly.estimates <- rfin.returns.estimate(data$returns, type="monthly")
portfolio.min.var <- rfin.portfolio.create(monthly.estimates, type="min.variance", risk.free.rate=0.03)
rfin.portfolio.plot(portfolio.min.var)

#.....................................................
#3. create and plot an efficient portfolio with 3.5% monthly return, print out its weights
portfolio3.5 <- rfin.portfolio.create(monthly.estimates, risk.free.rate=0.03, desired.return=0.035)
rfin.portfolio.plot(portfolio3.5)
portfolio3.5$weights

#.....................................................
#4. compute the betas for portfolio assets, using S&P500 as a market index
data.sp500 <- rfin.returns.get(c("^GSPC"), from="2010-01-01")
colnames(data.sp500$returns) <- "sp500"
returns.all <- merge(data$returns, data.sp500$returns)
names <- colnames(returns.all)[colnames(returns.all) != "sp500"]
betas <- lm(names~sp500, data=returns.all)





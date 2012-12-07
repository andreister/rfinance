# .....................................................................
#
# Small script to show how to use the functions.
#
# .....................................................................
options(digits=4, width=70)
set.seed(123)

#.....................................................
#get prices and returns
source(file="rfin/returns.r")
data <- rfin.returns.get(symbols="ba,msft,sbux,aapl", from="2010-01-01")
rfin.returns.plot(data$returns)

#.....................................................
#estimate the returns
estimates <- rfin.returns.estimate(data$returns, type="annual")
estimates$assets

#.....................................................
#calculate 5% VaR (and its error) for $30M investment in MSFT
VaR <- rfin.returns.VaR(data$returns[,"msft"], p=0.05, wealth=30)   #!!!!!!!!check against PerformanceAnalytics.VaR(returns, p=.95, method="gaussian") ??
VaR$value
VaR$error

#.....................................................
#create and plot "min variance" portfolio
source(file="rfin/portfolio.r")
portfolio.min.var <- rfin.portfolio.create(estimates, type="min.variance", risk.free.return=0.03)
portfolio.min.var$risk
portfolio.min.var$return
rfin.portfolio.plot(portfolio.min.var)

#.....................................................
#create and plot an efficient portfolio with 30% annual return, print out its weights
portfolio30 <- rfin.portfolio.create(estimates, risk.free.return=0.03, desired.return=0.3)
rfin.portfolio.plot(portfolio30)
portfolio30$weights

#.....................................................
#compute the betas for portfolio assets, using S&P500 as a market index
data.sp500 <- rfin.returns.get(c("^GSPC"), from="2010-01-01")
colnames(data.sp500$returns) <- "sp500"
returns.all <- merge(data$returns, data.sp500$returns)
names <- colnames(returns.all)[colnames(returns.all) != "sp500"]
betas <- lm(names~sp500, data=returns.all)





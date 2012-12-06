rfinance
========

> data <- rfin.returns.get(symbols="ba,msft,sbux,aapl", from="2010-01-01")
> estimates <- rfin.returns.estimate(data$returns, type="annual")

> portfolio3.5 <- rfin.portfolio.create(estimates, risk.free.rate=0.03, desired.return=0.05)
> portfolio3.5$weights
> rfin.portfolio.plot(portfolio3.5)
rfinance
========

## Returns plotting, estimation, etc.

Get prices and returns, plot the returns:
```r
data <- rfin.returns.get(symbols="ba,msft,sbux,aapl", from="2010-01-01")
rfin.returns.plot(data$returns)
```

Calculate 5% VaR (and its error) for $30M investment in MSFT:
```r
VaR <- rfin.returns.VaR(data$returns[,"msft"], p=0.05, wealth=30)
```
VaR$value
[1] -3.073379
VaR$error
[1] 0.4703777
```

Estimate the returns:
```r
estimates <- rfin.returns.estimate(data$returns, type="annual")
```
```
estimates$assets
            mean mean.error        sd   sd.error        var  var.error
ba   0.093330016  0.1274434 0.2176510 0.02601427 0.04737196 0.01132406
msft 0.007521374  0.1340554 0.2289432 0.02736395 0.05241501 0.01252958
sbux 0.324034382  0.1388951 0.2372085 0.02835184 0.05626786 0.01345059
aapl 0.362029000  0.1384465 0.2364424 0.02826027 0.05590499 0.01336385
```

## Portfolio estimation

Create and plot an efficient portfolio with 30% annual return, print out its weights:
```r
portfolio30 <- rfin.portfolio.create(estimates, risk.free.return=0.03, desired.return=0.3)
rfin.portfolio.plot(portfolio30)
portfolio30$weights
```
```
       aapl          ba        msft        sbux 
 0.50717399 -0.03316618  0.16099132  0.36500087 
```

rfin.returns.get <- function(symbols = "msft,goog", from="2005-09-01", to=Sys.Date()) {
	#..........................................................
	#
	#  Returns continuously compounded monthly returns and
	#  adjusted closing prices for the given list of instruments.
	#
	#..........................................................
	library(tseries)
	
	split <- function (text, delimiter = ',') { 
		values <- strsplit(text, delimiter)
		unlist(lapply(values, function(x) { sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE) }))
	}
	symbols <- split(symbols)

	prices = NA
	for (s in symbols) {
		quote <- get.hist.quote(instrument=s, start=from, end=to, quote="AdjClose", provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")
		index(quote) <- as.yearmon(index(quote))
		colnames(quote) <- s
		if (s == symbols[1]) {
			prices <- quote
		}
		else {
			prices <- merge(prices, quote)
		}
	}

	returns <- diff(log(prices))

	#replace NA with 0
	dates <- index(returns)
	symbols <- colnames(returns)
	returns <- coredata(returns)
	returns[is.na(returns)] <- 0
	returns <- zoo(returns)
	index(returns) <- dates
	colnames(returns) <- symbols

	list (
		returns = returns,
		prices = prices
	)
}

rfin.returns.plot <- function(returns, col="blue", title="CC returns") {
	#..........................................................
	#
	#  Plots the returns.
	#
	#..........................................................
	horizontal.line.panel <- function(...) {
	  lines(...)
	  abline(h=0, col="grey")
	}
	plot(returns, lwd=2, panel=horizontal.line.panel, col=col, main=title, xlab="")
}

rfin.returns.VaR <- function(ccreturns, p=0.05, wealth=1) {
	#..........................................................
	#
	#  Computes value at risk for the given investment in the asset: with the given
	#  probability 'p' we would lose 'VaR' (or more!) fraction of the inital wealth.
	#
	#  We asssume normal distribution of the continuosly compounded returns, so VaR is 
	#  calculated as a quantile of a standard normal. However, since we rely on the 
	#  estimated values, we also report estimation error.
	#
	#..........................................................
	library(boot)
	var.bootstrap <- function(data, idx) {
		q <- mean(data[idx]) + sd(data[idx])*qnorm(p)
		exp(q) - 1
	}
	result <- boot(data=ccreturns, statistic=var.bootstrap, R=999)

	list (
		value = result$t0*wealth,
		error = sd(as.vector(result$t))*wealth
	)
}
 
rfin.returns.estimate <- function(ccreturns, type=c("monthly", "annual")) {
	#..........................................................
	#
	#  Computes the Constant Expected Return estimates 
	#  for the given list of continuously compounded returns.
	#
	#..........................................................
	returns.covar <- var(ccreturns)
	returns.corr <- cor(ccreturns)

        hat.mean <- apply(ccreturns, 2, mean)
	hat.sd = apply(ccreturns, 2, sd)
	hat.var = apply(ccreturns, 2, var)
	hat.covar <- returns.covar[lower.tri(returns.covar)]
	hat.corr <- returns.corr[lower.tri(returns.corr)]

	nobs <- nrow(ccreturns)
	hat.mean.error <- hat.sd/sqrt(nobs)
	hat.sd.error <- hat.sd/sqrt(2*nobs)
	hat.var.error <- hat.var/sqrt(nobs/2)
	hat.corr.error = (1-hat.corr^2)/sqrt(nobs)
	hat.covar.error = NA  #no easy formula??

	if (type == "monthly") {
		assets <- zoo(cbind(hat.mean, hat.mean.error, hat.sd, hat.sd.error, hat.var, hat.var.error))
		result <- list(
			assets = assets,
			corr = list(value=hat.corr, error = hat.corr.error, matrix=returns.corr),
			covar = list(value=hat.covar, error = hat.covar.error, matrix=returns.covar)	
		)
	}
	else if (type == "annual") {
		assets <- zoo(cbind(hat.mean*12, hat.mean.error*12, hat.sd*sqrt(12), hat.sd.error*sqrt(12), hat.var*12, hat.var.error*12))
		result <- list(
			assets = assets,
			corr = list(value=hat.corr, error = hat.corr.error, matrix=returns.corr),
			covar = list(value=hat.covar*12, error = hat.covar.error*12, matrix=returns.covar*12)	
		)
	}
	else {
		result <- NA
	}

	if (length(result) > 1) {
		index(result$assets) <- rownames(result$assets)
		colnames(result$assets) <- c("mean", "mean.error", "sd", "sd.error", "var", "var.error")
	}

	result
}

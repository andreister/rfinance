rfin.returns.get <- function(symbol = c("msft", "goog"), from="2005-09-01", to=Sys.Date()) {
	#..........................................................
	#
	#  Returns continuously compounded monthly returns and
	#  adjusted closing prices for the given list of instruments.
	#
	#..........................................................
	library(tseries)

	prices = NA
	for (s in symbol) {
		quote <- get.hist.quote(instrument=s, start=from, end=to, quote="AdjClose", provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")
		index(quote) <- as.yearmon(index(quote))
		colnames(quote) <- s
		if (s == symbol[1]) {
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

rfin.returns.estimate <- function(returns, type=c("monthly", "annual")) {
	#..........................................................
	#
	#  Computes the Constant Expected Return estimates 
	#  for the given list of continuously compounded returns.
	#
	#..........................................................
	returns.covar <- var(returns)
	returns.corr <- cor(returns)

        hat.mean <- apply(returns, 2, mean)
	hat.sd = apply(returns, 2, sd)
	hat.var = apply(returns, 2, var)
	hat.covar <- returns.covar[lower.tri(returns.covar)]
	hat.corr <- returns.corr[lower.tri(returns.corr)]

	nobs <- nrow(returns)
	hat.mean.error <- hat.sd/sqrt(nobs)
	hat.sd.error <- hat.sd/sqrt(2*nobs)
	hat.var.error <- hat.var/sqrt(nobs/2)
	hat.corr.error = (1-hat.corr^2)/sqrt(nobs)
	hat.covar.error = NA

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

fin.price.summary <- function(symbol = c("msft", "goog"), from="2005-09-01", to=Sys.Date()) {
	#..........................................................
	#
	#  Returns adjusted closing prices and continuously compounded monthly returns
	#  for the given list of instruments.
	#
	#..........................................................
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
		prices = prices,
		returns = returns
	)
}

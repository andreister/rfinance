rfin.portfolio.create <- function(estimations, type=c("min.variance", "tangency"), risk.free.rate = 0.03, weights=NA, desired.return=NA) {
	#..........................................................
	#
	#  Creates a portfolio for the given CER estimations.
	#
	#..........................................................
	expected.returns <- estimations$assets[,"mean"]
	expected.covariance <- estimations$covar$matrix
	
	if (is.na(weights)) {
		weights <- rfin.portfolio.getWeights(expected.returns, expected.covariance, type, risk.free.rate, desired.return)
	}

	return <- as.numeric(weights %*% expected.returns)
	standard.deviation <- as.numeric(sqrt(t(weights) %*% expected.covariance %*% weights))
       	sharpe.ratio <- as.numeric((return - risk.free.rate)/standard.deviation)

	list (
		weights = weights, 
		risk = standard.deviation, 
		return = return, 
		sharpe.ratio = sharpe.ratio,
		input = estimations
	)
}

rfin.portfolio.plot <- function(portfolio, risk.free.rate=0.03, grid=list(from=-0.5, to=1.5, length=30)) {
	#..........................................................
	#
	#  Plots risk/return of the given portfolio along with efficient frontier.
	#
	#..........................................................
	efficient.frontier = rfin.portfolio.getEfficientFrontier(portfolio$input, risk.free.rate, grid)

     	y.lim = c(0,max(efficient.frontier$returns, portfolio$return))
	x.lim = c(0,max(efficient.frontier$risks, portfolio$risk))
     	plot(efficient.frontier$risks, efficient.frontier$returns, type="b", xlim=x.lim, ylim=y.lim, xlab=expression(sigma[p]), ylab=expression(mu[p]),  main="Portfolio and Efficient Frontier", col="orange", lwd=2)
	points(portfolio$risk, portfolio$return, type="b", col="blue", pch=20)
}

rfin.portfolio.getEfficientFrontier <- function(estimations, risk.free.rate, grid) {
	#..........................................................
	#
	#  Returns efficient frontier for the given estimated asset returns.
	#
	#  Employs the fact that any portfolio on the efficient frontier can be presented
	#  as a convex combinations of two efficient portfolios (for those we use "global
	#  min variance" portfolio and a "min variance with a fixed return" portfolio).
	#
	#..........................................................
	expected.returns <- estimations$assets[,"mean"]
	portfolio.min.variance <- list(
		global = rfin.portfolio.create(estimations, type="min.variance"),
		max.return = rfin.portfolio.create(estimations, type="min.variance", risk.free.rate=risk.free.rate, desired.return=max(expected.returns))
	)

	# combinations of the two above portfolios would create efficient frontier
	alpha <- seq(from=grid$from, to=grid$to, length=grid$length)			
  	weights <- alpha %*% t(portfolio.min.variance$global$weights) + (1-alpha) %*% t(portfolio.min.variance$max.return$weights)

  	returns <- as.vector(weights %*% expected.returns)			# expected returns of efficient portfolios
  	covar.matrix <- weights %*% estimations$covar$matrix %*% t(weights) 	# cov mat of efficient portfolios
  	risks <- as.vector(sqrt(diag(covar.matrix)))				# std devs of efficient portfolios

	portfolio.names <- paste(rep("P", grid$length), seq(1, grid$length))
  	names(risks) <- portfolio.names
  	names(returns) <- portfolio.names
  	dimnames(weights) <- list(portfolio.names, rownames(estimations$assets))
  	list (
		weights = weights,
		risks = risks,
		returns = returns	
	)
}

rfin.portfolio.getWeights <- function(expected.returns, covar.matrix, type=c("min.variance", "tangency"), risk.free.rate, desired.return = NA) {
	#..........................................................
	#
	#  Computes the weights for a portfolio.
	#
	#..........................................................
	result = NA

	assets.count <- length(expected.returns)
	ones <- rep(1, assets.count)
	zeroes <- rep(0, assets.count)
	if (!is.na(desired.return)) {		
		#  We have a risk minimization problem with two constraints: portfolio weights must add up to 1,
		#  and target return must equal to "desired.return". Now, to find the weights vector [w] we minimize
		#  portfolio variance function [w'*cov.matrix*w] subject to the above two constraints. 
		#
		#  We set up the Lagrangian [L = w'*cov*w -l1(w'*1 - 1) - l2(w'*expected.returns - desired.return)] 
		#  take the derivatives and set them to zero. In a matrix form, this will be A*x = b 
		#	| 2*[cov]             [expected.returns]   1 |   | [w] |   |     [0]         |   
		#       | [expected.returns]' 0                    0 | * |  l1 | = |  desired.return |
		#	| [1]'                0                    0 |   |  l2 |   |      1          |
		#  which gives "x = A(^-1)*b", with [weights] vector being all but the last element 
		A <- rbind(
			cbind(2*coredata(covar.matrix), coredata(expected.returns), ones),
			cbind(rbind(coredata(expected.returns), ones), matrix(0,2,2))	
		)
		b <- as.matrix(c(zeroes, desired.return, 1))
  		x <- solve(A, b)
		result <- x[1:assets.count]
	}	
	else {
		if (type == "min.variance") {
			#  Again, risk minimization problem. To find the weights vector [w] we minimize
			#  portfolio variance function [w'*cov.matrix*w] subject to [w'*1.vector = 1] constraint. 
			#
			#  Set up the Lagrangian [L = w'*cov*w -l(w'*1 - 1)] take derivatives dL/dw and dL\dl and 
			#  set them to zero. In a matrix form, this will be A*x = b 
			#	| 2*[cov]  [1] | * | [w] | = | [0] |
			#	| [1]'      0  |   |  l  |   |  1  |
			#  which gives "x = A(^-1)*b", with [weights] vector being all but the last element 
			#  of [x] (the last element is the Lagrange multiplier).
        		A <- rbind(
				cbind(2*covar.matrix, ones),
				c(ones, 0)
			)
			b <- as.matrix(c(zeroes, 1))
			x <- solve(A, b)
			result <- x[1:assets.count]       
	
        	        #alternatively, we can solve the above matrix equation row-by-row, and get an
			#analytic solution x=cov^(-1)*[1]/[1]*cov^(-1)*[1] which turns to a one-liner:
			#result <- as.vector(rowSums(covar.inv) / sum(covar.inv))
		}
		else if (type == "tangency") {
			covar.inv <- solve(covar.matrix)
  			weights <- covar.inv %*% (expected.returns - risk.free.rate) 
			result <- as.vector(weights/sum(weights))	# normalize weights
		}
	}

	names(result) = names(expected.returns)
	result
}

rfin.portfolio.suggest <- function(portfolio, desired.risk, risk.free.rate = 0.03) {
	#..........................................................
	#
	#  Suggests an efficient (min variance) portfolio for the given level of risk.
	#
	#  Will be some combination of the tangency portfolio and risk-free investment.
	#
	#..........................................................
	risk.investment = coredata(desired.risk/portfolio$risk)  #volume of risk investment
	return = risk.free.rate + risk.investment*(portfolio$return - risk.free.rate)
	list (
		risk.free.investment = 1 - risk.investment,
		risk = desired.risk,
		return = coredata(return)
	)
}

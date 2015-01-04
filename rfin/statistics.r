rfin.statistics.fit <- function (x, y, model=c('linear', 'exponential', 'logistic'), plot=FALSE) {
  #..........................................................
  #
  #  Fits a linear/exponential/logistic model into given data.
  #
  #..........................................................
  xlab = deparse(substitute(x))
  ylab = deparse(substitute(y))
                 
  x <- as.numeric(x)
  y <- as.numeric(y)
  y[y == 0] <- 1e-09
  
  a <- 0
  b <- 0
  r.squared <- 0
  
  switch(model,
    'linear'={
      estimate <- summary(lm(y ~ x))
      a <- estimate$coef[1]        #intercept
      b <- estimate$coef[2]        #slope
      r.squared <- estimate$r.squared
      f <- function(x) {
        a + b*x
      }
      
      result <- list(a = a, b = b, r.squared = r.squared)
    },
    'exponential'={
      estimate <- summary(lm(log(y) ~ x))
      a <- exp(estimate$coef[1])   #intercept
      b <- exp(estimate$coef[2])   #slope
      r.squared <- estimate$r.squared
      
      f <- function(x) {
        a * (b^x)
      }
      
      result <- list(a = a, b = b, r.squared = r.squared)
    },
    'logistic'={
      estimate.nls <- nls(y ~ SSlogis(x, Asym, xmid, scal))
      estimate <- summary(estimate.nls)
      C <- estimate$coef[1]
      b <- exp(1/estimate$coef[3])
      a <- exp((estimate$coef[2]) * (1/estimate$coef[3]))
            
      error.total <- sum( (y - mean(y))^2 )
      error.estimate <- sum( estimate$residuals^2 )
      r.squared<- 1 - error.estimate/error.total
      
      f <- function(x) {
        C / (1 + a*b^(-x))
      }
      result <- list(C = C, a = a, b = b, r.squared = r.squared)
    },
    {
      stop(cat("Unexpected model: ", model))
    }
  )
  
  if (plot) {
    x.seq <- seq(min(x), max(x), length = 100)
    plot(x, y, main = model, pch = 16, xlab = xlab, ylab = ylab)
    lines(x.seq, f(x.seq))
  }    
  
  return (result)
}
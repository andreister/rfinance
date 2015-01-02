rfin.statistics.fit <- function (x, y, model=c('linear', 'exponential', 'logistic'), plot=FALSE) {
  #..........................................................
  #
  #  Fits a linear/exponential/logistic model into given data.
  #
  #..........................................................
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
        a + b*x.seq
      }
    },
    'exponential'={
      estimate <- summary(lm(log(y) ~ x))
      a <- exp(estimate$coef[1])   #intercept
      b <- exp(estimate$coef[2])   #slope
      r.squared <- estimate$r.squared
      
      f <- function(x) {
        a * (b^x.seq)
      }
    },
    'logistic'={
      
    }
  )
  
  if (plot) {
    x.seq <- seq(min(x), max(x), length = 100)
    plot(x, y, main = model, pch = 16, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
    lines(x.seq, f(x.seq))
  }    
  
  #cat(" a = ", round(a, 5), "\n", "b = ", round(b, 5), "\n", "R-squared = ", round(r.squared, 5))
  return (list(a = a, b = b, r.squared = r.squared))
}
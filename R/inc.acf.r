inc.acf <- function(x, minlag.n, maxlag.n, minlag.acf, maxlag.acf){

  x.ts <- ts(data = x, start = 1, end = length(x), frequency = 1)
  x.acf <- acf(x.ts, plot = FALSE)
  x.acf <- data.frame(lag = x.acf$lag, acf = x.acf$acf)
  # head(x.acf)
  
  # Theoretical ACF function for FMD:
  b <- minlag.acf
  m <- -(minlag.acf - maxlag.acf) / (maxlag.n - minlag.n)
  x.acf$tacf <- (m * x.acf$lag) + b
  head(x.acf)
  
  # Residuals:
  res <- sum((x.acf$tacf - x.acf$acf)^2)
  call <- data.frame(minlag.n = minlag.n, maxlag.n = maxlag.n, minlag.acf = minlag.acf, maxlag.acf = maxlag.acf)
  
  rval.ls <- list(acf = x.acf, residual = res, call = call)
  return(rval.ls)
}

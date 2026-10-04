inc.breaks <- function(lower, upper, nbreaks, digits = 3) {
  
  lower.log2 <- log2(lower)
  upper.log2 <- log2(upper)
  
  # Ensure min is negative and max is positive
  if (lower.log2 > 0 || upper.log2 < 0) {
    stop("Lower must be less than 1 and upper must be greater than 1")
  }
  
  # Define sequence break size:
  step <- (upper.log2 - lower.log2) / (nbreaks - 1)
  
  # Generate sequence:
  brk <- seq(from = lower.log2, to = upper.log2, by = step)
  lab <- round(2^seq(from = lower.log2, to = upper.log2, by = step), digits =  digits)
  
  rval.ls <- list(limits = range(brk), breaks = brk, labels = lab) 
  return(rval.ls)
}

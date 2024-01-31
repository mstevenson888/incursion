inc.ffi <- function(ips, par, start, period = 14, conf.level = 0.95){
   
   # Infection date: ips[,6]
   # Onset of clinical signs date: ips[,7]
   # Visit date: ips[,8]
   end <- start + period
  
   # Numerator:
   id <- ips[,6] >= start & ips[,6] < end
   ips.tmp <- ips[id,]
   num <- nrow(ips.tmp)
   
   # Denominator is comprised of all farms that were present on the start date:
   id <- par$slgdate >= start | is.na(par$slgdate)
   den <- nrow(par[id,])
   
   # Calculate first "fortnight" incidence:
   # Wilson's method (see Rothman, Epidemiology An Introduction, page 132): 
   N. <- 1 - ((1 - conf.level) / 2)
   z <- qnorm(N., mean = 0, sd = 1)
   
   a <- num
   n <- den
   p <- a / n
        
   a. <- n/(n + z^2)
   b. <- a/n
   c. <- z^2/(2 * n)
   d. <- (a * (n - a)) / n^3
   e. <- z^2 / (4 * n^2)
        
   low <- a. * (b. + c. - (z * sqrt(d. + e.)))
   up <- a. * (b. + c. + (z * sqrt(d. + e.)))
  
   rval <- data.frame(est = p, lower = low, upper = up)
   rval
}

# load("C:\\TEMP\\incursion\\data\\inc.nzfmd.RData")
# data(inc.nzfmd)
# ips <- inc.nzfmd$ips[,c(1:5,8:10)]
# par <- inc.nzfmd$par[,c(1:5)]
# 
# ## Incidence risk for the 14 days following 10 Mar 2001: 
# res <- inc.ffi(ips = ips, par = par, start = as.Date("2001-03-10", format = "%Y-%m-%d"), period = 14, conf.level = 0.95)
# round(res * 100, digits = 2)
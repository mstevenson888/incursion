inc.selfsurv <- function(ips, start, period = 14, conf.level = 0.95){

   N. <- 1 - ((1 - conf.level) / 2)
   z <- qnorm(N., mean = 0, sd = 1)

   end <- start + period
      
   # Select IPs with signs date within specified date range:
   id <- ips[,5] >= start &  ips[,5] < end
   ips <- ips[id,]

   # Denominator: the number of IPs with onset of signs date within the period specified:
   n <- nrow(ips)
      
   # Numerator: number of public reports:
   r <- subset(ips, ips[,4] == 1)
   r <- nrow(r)
   
   p <- r/n
   low <- ifelse(r > 0, 
           ((qchisq(N., df = 2 * r, lower.tail = FALSE) / 2) / n), 0)
   up <- ifelse(r > 0, 
       ((qchisq(1 - N., df = 2 * (r + 1), lower.tail = FALSE) / 2) / n), 
       ((qchisq(1 - N., df = 2, lower.tail = FALSE) / 2) / n))
        
    rval <- data.frame(est = p, lower = low, upper = up)
    rval
}
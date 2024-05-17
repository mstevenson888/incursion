inc.ripratio <- function(ips, tra, start, period = 14, conf.level = 0.95){
   
   N. <- 1 - ((1 - conf.level) / 2)  
   z <- qnorm(N., mean = 0, sd = 1)

   end <- start + period
      
   # Numerator: the number of premises exposed to direct or indirect movements of identified IPs (before and after onset of signs date):
   id <- ips[,1] %in% tra[,1] 
   r <- length(ips[id,1])
   
   # Denominator: the number of IPs with onset of signs date for the period specified:
   n <- subset(ips, ips[,4] >= start & ips[,4] < end)
   n <- length(n[,1])

   p <- r/n
   low <- ifelse(r > 0, 
           ((qchisq(N., df = 2 * r, lower.tail = FALSE) / 2) / n), 0)
   up <- ifelse(r > 0, 
       ((qchisq(1 - N., df = 2 * (r + 1), lower.tail = FALSE) / 2) / n), 
       ((qchisq(1 - N., df = 2, lower.tail = FALSE) / 2) / n))
        
    rval <- data.frame(est = p, lower = low, upper = up)
    rval
}
inc.trace <- function(m, b, mlage, edate, det.dis, buffer, conf.level = 0.95){
  
  lbound <- (1 - conf.level) / 2
  mbound <- 0.500
  ubound <- 1 - lbound
  
  logm <- log(m)
  logsd <- (log(b) - log(m)) / 3
  
  lnorm.low <- qlnorm(p = lbound, meanlog = logm, sdlog = logsd)
  lnorm.med <- qlnorm(p = mbound, meanlog = logm, sdlog = logsd)
  lnorm.upp <- qlnorm(p = ubound, meanlog = logm, sdlog = logsd)
  
  btrace.s <- edate - mlage - lnorm.upp - buffer
  btrace.e <- edate - mlage - lnorm.low
  btrace <- data.frame(start = btrace.s, end = btrace.e)
  
  ftrace.s <- edate - mlage - lnorm.upp - buffer 
  ftrace.e <- edate + det.dis
  ftrace <- data.frame(start = ftrace.s, end = ftrace.e)
  
  lnorm <- data.frame(meanlog = logm, sdlog = logsd)
  
  rval.ls <- list(btrace = btrace, ftrace = ftrace, lnorm = lnorm) 
  return(rval.ls)
}   


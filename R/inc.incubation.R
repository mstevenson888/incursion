inc.incubation <- function(ips, meanlog = log(7), sdlog = log(1.5)){
  
  ip <- -99
  inf <- as.Date("1901-01-01", format = "%Y-%m-%d") 
  inc <- -99
  infest <- -99
  incest <- -99

  for(i in 1:length(ips[,1])){
       
       # Infection date present, signsdate absent, species age absent:
       if(!is.na(ips[i,4]) & is.na(ips[i,5]) & sum(ips[i,7:12]) == 0){
         
         # Generate an incubation period:
         rval.inc <- round(rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog), digits = 0)
         
         # Use the recorded infection date:
         rval.inf <- ips[i,4]
         
         # Flag to indicate estimated values (0 = true, 1 = estimated):
         rval.incest <- 1; rval.infest <- 0
         }
   
       # Infection date absent, signsdate present, species age absent:
       if(is.na(ips[i,4]) & !is.na(ips[i,5]) & sum(ips[i,7:12]) == 0){
         
         # Generate an incubation period:
         rval.inc <- round(rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog), digits = 0)
         
         # Estimated infection date = signsdate - incubation period:
         rval.inf <- ips[i,4] - rval.inc
         
         # Flag to indicate estimated values (0 = true, 1 = estimated):
         rval.incest <- 1; rval.infest <- 1
         }
       
       # Infection date absent, signsdate absent, species age present:
       if(is.na(ips[i,4]) & is.na(ips[i,5]) & sum(ips[i,7:12]) > 0){
         
         # Generate an incubation period:
         rval.inc <- round(rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog), digits = 0)
         
         # Estimated infection date = visitdate - (max of signsage) - incubation period:
         rval.inf <- ips[i,6] - max(ips[i,7:12]) - rval.inc
         
         # Flag to indicate estimated values (0 = true, 1 = estimated):
         rval.incest <- 1; rval.infest <- 1 
         }
   
       # Infection date present, signsdate present, species age absent:
       if(!is.na(ips[i,4]) & !is.na(ips[i,5]) & sum(ips[i,7:12]) == 0){
         
         # Generate an incubation period:
         rval.inc <- ips[i,5] - ips[i,4]
         
         # Use actual infection date:
         rval.inf <- ips[i,4]
         
         # Flag to indicate estimated values (0 = true, 1 = estimated):
         rval.incest <- 0; rval.infest <- 0 
         }
   
       # Infection date absent, signsdate present, species age present:
       if(is.na(ips[i,4]) & !is.na(ips[i,5]) & sum(ips[i,7:12]) > 0){
         
         # Generate an incubation period:
         rval.inc <- round(rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog), digits = 0)
         
         # Estimated infection date = signsdate - estimated incubation period:
         rval.inf <- ips[i,5] - rval.inc
         
         # Flag to indicate estimated values (0 = true, 1 = estimated):
         rval.incest <- 1; rval.infest <- 1 
       }
   
       # Infection date present, signsdate present, species age present:
       if(!is.na(ips[i,4]) & !is.na(ips[i,5]) & sum(ips[i,7:12]) > 0){
         
         # Calculate incubation period:
         rval.inc <- ips[i,5] - ips[i,4]
         
         # Use actual infection date:
         rval.inf <- ips[i,4]
         
         # Flag to indicate estimated values (0 = true, 1 = estimated):
         rval.incest <- 0; rval.infest <- 0 
       }
  
  ip <- c(ip, ips[i,1])
  inf <- c(inf, rval.inf)
  infest <- c(infest, rval.infest)
  inc <- c(inc, rval.inc)
  incest <- c(incest, rval.incest)  
 } 

ip <- ip[-1]
inf <- as.Date(inf[-1], format = "%Y-%m-%d")
infest <- infest[-1]
inc <- inc[-1]
incest <- incest[-1]

rval <- data.frame(ip, infest, inc, incest)
rval <- data.frame(inf, rval)
rval <- rval[,c(2,1,3,4,5)]
names(rval) <- c("ip","inf.date", "inf.est", "inc", "inc.est")
rval 
} 
 
# load("C:\\TEMP\\incursion\\data\\inc.nzfmd.RData")
# ips <- inc.nzfmd$ips[,c(1:3,8:10,13,16,19,22,25,28)]
# fmd.inc <- inc.incubation(ips, meanlog = log(7), sdlog = log(1.5)) 
  

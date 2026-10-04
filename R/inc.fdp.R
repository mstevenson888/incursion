inc.fdp <- function(ips, use = "species", conf.level = 0.95){
   
   if(ncol(ips) == 21){  
   spp.pop <- as.matrix(ips[,c(4,7,10,13,16,19)])
   spp.ini <- as.matrix(ips[,c(5,8,11,14,17,20)])
   spp.age <- as.matrix(ips[,c(6,9,12,15,18,21)])
   frm.pop <- apply(spp.pop, MARGIN = 1, FUN = sum)
   
   rval.spp <- c(); rval.pop <- c(); rval.ini <- c()
      
   # Which species had the oldest signs on each farm?
   for(i in 1:nrow(spp.age)){
      tmp.age <- matrix(spp.age[i,], nrow = 1)
      oldest <- max(tmp.age)
      
      # Sometimes will be two or more species with the oldest lesions. Pick one at random:
      tmp.spp <- col(tmp.age)[tmp.age == oldest]
      pick <- sample(x = 1:length(tmp.spp), size = 1)
      tmp.spp <- tmp.spp[pick]
      
      # Return number in group and number initially affected for the species with the oldest clinical signs:
      tmp.pop <- as.vector(spp.pop[i,tmp.spp])
      tmp.ini <- as.vector(spp.ini[i,tmp.spp])

      rval.spp <- c(rval.spp, tmp.spp)
      rval.pop <- c(rval.pop, tmp.pop)
      rval.ini <- c(rval.ini, tmp.ini)
   }

   # Calculate first day incidence:
   # Wilson's method (see Rothman, Epidemiology An Introduction, page 132): 
   
   if(use == "all"){
   # Option 1: All animals on infected place included in the denominator:
   N. <- 1 - ((1 - conf.level) / 2)
   z <- qnorm(N., mean = 0, sd = 1)
   
   a <- rval.ini
   n <- frm.pop
   p <- a / n
   b <- n - a
          
   # Exact method (see http://www.folkesundhed.au.dk/uddannelse/software):
   a. <- ifelse(a == 0, a + 1, a)
   b. <- ifelse(b == 0, b + 1, b) 
   low <- a. /(a. + (b. + 1) * (1 / qf(1 - N., 2 * a., 2 * b. + 2)))
   up <- (a. + 1) / (a. + 1 + b. / (1 / qf(1 - N., 2 * b., 2 * a. + 2)))
   low <- ifelse(a == 0, 0, low)
   up <- ifelse(a == n, 1, up)

   rval <- data.frame(placeid = ips[,1], ipid = ips[,3], ncas = a, natrisk = n, est = p, lower = low, upper = up)
   
   id <- !is.na(rval$est)
   rval <- rval[id,]
   }

   else if(use == "species"){
   # Option 2: Only all animals on infected place included in the denominator:

   # Calculate first day incidence:
   N. <- 1 - ((1 - conf.level) / 2)
   z <- qnorm(N., mean = 0, sd = 1)
   
   a <- rval.ini
   n <- rval.pop
   p <- a / n
   b <- n - a
         
   # Exact binomial confidence limits (D. Collett (1999): Modelling binary data. Chapman & Hall/CRC, Boca Raton Florida, p. 24).
   a. <- ifelse(a == 0, a + 1, a)
   b. <- ifelse(b == 0, b + 1, b) 
   low <- a. /(a. + (b. + 1) * (1 / qf(1 - N., 2 * a., 2 * b. + 2)))
   up <- (a. + 1) / (a. + 1 + b. / (1 / qf(1 - N., 2 * b., 2 * a. + 2)))
   low <- ifelse(a == 0, 0, low)
   up <- ifelse(a == n, 1, up)
   
   rval <- data.frame(placeid = ips[,1], ipid = ips[,3], ncas = a, natrisk = n, est = p, lower = low, upper = up) 
      
   id <- !is.na(rval$est)
   rval <- rval[id,]
   }
  }

   if(ncol(ips) == 6){  
    spp.pop <- ips[,4]
    spp.ini <- ips[,5]
    spp.age <- ips[,6]

    # Calculate first day incidence:
    N. <- 1 - ((1 - conf.level) / 2)
    z <- qnorm(N., mean = 0, sd = 1)
      
    a <- spp.ini
    n <- spp.pop
    p <- a / n
    b <- n - a
         
    # Exact binomial confidence limits (D. Collett (1999): Modelling binary data. Chapman & Hall/CRC, Boca Raton Florida, p. 24).
    a. <- ifelse(a == 0, a + 1, a)
    b. <- ifelse(b == 0, b + 1, b) 
    low <- a. /(a. + (b. + 1) * (1 / qf(1 - N., 2 * a., 2 * b. + 2)))
    up <- (a. + 1) / (a. + 1 + b. / (1 / qf(1 - N., 2 * b., 2 * a. + 2)))
    low <- ifelse(a == 0, 0, low)
    up <- ifelse(a == n, 1, up)
   
    rval <- data.frame(placeid = ips[,1], ipid = ips[,3], ncas = a, natrisk = n, est = p, lower = low, upper = up) 
  
    id <- !is.na(rval$est)
    rval <- rval[id,]
    }
   rval
}

# load("C:\\TEMP\\incursion\\data\\inc.nzfmd.RData")
# ips <- inc.nzfmd$ips
# ips.spp <- inc.nzfmd$ips[,c(1:3,11:28)]
# 
# rval.df <- inc.fdp(ips.spp, use = "species", conf.level = 0.95)
# rval.df$rank <- rank(rval.df$est, ties.method	= "random")
# rval.df <- rval.df[sort.list(rval.df$rank),]

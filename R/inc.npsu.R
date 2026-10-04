inc.npsu <- function (nssu, npsu = NA, psumin, psumode, psumax){
  
  ssu.per.psu <- c()
  tnpsu <- 0
  
  if(is.na(npsu)){
    while (sum(ssu.per.psu) < nssu) {
      tssu.per.psu <- round(mc2d::rpert(n = 1, min = psumin, mode = psumode, max = psumax, shape = 4), digits = 0)
      ssu.per.psu <- c(ssu.per.psu, tssu.per.psu)
      tnpsu <- length(ssu.per.psu)
    }    
  }
  
  if(!is.na(npsu)){
    # Generate PERT-distributed raw SSUs:
    ssu.per.psu <- mc2d::rpert(n = npsu, min = psumin, mode = psumode, max = psumax, shape = 4)
    
    # Scale to match total nssu:
    ssu.per.psu <- round(ssu.per.psu / sum(ssu.per.psu) * nssu)
    tnpsu <- length(ssu.per.psu)
  }

  rval.ls <- list(npsu = tnpsu, ssu.per.psu = ssu.per.psu)
  return(rval.ls)
}
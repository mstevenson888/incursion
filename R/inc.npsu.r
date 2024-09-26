# Function to estimate the number of cattle herds in a village, given an estimate of the total number of cattle present

# nssu: integer, representing the total number of individuals in the population.
# psumin: integer, minimum herd size.
# psumode: integer, mode herd size.
# psumax: integer, max herd herd size.

# Returns a vector of estimated herd sizes.

inc.npsu <- function(nssu, psumin, psumode, psumax){
  hsize <- c()
  
  # Take a random draw from the herd size distribution to estimate the total number of herds:
  while(sum(hsize) < nssu) {
    thsize <- round(mc2d::rpert(n = 1, min = psumin, mode = psumode, max = psumax, shape = 4), digits = 0); thsize
    # Vector of the number of individuals in each establishment:
    hsize <- c(hsize, thsize)
  }
  rval.ls <- list(npsu = length(hsize), psusize = hsize)
  return(rval.ls)
}

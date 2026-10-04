inc.rmrisk <- function(ritem, spp, ldays, incont, dest) {
  
  # Recycle scalars so any mix of scalar and vector inputs works:
  n      <- max(length(ritem), length(spp), length(ldays), length(incont), length(dest))
  ritem  <- rep_len(ritem,  n)
  spp    <- rep_len(spp,    n)
  ldays   <- rep_len(ldays, n)
  incont <- rep_len(incont, n)
  dest  <- rep_len(dest,  n)
  
  # Species-specific day threshold: cattle 4, sheep/pigs 6 (%in% treats NA safely, so unknown species stay NA):
  threshold <- rep(NA_real_, n)
  threshold[spp %in% "cattle"] <- 4
  threshold[spp %in% c("sheep", "pigs")] <- 6
  
  # Helper: TRUE only where the condition is TRUE (NA becomes FALSE):
  yes <- function(x) !is.na(x) & x
  
  early <- yes(ldays <= threshold)    # within threshold
  late  <- yes(ldays >  threshold)    # beyond threshold
  host  <- ritem  %in% "host"
  item  <- ritem  %in% "object"
  inc   <- yes(incont)
  ninc  <- yes(!incont)
  out   <- dest  %in% "outside"
  wthn  <- dest  %in% "within"
  
  # Start with NA; anything unrecognised stays NA:
  assess <- rep(NA_character_, n)
  
  # Hosts: depends only on days
  assess[host & early] <- "high"
  assess[host & late]  <- "medium"
  
  # Items not in contact: always low
  assess[item & ninc] <- "low"
  
  # Items in contact
  assess[item & inc & early]         <- "high"
  assess[item & inc & late & out]    <- "high"
  assess[item & inc & late & wthn]   <- "medium"
  
  assess
}
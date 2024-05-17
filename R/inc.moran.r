inc.moran <- function(x, listw){
   mtest <- spdep::moran.test(x = x, listw = listw, randomisation = TRUE, zero.policy = TRUE)
   mplot <- spdep:: moran.plot(x = x, listw = listw, zero.policy = TRUE, plot = FALSE, return_df = TRUE)
  
   rval.ls <- list(moran.test = mtest, moran.plot = mplot)
   return(rval.ls)
}
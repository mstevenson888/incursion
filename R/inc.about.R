"inc.about" <- function()
{
  cat("\n")
  cat("----------------------------------------------------------------------------------\n")
  ver <- packageDescription("incursion", lib.loc = NULL, fields = "Version")
  cat(paste("incursion version", ver))
  cat("\n")
  cat("Functions for the Analysis of Infectious Disease Outbreaks in Human and")
  cat("Animal Populations.")
  cat("\n")
  cat("See https://mvs.unimelb.edu.au/research/groups/veterinary-epidemiology-melbourne")
  cat("for details.")
  cat("\n")
  cat("----------------------------------------------------------------------------------\n")
  invisible()
}

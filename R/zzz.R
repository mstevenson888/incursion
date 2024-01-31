.onAttach <- function(libname, pkgname)
{
  ver <- as.character(read.dcf(file.path(libname, pkgname, "DESCRIPTION"), "Version"))
  packageStartupMessage("Package incursion ", ver, " is loaded", appendLF = TRUE)
  packageStartupMessage("Type help(inc.about) for summary information")
  packageStartupMessage("\n")
}

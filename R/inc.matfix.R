inc.matfix <- function(dat){
  dat.dim <- dim(dat)
  nrows <- dat.dim[1]
  rval <- dat[nrows, ]
  for (i in (nrows - 1):1){
    rval <- rbind(rval, dat[i,])
  }
  rval
}

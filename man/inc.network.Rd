\name{inc.network}

\docType{data}

\alias{inc.network}

\title{Data from a hypothetical tracing exercise}

\description{
Data from a hypothetical tracing exercise.}

\usage{data(inc.network)}

\format{
A list comprised of objects \code{edg.dat} and \code{vert.dat}. 

Object \code{edg.dat} provides details of transactions (that is, edges) between vertices in a network. Variables are: \code{srcherd} the identifier of the vertex acting as the source of the transaction, \code{desherd} the identifier of the vertex acting as the destination of the transaction, \code{mdate} the date on which the transaction occurred, and \code{n} an estimated risk score for the transaction.

Object \code{vert.dat} provides details of individual vertices participating in the network. Variables are \code{herd}: a unique vertex identifier, \code{xcoord}: the easting coordinate of the vertex centroid, and \code{ycoord}: the northing coordinate of the vertex centroid.
}

\details{
Variable \code{mdate} in the \code{edg.dat} table is in \code{character} format. Easting and northing coordinates are in NZTM format (EPSG 2193).
}

\examples{
## EXAMPLE 1:
\dontrun{
library(ggplot2); library(sf)

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
nzadm0tm.sf <- st_read(dsn = mdir[1], layer = "nzadm0-NZTM2000")
nzadm2tm.sf <- st_read(dsn = mdir[1], layer = "nzadm2-NZTM2000")

data(inc.network)

ggplot() +
  theme_bw() +
  geom_point(data = inc.network$vert.dat, aes(x = xcoord, y = ycoord), 
     col = "red", pch = 16) +
  geom_sf(data = nzadm2tm.sf, fill = "transparent", colour = "grey") +
  geom_sf(data = nzadm0tm.sf, fill = "transparent", colour = "black") +
  coord_sf(datum = st_crs(nzadm2tm.sf)) +
  scale_x_continuous(name = "Easting (km)", labels = inc.mformat(div = 1000)) +
  scale_y_continuous(name = "Northing (km)", labels = inc.mformat(div = 1000))
}
}
\keyword{datasets}

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
data(inc.network)

plot(x = inc.network$vert.dat$xcoord / 1000, 
   y = inc.network$vert.dat$ycoord / 1000,
   xlab = "Easting (km)", ylab = "Northing (km)", pch = 16)
}

\keyword{datasets}

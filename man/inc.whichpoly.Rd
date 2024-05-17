\name{inc.whichpoly}

\alias{inc.whichpoly}

\title{
In which polygon does a point lie?
}

\description{
Attaches polygon attribute table data to a data frame listing the longitude and latitude of point locations.
}

\usage{
inc.whichpoly(points, poly.sf)
}

\arguments{
  \item{points}{a data frame listing the longitude \code{lon} and latitude \code{lat} of the point location.}
  \item{poly.sf}{an \code{sf} polygon object defining the polygons to be queried.}
}

\details{
This function would usually be used when one wants to define a new farm or herd location to be included in an AADIS herd data set. The farm-herd location attributes required by AADIS include the identifier of the region, state and local government area (LGA) in which the farm-herd is located. This function can be used to query digital maps (e.g., ESRI shape file sets) providing region, state and local government area boundaries. 

Digital maps used for this function must be projected in decimal degress (EPSG 4326).
}

\examples{
\dontrun{
## EXAMPLE 1:
library(sf)

## Create a data frame listing farm-herd locations:
farmsll.df <- data.frame(id = 1, lon = 144.39, lat = -35.97)

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
auadm1ll.sf <- st_read(dsn = mdir[1], layer = "auadm1-LL")

inc.whichpoly(points = farmsll.df, poly.sf = auadm1ll.sf)

## This farm is in Victoria, Australia.
}
}
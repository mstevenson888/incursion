\name{inc.closestpoint}

\alias{inc.closestpoint}

\title{
Which point is closest to a given point location?
}

\description{
Returns the point location identifier of the nearest point to a data frame listing the longitude and latitude of point locations.
}

\usage{
inc.closestpoint(points, locations)
}

\arguments{
  \item{points}{a data frame listing the longitude \code{lon} and latitude \code{lat} of the point location.}
  \item{locations}{a data frame listing the identifier \code{id}, the longitude \code{lon} and latitude \code{lat} of point locations to be queried.}
}

\details{
This function would usually be used when one wants to define a new farm or herd location to be included in an AADIS herd data set. The farm-herd location attributes required by AADIS include (among other things) the identifier of the nearest weather station. This function can be used to query a data frame listing the identity and coordinates of weather station to return the nearest weather station to each location listed in data frame \code{point}. 
}

\examples{
\dontrun{
## EXAMPLE 1:
## Create a data frame listing herd location:
herdll.df <- data.frame(id = 1, lon = 144.39, lat = -35.97)

## AADIS simulation results. Select herd locations for run 1:
data(inc.aufmd)

## Select data from a single simulation run:
id <- inc.aufmd$run == 1
taufmd.df <- inc.aufmd[id,c(21,9,8)]
names(taufmd.df) <- c("id","lon","lat")

## What is the closest AADIS herd (as listed in taufmd.df) to the herd
## of interest?
inc.closestpoint(points = herdll.df, locations = taufmd.df)

## Herd 2 is the closest herd (lon 144.39, lat -35.97).
}
}
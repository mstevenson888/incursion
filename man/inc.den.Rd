\name{inc.den}

\alias{inc.den}

\title{
Compute location and individual (human, animal) density in an area around a defined point location
}

\description{
Compute location and individual (i.e., human, animal) density in an area around a defined point location.
}

\usage{
inc.den(loc, par, dim, shape = "square")
}

\arguments{
  \item{loc}{a vector of length two defining the Cartesian coordinates of the point location of interest.}
  \item{par}{a three-column data frame comprised of three columns listing (in order) the easting coordinate of the premises location, the northing coordinate of the premises location, and the number of individuals present at each premises location.}
  \item{dim}{scalar, defining the diameter of the circle constructed around the point location of interest (where \code{shape = "circle"}) or the length of each side of a square constructed around the point location of interest (where \code{shape = "square"}).}
  \item{shape}{character string defining the shape of the area constructed around the point location of interest. Options are \code{square} or \code{circle}.}
}

\details{
Point location details must be provided as Cartesian coordinates (not latitude and longitude). 
}


\value{
A list containing the following:
  \item{premises}{the number of premises in the area, the size of the area, and premises density.}
  \item{individuals}{the number of individuals in the area, the size of the area, and individual density.}
}

\examples{
## EXAMPLE 1:
## Cartesian coordinates of the point location of interest:
loc <- c(1789917, 5924157)

## Data frame providing details of the point locations of the farm population 
## at risk:
data(inc.nzfmd)
par <- inc.nzfmd$par[,3:4]

# Simulate animal counts for each farm in data frame par:
par$lsu <- round(runif(n = nrow(par), min = 1, max = 1500), digits = 0)

## We want to know the density of farms and the density of animals in a 5 km
## by 5 km box drawn around the point location of interest:
rval <- inc.den(loc = loc, par = par, dim = 5000, shape = "square")
rval

## There are 9 farms in the 5 km by 5 km area. Density is 3.6e-07 farms per
## square metre (densities are reported in metres squared because Cartesian
## coordinates are specified in metres). How many farms per square kilometre?
rval$premises$n * 1000^2 / rval$premise$area

## Answer: 0.36 farms per square kilometre.
}


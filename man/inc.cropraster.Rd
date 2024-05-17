\name{inc.cropraster}

\alias{inc.cropraster}

\title{
Crop a raster object to the boundaries of a polygon vector object
}

\description{
Crop a raster object to the boundaries of a polygon vector object.
}

\usage{
inc.cropraster(raster, sf, snap = "out", type = "raster")
}

\arguments{
  \item{raster}{an object of class \code{raster} to be cropped.}
  \item{sf}{an object of clas \code{sf}, the polygon scalar, the minimum lag to be evaluated.}
  \item{snap}{character, either \code{near}, \code{in} or \code{out}.}
  \item{type}{character, either \code{raster} or \code{data.frame}.}
}

\details{
The function returns either a raster or data frame representing the cropped data. See the examples, below.
}

\value{
Either a raster or data frame, dependent on the setting entered by the user for \code{type}.
}
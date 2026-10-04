\name{inc.cropraster}

\alias{inc.cropraster}

\title{
Crop a raster object to the boundaries of a polygon vector object
}

\description{
Crop a raster object to the boundaries of a polygon vector object.
}

\usage{
inc.cropraster(x, sf, type = "raster")
}

\arguments{
  \item{x}{an object of class \code{SpatRaster} from package \code{terra} or class \code{RasterLayer} from package \code{raster} to be cropped.}
  \item{sf}{an object of class \code{sf} to be used as the mask for cropping.}
  \item{type}{character, output type either \code{raster} or \code{data.frame}.}
}

\details{
This function returns either a raster or data frame representing the cropped data.
}

\value{
Either a raster or data frame, dependent on the setting entered by the user for \code{type}.
}
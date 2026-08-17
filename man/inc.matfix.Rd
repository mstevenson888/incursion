\name{inc.matfix}

\alias{inc.matfix}

\title{
Re-configures a \code{spatstat} \code{im} object for plotting using \code{ggplot2} 
}

\description{
Re-configures a \code{spatstat} \code{im} object for plotting using \code{ggplot2}.
}

\usage{
inc.matfix(dat)
}

\arguments{
  \item{dat}{a \code{spatstat} \code{im} object.}
}

\examples{
## EXAMPLE 1:
\dontrun{
library(ggplot2); library(raster); library(RColorBrewer); library(sf);
library(spatstat)

data(inc.kofmd)

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
koadm2tm.sf <- st_read(dsn = mdir[1], layer = "koadm2-EPSG5174")

## Create an observation window:
kobnd.df <- inc.kofmd$koboundary
kobnd.ow <- owin(poly = list(x = kobnd.df[,1], y = kobnd.df[,2]), 
    unitname = c("metre", "metres"))

## Create ppp objects:
kofarms.df <- inc.kofmd$kofarms
kosimfmd.df <- inc.kofmd$kosimfmd$inf
koactfmd.df <- inc.kofmd$koactfmd

kofarms.ppp <- ppp(x = kofarms.df$xcoord, y = kofarms.df$ycoord, 
   window = kobnd.ow)
kosimfmd.ppp <- ppp(x = kosimfmd.df$xcoord, y = kosimfmd.df$ycoord, 
   window = kobnd.ow)

## Set the bandwidth and grid dimensions:
sigma <- 1200; dimyx = c(200, 200)

## Kernel smooth:
kofarms.im <- density(kofarms.ppp, sigma = sigma, dimyx = dimyx)
kosimfmd.im <- density(kosimfmd.ppp, sigma = sigma, dimyx = dimyx)

## The density.ppp function returns an estimate of the intensity of the 
## underlying point process, that is the expected number of points per unit 
## area (in this case, the number of points per metre). Express results as 
## the number of points per square kilometre (1 square metre = 0.000001 
## square kilometres):
kofarms.im$v <- kofarms.im$v / 0.000001
summary(as.vector(kofarms.im$v))
## Maximum farm density is 6.2 per farms per square kilometre.

## Do the same for the simulations. Note the division by 9, because simulation
## results are for 9 iterations:
kosimfmd.im$v <- kosimfmd.im$v / 0.000001
summary(as.vector(kosimfmd.im$v) / 9)

## Image plot of farm density with superimposed contour plot to identify
## areas where the density of farms predicted to become infected was 
## relatively high:

kosimfmd.r <- raster(inc.matfix(as.matrix(kosimfmd.im$v, nrow = length(xgrid))))
extent(kosimfmd.r) <- st_bbox(koadm2tm.sf)
kosimfmd.rdf <- data.frame(rasterToPoints(kosimfmd.r))

breaks <- seq(from = 0, to = 2.5, by = 0.5)
cols <- brewer.pal(n = 7, name = "Oranges")

ggplot() +
  theme_bw() +
  geom_raster(data = kosimfmd.rdf, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colours = cols, breaks = breaks, 
     name = "Density", limits = c(-0.01,2.5)) + 
  geom_point(data = koactfmd.df, aes(x = xcoord, y = ycoord), 
     shape = 16, colour = "red") +
  geom_sf(data = koadm2tm.sf, fill = "transparent", col = "black") +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") 
 }
}
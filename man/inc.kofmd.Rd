\name{inc.kofmd}

\docType{data}

\alias{inc.kofmd}

\title{Actual and simulated outbreaks data from the 2002 outbreak of foot and mouth disease in South Korea}

\description{
Actual and simulated outbreaks data from the 2002 outbreak of foot and mouth disease in South Korea.}

\usage{data(inc.kofmd)}

\format{
A list comprised of objects \code{kofarms}, \code{kosimfmd}, \code{koactfmd} and \code{koboundary}.

Object \code{kofarms} provides summary information for 3570 livestock farms at risk. Variables are: \code{id}: unique premises identifier, \code{farmid}: secondary unique premises identifier, \code{type}: farm type, \code{cattle}: number of cattle present, \code{other}: number of other FMD susceptible species present, \code{pigs}: number of pigs present, \code{xcoord}: easting coordinate of the premises centroid (EPSG 5174 projection), and \code{ycoord}: northing coordinate of the premises centroid (EPSG 5174 projection).

Object \code{kosimfmd} is a list comprised of the following data frames: \code{dep} depopulated premises, \code{det} premises detected with disease, \code{inf} infected premises and \code{vac} vaccinated premises. Each data frame lists \code{itno} the simulation iteration number; \code{time} simulation day; \code{id} premises identifier; \code{type} premises type; \code{cattle} number of cattle present; \code{other} number of other FMD susceptible species present; \code{pigs} number of pigs present. 

Object \code{koactfmd} provides details of the 18 premises identified as FMD positive during the 2002 outbreak in South Korea. Variables are: \code{itno} a fixed value of 1, \code{time} day of infection relative to 22 April 2002, \code{id} unique infected premises number, \code{xcoord}: easting coordinate of the premises centroid (EPSG 5174 projection), and \code{ycoord}: northing coordinate of the premises centroid (EPSG 5174 projection)

Object \code{koboundary} is a data frame listing the boundaries of the study area.
}

\references{
Yoon H, Wee S, Stevenson M, O'Leary B, Morris R, Hwang I, Park C, Stern M (2006). Simulation analyses to evaluate alternative control strategies for the 2002 foot-and-mouth disease outbreak in the Republic of Korea. Preventive Veterinary Medicine 74, 212 - 225. DOI:  10.1016/j.prevetmed.2005.12.002.
}

\examples{
## EXAMPLE 1:
\dontrun{
library(ggplot2); library(raster); library(RColorBrewer); 
   library(sf); library(spatstat)

data(inc.kofmd)

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
koadm2tm.sf <- st_read(dsn = mdir[1], layer = "koadm2-EPSG5174")

## Create an observation window:
kobnd.df <- inc.kofmd$koboundary
kobnd.ow <- owin(poly = as.matrix(kobnd.df[1:616,]), 
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
   scale_fill_gradientn(colours = cols, breaks = breaks, name = "Density", 
     limits = c(-0.01,2.5)) + 
   stat_contour(data = kosimfmd.rdf, aes(x = x, y = y, z = layer, 
     linetype = c(1,2)), breaks = 0.5, linetype = 1, col = "black") +
   geom_point(data = koactfmd.df, aes(x = xcoord, y = ycoord), 
     shape = 16, colour = "black") +
   geom_sf(data = koadm2tm.sf, fill = "transparent", col = "grey") +
   scale_x_continuous(name = "Longitude") +
   scale_y_continuous(name = "Latitude")  
 }
}
\keyword{datasets}

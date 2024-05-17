\name{inc.nzfmd}

\docType{data}

\alias{inc.nzfmd}

\title{Data from a hypothetical outbreak of foot and mouth disease in New Zealand}

\description{
Data from a hypothetical outbreak of foot and mouth disease in New Zealand, simulated using InterSpread Plus.
}

\usage{data(inc.nzfmd)}

\format{
A list comprised of objects \code{ips}, \code{par}, \code{tra}, and \code{n0001}. These data provide details of 122 premises simulated as infected with foot and mouth from a population of 28,291 premises at risk. 

Object \code{ips} provides summary information for the 122 infected premises. Variables are \code{placeid}: unique premises identifier, \code{placetype}: premises type, \code{ipnumber}: infected premises number, \code{x}: easting coordinate of the premises centroid, \code{y}: northing coordinate of the premises centroid, \code{pubrept}: identifies whether or not the infected premises was reported by a member of the public (1 = public report, 0 = other), \code{cause}: estimated cause of infection, \code{infdate}: date of infection, \code{sgndate}: date of onset of clinical signs, and \code{visdate}: visit date. The next 18 variables provide the following details for pigs, dairy cattle, beef cattle, sheep, deer, and goats present on each infected premises: the number of animals present (e.g. \code{nopig}), the number of animals initially infected (e.g. \code{inipig}), and the estimated age of lesions at time of examination (e.g. \code{agepig}). 

Object \code{par} provides details of the premises at risk. Variables are \code{placeid}: unique premises identifier, \code{placetype}: premises type, \code{x}: easting coordinate of the premises centroid, \code{y}: northing coordinate of the premises centroid, \code{slgtdate} date of slaughter, \code{disdate} date cleaning and disinfection completed. 

Object \code{tra} provides details of tracing events. Variables are \code{srcplaceid} unique premises identifier of the source, \code{srcsgndate} date of onset of clinical signs on the source premises, \code{desplaceid} unique premises identifier of the destination, \code{causeid} cause identifier, \code{dessgndate} date of onset of clinical signs on the destination premises, \code{startdate} date event started, and \code{enddate} date event ended.

Object \code{n0001} lists places that lie within 0 -- 1000 metres of each of the 28,291 premises included in \code{par}. This object was produced using the \code{dnearneigh} in the \code{spdep} package.

}

\details{
Dates are in \code{Date} format. Easting and northing coordinates are in NZTM format. The NZTM projection (EPSG code 2193) is based on the New Zealand geodetic datum 2000 which uses the GRS80 ellipsoid (as used by WGS84). NZTM is the current (2022) standard for Biosecurity New Zealand.
}

\references{
Stevenson M, Sanson R, Stern M, O'Leary B, Sujau M, Molles-Benfell N, Morris R (2013). InterSpread Plus: a spatial and stochastic simulation model of disease in animal populations Preventive Veterinary Medicine 109, 10 - 24. DOI:  10.1016/j.prevetmed.2012.08.015.
}

\seealso{
\code{\link{inc.incubation}, \link{inc.ffi}, \link{inc.fdp}, \link{inc.genint}}
}

\examples{
## EXAMPLE 1:
\dontrun{
library(ggplot2); library(sf)

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
nzadm0tm.sf <- st_read(dsn = mdir[1], layer = "nzadm0-NZTM2000")
nzadm2tm.sf <- st_read(dsn = mdir[1], layer = "nzadm2-NZTM2000")

## Subset the map to focus on the area of interest:
id <- nzadm2tm.sf$NAME_2 == "Auckland"
tnzadm2tm.sf <- nzadm2tm.sf[id,]

## Load map key data: 
data(inc.mapkey)
mapkey.df <- inc.mapkey

# Load details of IPs:
data(inc.nzfmd)
ips.df <- inc.nzfmd$ips[,1:5]

## Assign scode --- add some DCPs to make the map interesting:
ips.df$scode <- "IP"
id <- sample(x = ips.df$placeid, size = 25, replace = FALSE)
ips.df$scode[ips.df$placeid \%in\% id] <- "DCP"
ipstm.sf <- st_as_sf(ips.df, coords = c("x","y"), remove = FALSE)
st_crs(ipstm.sf) <- 2193

# Farm population at risk:
par.df <- inc.nzfmd$par
partm.sf <- st_as_sf(par.df, coords = c("x","y"), remove = FALSE)
st_crs(partm.sf) <- 2193
partm.sf <- st_intersection(x = partm.sf, y = tnzadm2tm.sf)

ggplot() +
   theme_bw() +
   geom_sf(data = partm.sf, colour = "grey", shape = 1) +
   geom_sf(data = ipstm.sf, aes(colour = scode, shape = scode)) +
   geom_sf(data = tnzadm2tm.sf, fill = "transparent", colour = "black") +
   coord_sf(datum = st_crs(nzadm2tm.sf)) +
   scale_color_manual(breaks = mapkey.df$scode, values = mapkey.df$hex, 
     name = "Status") +
   scale_shape_manual(breaks = mapkey.df$scode, values = mapkey.df$shape, 
     name = "Status") +
   scale_x_continuous(name = "Easting (km)", labels = inc.mformat(div = 1000)) +
   scale_y_continuous(name = "Northing (km)", labels = inc.mformat(div = 1000))
 }
}
\keyword{datasets}

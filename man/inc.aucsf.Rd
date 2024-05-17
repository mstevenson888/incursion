\name{inc.aucsf}

\docType{data}

\alias{inc.aucsf}

\title{Data from a hypothetical outbreak of classical swine fever in Australia}

\description{
Data from a hypothetical outbreak of classical swine fever in Australia, simulated using the Australian Animal Disease Simulation Model, AADIS.
}

\usage{data(inc.aucsf)}

\format{
A data frame listing the simulated infection and detection details for premises with classical swine fever susceptible stock in Australia.

}

\details{
Location details are in decimal degrees and GDA94 Australian Albers (EPSG 3577) coordinates.
}

\seealso{
\code{\link{inc.mapkey}}
}

\examples{
## EXAMPLE 1:
\dontrun{
library(ggplot2); library(grid); library(sf)

## Set the name of image file to be written: 
fname <- "Farm_status_with_locator_map.png"

## Set the directory from which to read the shape file sets:
mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")

## Map of Victoria:
auadm1ll.sf <- st_read(dsn = mdir[1], layer = "auadm1-LL")
id <- auadm1ll.sf$NAME_1 == "Victoria"
viadm1ll.sf <- auadm1ll.sf[id,]
viadm1gd.sf <- st_transform(viadm1ll.sf, 3577)

## Map of Campaspe control and restricted zones:
cacbnll.sf <- st_read(dsn = mdir[1], layer = "aucampaspe_cbndry-LL")
cacbngd.sf <- st_transform(cacbnll.sf, 3577)

carbnll.sf <- st_read(dsn = mdir[1], layer = "aucampaspe_rbndry-LL")
carbngd.sf <- st_transform(carbnll.sf, 3577)

## Map of Campaspe roads:
caroall.sf <- st_read(dsn = mdir[1], layer = "aucampaspe_roads-LL")
caroagd.sf <- st_transform(caroall.sf, 3577)

## Simulated CSF outbreak data:
data(inc.aucsf)
cafar.df <- inc.aucsf

## Define the boundaries of the study area (to appear as a box on the 
## locator map):
cacbnll.bbox <- st_bbox(cacbnll.sf)
locatorll.df <- data.frame(xmin = cacbnll.bbox[1], xmax = cacbnll.bbox[3], 
   ymin = cacbnll.bbox[2], ymax = cacbnll.bbox[4])

# Load map key data: 
data(inc.mapkey)
mapkey.df <- inc.mapkey

## Map showing location of IPs, DCPs using recommended symbology:
detail.gg <- ggplot() +
  theme_bw() +
  geom_sf(data = cacbngd.sf, colour = "black", fill = "yellow", 
     alpha = 0.1) + 
  geom_sf(data = carbngd.sf, colour = "#DC143C", fill = "#DC143C", 
     alpha = 0.1) +
  geom_sf(data = caroagd.sf, colour = "grey") +
  geom_point(data = cafar.df, aes(x = xcoord, y = ycoord, 
     col = status, shape = status), size = 4) +
  coord_sf(datum = st_crs(cacbngd.sf)) +
  scale_color_manual(breaks = mapkey.df$scode, values = mapkey.df$hex, 
     name = "Status") +
  scale_shape_manual(breaks = mapkey.df$scode, values = mapkey.df$shape, 
     name = "Status") +
  scale_x_continuous(name = "Easting (km)", labels = inc.mformat(div = 1000)) +
  scale_y_continuous(name = "Northing (km)", labels = inc.mformat(div = 1000))

## Locator map:
locator.gg <- ggplot() + 
  theme_bw() +
  geom_sf(data = viadm1ll.sf, colour = "grey10", fill = "#fff7bc") +
  geom_rect(data = locatorll.df, aes(xmin = xmin, xmax = xmax, 
     ymin = ymin, ymax = ymax), alpha = 0, colour = "red", 
     linewidth = 1, linetype = 1) + 
  coord_sf(datum = st_crs(viadm1ll.sf)) +
  theme(axis.text.x = element_blank(), 
     axis.text.y = element_blank(), 
     axis.ticks = element_blank(),
     axis.title.x = element_blank(), 
     axis.title.y = element_blank()) +
  theme(panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
     panel.background = element_blank(), 
     axis.line = element_line(colour = "black"))

## Write the map out as a PNG file:
png(file = fname, w = 1800, h = 1800, res = 300)
grid.newpage()
v1 <- viewport(width = 1.0, height = 1.0,   x = 0.500, y = 0.500)
v2 <- viewport(width = 0.20, height = 0.20, x = 0.650, y = 0.825)
print(detail.gg, vp = v1) 
print(locator.gg, vp = v2)
dev.off()
}
}
\keyword{datasets}

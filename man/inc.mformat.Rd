\name{inc.mformat}

\alias{inc.mformat}

\title{
Format ggplot2 axis labels
}

\description{
Format ggplot2 axis labels.
}

\usage{
inc.mformat(div)
}

\arguments{
  \item{div}{scalar, numeric. By what should axis coordinates be divided by?}
}

\examples{
## EXAMPLE 1:
\dontrun{
library(ggplot2); library(sf)

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
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

## Plot the data. Axis labels shown in kilometres (instead of metres):
ggplot() +
   theme_bw() +
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
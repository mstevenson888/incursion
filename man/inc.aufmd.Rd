\name{inc.aufmd}

\docType{data}

\alias{inc.aufmd}

\title{Data from a hypothetical outbreak of foot and mouth disease in Australia}

\description{
Data from a hypothetical outbreak of foot and mouth disease in Australia, simulated using the Australian Animal Disease Simulation Model, AADIS.
}

\usage{data(inc.aufmd)}

\format{
A data frame listing the simulated infection and detection details for premises with foot and mouth disease susceptible stock in Australia.
}

\details{
Location details are in decimal degrees.
}

\references{
Bradhurst R, Roche S, East I, Kwan P, Garner M (2015). A hybrid modelling approach to simulating foot-and-mouth disease outbreaks in Australian livestock. Frontiers in Environmental Science 3. DOI: 10.3389/fenvs.2015.00017.
}

\seealso{
\code{\link{inc.incubation}, \link{inc.ffi}, \link{inc.fdp}, \link{inc.genint}}
}

\examples{
\dontrun{
## EXAMPLE 1:
library(dplyr); library(ggplot2); library(scales); library(tidyverse)

data(inc.aufmd)
aufmd.df <- inc.aufmd 

## Which simulation run produced the largest outbreak?
obsize.df <- aufmd.df \%>\%
  filter(day.infected > 0) \%>\%
  group_by(run)
obsize.df <- data.frame(summarise(obsize.df, n = n()))
obsize.df[obsize.df$n == max(obsize.df$n),]
## Run 10 had 42 IPs

taufmd.df <- aufmd.df \%>\%
  filter(day.infected > 0 & run == 10) \%>\%
  mutate(date.infected = as.Date("01-07-2022", format = "\%d-\%m-\%Y") 
     + day.infected)

ggplot(data = taufmd.df, aes(x = date.infected)) +
  theme_bw() +
  geom_histogram(binwidth = 1, fill = "dark blue", colour = "gray", 
     linewidth = 1) +
  scale_x_date(breaks = date_breaks("7 days"), labels = date_format("\%d \%b"), 
     name = "Infection date") +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2), limits = c(0,10), 
     name = "Number of farms") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## EXAMPLE 2:
mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
auadm0ll.sf <- st_read(dsn = mdir[1], layer = "auadm0-LL")
auadm1ll.sf <- st_read(dsn = mdir[1], layer = "auadm1-LL")

## Load map key data: 
data(inc.mapkey)
mapkey.df <- inc.mapkey

# Load details of IPs for run 10:
taufmd.df <- aufmd.df \%>\%
  filter(day.infected > 0 & run == 10) \%>\%
  mutate(scode = "IP")

taufmd.sf <- st_as_sf(taufmd.df, coords = c("long","lat"), remove = FALSE)
st_crs(taufmd.sf) <- 4283

ggplot() +
  theme_bw() +
  geom_sf(data = taufmd.sf, aes(colour = scode, shape = scode), size = 3) +
  geom_sf(data = auadm1ll.sf, fill = "transparent", colour = "grey") +
  geom_sf(data = auadm0ll.sf, fill = "transparent", colour = "black") +
  coord_sf(datum = st_crs(taufmd.sf)) +
  scale_color_manual(breaks = mapkey.df$scode, values = mapkey.df$hex, 
     name = "Status") +
  scale_shape_manual(breaks = mapkey.df$scode, values = mapkey.df$shape, 
     name = "Status") +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  theme(legend.position = c(0.2,0.15))
 }
}
\keyword{datasets}

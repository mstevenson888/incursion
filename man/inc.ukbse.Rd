\name{inc.ukbse}

\docType{data}

\alias{inc.ukbse}

\title{Counts of confirmed cases of bovine spongiform encephalopathy in the United Kingdom by calendar month and county, 1992}

\description{
Counts of confirmed cases of bovine spongiform encephalopathy in the United Kingdom by calendar month and county, 1992.}

\usage{data(inc.ukbse)}

\format{
A data frame listing \code{cname} county name, \code{myr} month and year of BSE confirmation, \code{ncas} count of confirmed cases of BSE, and \code{npop} the estimated county cattle population for 1992.
}

\references{
Stevenson M, Wilesmith J, Ryan J, Morris R, Lockhart J, Lin D, Jackson R (2000a) Temporal aspects of the bovine spongiform encephalopathy epidemic in Great Britain: Individual animal-associated risk factors for disease. Veterinary Record 147: 349 - 354. DOI: 10.1136/vr.147.13.349.
}

\examples{
## EXAMPLE 1:
\dontrun{
library(dplyr); library(ggplot2); library(RColorBrewer); library(sf); 
library(tidyverse)

## Choropleth map showing the incidence risk of BSE by county (expressed as 
## the number of cases per 100 head of cattle), 1992.
data(inc.ukbse)

## Collapse the data by year:
inc.ukbse <- inc.ukbse \%>\%
   group_by(cname)
inc.ukbse <- data.frame(summarize(inc.ukbse, 
    ncas = sum(ncas, na.rm = TRUE), npop = mean(npop, na.rm = TRUE))) 
inc.ukbse$inc <- (inc.ukbse$ncas / inc.ukbse$npop) * 100

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
ukadm2bng.sf <- st_read(dsn = mdir[1], layer = "ukadm2-BNG")
ukadm2bng.sf$inc <- inc.ukbse$inc[match(ukadm2bng.sf$cname, inc.ukbse$cname)]

ggplot() + 
  theme_bw() +
  geom_sf(data = ukadm2bng.sf, aes(fill = inc), colour = "dark grey") + 
  scale_fill_gradientn(limits = c(0,5), colours = brewer.pal(n = 5, "Reds"), 
      guide = "colourbar") +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  labs(fill = "BSE cases\nper 100") +
  theme(legend.position.inside = c(0.85,0.65), 
      legend.background = element_rect(colour = NA, fill = NA))
}
}
\keyword{datasets}

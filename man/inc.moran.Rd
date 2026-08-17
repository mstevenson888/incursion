\name{inc.moran}

\alias{inc.moran}

\title{
Local Moran statistics
}

\description{
Returns the local Moran's I statistic for area units based on the specified spatial weights object.
}

\usage{
inc.moran(x, listw)
}

\arguments{
  \item{x}{a numeric vector listing the outcome of interest. The length of \code{x} should be the same as the neighbours list in \code{listw}.}
  \item{listw}{a \code{listw} object created by \code{nb2listw}.}
}

\details{
The function returns a list with data suitable for a Moran scatter plot and for mapping.
}

\value{
A list containing:
  \item{moran.test}{an object of class \code{htest} listing the global Moran's I statistic and a test of its statistical signficance.}
  \item{moran.plot}{a data frame listing the outcome of interest, \code{x} and the weighted value of the outcome of interest for \code{x}'s neighbours. The weighted value is computed using \code{listw}.}
}

\examples{
## EXAMPLE 1:
## Incidence risk of BSE by county (expressed as the number of cases per
## 100 head of cattle), 1992.
\dontrun{
library(dplyr); library(ggplot2); library(RColorBrewer); library(sf); 
library(spdep); library(tidyverse)

data(inc.ukbse)

## Collapse the data by county:
inc.ukbse <- inc.ukbse \%>\%
  group_by(cname)
inc.ukbse <- data.frame(summarize(inc.ukbse, cid = first(cid),
   ncas = sum(ncas, na.rm = TRUE), npop = mean(npop, na.rm = TRUE))) 
inc.ukbse$inc <- (inc.ukbse$ncas / inc.ukbse$npop) * 100

mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")
ukadm2bng.sf <- st_read(dsn = mdir[1], layer = "ukadm2-BNG")
ukadm2bng.sf$inc <- inc.ukbse$inc[match(ukadm2bng.sf$cid, inc.ukbse$cid)]

breaks <- seq(from = 0, to = 3, by = 0.75)

ggplot() + 
  theme_bw() +
  geom_sf(data = ukadm2bng.sf, aes(fill = inc), colour = "dark grey") + 
  scale_fill_gradientn(breaks = breaks, limits = c(0,3), 
   colours = brewer.pal(n = 5, "Greens"), guide = "colourbar") +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  labs(fill = "BSE cases\nper 100") +
  theme(legend.position.inside = c(0.85,0.65), 
   legend.background = element_rect(colour = NA, fill = NA))

## Create queen contiguity neighbours. Tracts sharing boundary points are 
## defined as neighbours:
ukadm2bng.nb01 <- poly2nb(ukadm2bng.sf, 
   row.names = ukadm2bng.sf$cid, queen = TRUE)
ukadm2bng.nbw01 <- nb2listw(ukadm2bng.nb01, style = "W", zero.policy = TRUE)

## Moran scatter plot:
ukadm2bng.mdf <- inc.moran(x = ukadm2bng.sf$inc, listw = ukadm2bng.nbw01)
names(ukadm2bng.mdf)
msplot.df <- ukadm2bng.mdf$moran.plot
names(msplot.df)

## Assign high and low risk using the 75th quantile of BSE incidence risk 
## as the cutpoint:
cut <- as.numeric(quantile(inc.ukbse$inc, probs = 0.75))

ggplot(data = msplot.df, aes(x = x, y = wx)) + 
  theme_bw() +
  geom_point(shape = 1) + 
  geom_smooth(formula = y ~ x, method = "lm") + 
  geom_hline(yintercept = cut, lty = 2) +
  geom_vline(xintercept = cut, lty = 2) + 
  geom_point(data = msplot.df[msplot.df$is_inf,], 
   aes(x = x, y = wx), shape = 9) +
  geom_text(data = msplot.df[msplot.df$is_inf,], 
   aes(x = x, y = wx, label = labels, vjust = 1.5)) +
  scale_x_continuous(breaks = breaks, limits = c(0,3), 
   name = "County BSE incidence (1992))") +
  scale_y_continuous(breaks = breaks, limits = c(0,3), 
   name = "Spatially lagged county BSE incidence (1992))")

## Assign county BSE incidence as high-high, low-low, low-high or high-low 
## based on cut (calculated above):
msplot.df$flag <- NA
msplot.df$flag[msplot.df$x > cut & msplot.df$wx > cut] <- "High-high"
msplot.df$flag[msplot.df$x < cut & msplot.df$wx < cut] <- "Low-low"
msplot.df$flag[msplot.df$x < cut & msplot.df$wx > cut] <- "Low-high"
msplot.df$flag[msplot.df$x > cut & msplot.df$wx < cut] <- "High-low"
table(msplot.df$flag)
msplot.df$flag <- factor(msplot.df$flag, 
   levels = c("Low-low","Low-high","High-low","High-high"))

## Add flags to ukadm2bng.sf:
ukadm2bng.sf$flag <- msplot.df$flag

## Choropleth map of county level local Moran statistics:
colours <- c(rgb(0,0,1, alpha = 0.2), "blue", rgb(1,0,0, alpha = 0.2), "red")

ggplot() +
  theme_bw() +
  geom_sf(data = ukadm2bng.sf, aes(fill = flag), colour = "dark grey") +
  scale_fill_manual(values = colours, labels = levels(ukadm2bng.sf$flag), 
   drop = FALSE) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position.inside = c(0.85,0.65), 
   legend.background = element_rect(colour = NA, fill = NA))

## Map the problem counties:
names(ukadm2bng.sf)
mcmap.df <- ukadm2bng.mdf$moran.plot

## Low BSE incidence counties surrounded by high BSE incidence counties 
## (x = low, wx = high):
id <- mcmap.df$labels \%in\% 
  as.numeric(mcmap.df$labels[mcmap.df$x < cut & mcmap.df$wx > cut])
pukadm2bng.sf <- ukadm2bng.sf[id,] 

breaks <- seq(from = 0, to = 3, by = 0.75)

ggplot() +
  theme_bw() +
  geom_sf(data = ukadm2bng.sf, aes(fill = inc), colour = "transparent") + 
  geom_sf(data = ukadm2bng.sf, fill = "transparent", colour = "grey") + 
  geom_sf(data = pukadm2bng.sf, fill = "red", colour = "red", 
   alpha = 0.35, size = 2) + 
  scale_fill_gradientn(breaks = breaks, limits = c(0,3), 
   colours = brewer.pal(n = 5, "Greens"), guide = "colourbar") +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  labs(fill = "BSE cases\nper 100") +
  theme(legend.position.inside = c(0.85,0.65), 
   legend.background = element_rect(colour = NA, fill = NA))

## High BSE incidence counties surrounded by low BSE incidence counties 
## (x = high, wx = low):
id <- mcmap.df$labels \%in\% 
  as.numeric(mcmap.df$labels[mcmap.df$x > cut & mcmap.df$wx < cut])
pukadm2bng.sf <- ukadm2bng.sf[id,] 

ggplot() +
  theme_bw() +
  geom_sf(data = ukadm2bng.sf, aes(fill = inc), colour = "transparent") + 
  geom_sf(data = ukadm2bng.sf, fill = "transparent", colour = "grey") + 
  geom_sf(data = pukadm2bng.sf, fill = "red", colour = "red", 
   alpha = 0.35, size = 2) + 
  scale_fill_gradientn(breaks = breaks, limits = c(0,3), 
   colours = brewer.pal(n = 5, "Greens"), guide = "colourbar") +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  labs(fill = "BSE cases\nper 100") +
  theme(legend.position.inside = c(0.85,0.65), 
        legend.background = element_rect(colour = NA, fill = NA))
 }
}


\name{inc.ukfmd}

\docType{data}

\alias{inc.ukfmd}

\title{Details of confirmed holding cases of foot and mouth disease in Cumbria in the United Kingdom, 2001}

\description{
Details of confirmed holding cases of foot and mouth disease in Cumbria in the United Kingdom, 2001.}

\usage{data(inc.ukfmd)}

\format{
A data frame listing \code{cph} county, parish holding identifier, \code{status} foot and mouth disease status, \code{infdate} the estimated date of infection, \code{condate} confirmation date and \code{slgtdate} herd slaughter date for cases of foot and mouth disease in Cumbria in the United Kingdom in 2001.
}

\references{
Wilesmith J, Stevenson M, King C, Morris R (2003) Spatio-temporal epidemiology of foot-and-mouth disease in two counties of Great Britain in 2001. Preventive Veterinary Medicine 61:  157 - 170. DOI:  10.1016/j.prevetmed.2003.08.002.
}

\examples{
## EXAMPLE 1:
## FMD cases in Cumbria, United Kingdom, 2001.
\dontrun{
library(dplyr); library(ggplot2); library(scales); library(tidyverse)

data(inc.ukfmd)
ukfmd.df <- inc.ukfmd
head(ukfmd.df)

## Aggregate the data by day:
ukfmd.df <- ukfmd.df \%>\%
   mutate(cssdate = as.Date(cssdate, format = "\%Y-\%m-\%d")) \%>\%
   group_by(cssdate)
ukfmd.df <- data.frame(dplyr::summarise(ukfmd.df, ncas = n()))
head(ukfmd.df)

## Generate a data frame listing a sequence of calendar dates and the 
## number of disease events detected on each date (with zero for those 
## dates on which no disease events were detected:
ukfmd.df <- ukfmd.df \%>\%
   complete(cssdate = seq.Date(min(cssdate, na.rm = TRUE), 
    max(cssdate, na.rm = TRUE), by = "day")) \%>\%
   replace_na(list(ncas = 0))
ukfmd.df <- data.frame(ukfmd.df)
head(ukfmd.df)                                                                                         

## Plot the FMD data as an epidemic curve using ggplot2:
ggplot(data = ukfmd.df, aes(x = cssdate, weight = ncas)) +
   theme_bw() +
   geom_histogram(binwidth = 1, fill = "dark blue", colour = "gray") +
   scale_x_date(breaks = date_breaks("1 month"), labels = date_format("\%b \%Y"), 
    name = "Date of onset of signs") +
   scale_y_continuous(breaks = seq(from = 0, to = 20, by = 5), 
    limits = c(0,20), name = "Number of confirmed cases") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 }
}
\keyword{datasets}

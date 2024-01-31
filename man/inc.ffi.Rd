\name{inc.ffi}

\alias{inc.ffi}

\title{
First fortnight incidence
}

\description{
Returns a data frame listing the first fortnight incidence (risk).
}

\usage{
inc.ffi(ips, par, start, period = 14, conf.level = 0.95)
}

\arguments{
  \item{ips}{an 8 column data frame listing details of infected places. Columns (in order): unique place identifier, place type, infected place number, easting coordinate of the place centroid, northing coordinate of the place centroid, infection date, date of onset of clinical signs, and visit date. There should be one row for each infected place.}
  \item{par}{a 5 column data frame listing: unique place identifier, place type, easting coordinate of the place centroid, northing coordinate of the place centroid, date of slaughter, date cleaning and disinfection completed.}
  \item{start}{the start date for the analysis.}
  \item{period}{the length of time (in days) to be analysed.}
  \item{conf.level}{scalar, defining the magnitude of the returned confidence interval. Must be a single number between 0 and 1.}
}

\details{
First fortnight incidence is a term coined by Hutber et al. (2006) as a predictor for regional prevalence zonal disease duration. The numerator is the number of infected places where the onset of clinical signs is from \code{start} to \code{start + period}. If \code{start} equals 22 Feb 2001 and \code{period} equals 14 days then selected interval is from 22 Feb 2001 to 7 Mar 2001 (inclusive). The denominator is the total number of susceptible places in the area of interest. Although `fortnight' is used as the name for this function it should be noted that incidence risk can be calculated for any time interval following \code{start} date.

Confidence intervals for the incidence risk estimates are calculated using Wilson's approximation (see Rothman 2002, page 132).
}

\value{
A data frame containing the following:
  \item{est}{the point estimate of incidence risk for the prescribed interval.}
  \item{lower}{the lower bound of incidence risk for the prescribed interval.}
  \item{upper}{the upper bound of incidence risk for the prescribed interval.}
}

\references{
Hutber AM, Kitching RP, Pilipcinec E (2006). Predictions for the timing and use of culling or vaccination during a foot-and-mouth disease epidemic. Research in Veterinary Science 81: 31 - 36. 

Rothman KJ (2002). Epidemiology An Introduction. Oxford University Press, London, pp. 130 - 143.
}

\examples{
## EXAMPLE 1:
data(inc.nzfmd)
ips <- inc.nzfmd$ips[,c(1:5,8:10)]
par <- inc.nzfmd$par[,c(1:5)]

## Incidence risk for the 14 days following 10 Mar 2001: 
res <- inc.ffi(ips = ips, par = par, start = as.Date("2001-03-10", 
   format = "\%Y-\%m-\%d"), period = 14, conf.level = 0.95)
round(res * 100, digits = 2)

## The incidence risk of disease for the 14 days following 10 Mar 2001 was
## 29 cases (95\% CI 23 to 36) cases per 100 places at risk.
}

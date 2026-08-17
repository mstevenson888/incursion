\name{inc.breaks}

\alias{inc.breaks}

\title{
Breaks and labels when plotting a choropleth map of risk estimates on a log2 scale
}

\description{
Provides breaks and labels when plotting a choropleth map of risk estimates on a log2 scale.
}

\usage{
inc.breaks(lower, upper, nbreaks, digits = 3)
}

\arguments{
  \item{lower}{scalar, the lower bound of the risk estimate.}
  \item{upper}{scalar, the upper bound of the risk estimate.}
  \item{nbreaks}{scalar, the required number of breaks for the legend (ideally on odd number).}
  \item{digits}{scalar, the required number of digits for legend lablels.}
}

\details{
The function returns a list with data that can be passed to the \code{scale_fill_gradientn} function in \code{ggplot2}.
}

\value{
A list containing:
  \item{limits}{the lower and upper bound of the the limits for plotting.}
  \item{breaks}{a vector listing the legend breaks.}
  \item{labels}{a vector listing the legend labels.}
}

\examples{
## EXAMPLE 1:
## A regression model has returned risk estimates for each area of interest
## on the log scale. You plot the risk estimates on the log scale as a 
## choropleth map but would like the legend labels to show risk ratios on 
## the appropriate scale (i.e., the interval from a risk ratio of 1 to 2 is 
## the same as the interval from 0.5 to 1). The lower bound of your risk
## estimates is 0.25, the upper bound is 4.00 and you'd like five breaks:

inc.breaks(lower = 0.25, upper = 4, nbreaks = 5, digits = 2)

## The limits for plotting are -2 to 2. The breaks are -2, -1,  0,  1 and  2. 
## The legend labels are 0.25, 0.50, 1.00, 2.00 and 4.00.

}


\name{inc.trace}

\alias{inc.trace}

\title{
Backward and forward trace windows
}

\description{
Lists the start and end date of of tracing windows for an identified infected place. 
}

\usage{
inc.trace(inc, edate, mlage, det.disin, buffer, plot = TRUE)
}

\arguments{
  \item{inc}{numeric vector of length three listing (in order) the minimum, median and maximum of the incubation (or latent) period for the disease of interest.}
  \item{edate}{scalar, date of infected place detection in \code{as.Date} format.}
  \item{mlage}{scalar, maximum lesion age (in days) on the date of detection.}
  \item{det.disin}{scalar, the interval (in days) from detection to the end of cleaning and disinfection procedures.}
  \item{buffer}{scalar, the length of the buffer period (in days) for backward tracing.}
  \item{plot}{logical. If \code{TRUE} a calendar is drawn to show backward and forward trace windows.}
}

\details{
This function can be used to define backward and forward tracing windows for places identified as infected during an infectious disease outbreak response.
}

\value{
A list containing the following:
   \item{incubation}{a data frame listing the median, minimum and maximum incubation-latent period as entered by the user. }
   \item{backward.trace}{a data frame listing \code{start} (the start of the backward trace window) and \code{end} (the end of the backward trace window).}
   \item{forward.trace}{a data frame listing \code{start} (the start of the forward trace window) and \code{end} (the end of the forward trace window).}
   \item{calendar}{a \code{ggplot2} plot object.}
}

\examples{
## EXAMPLE 1:
## The median incubation period (i.e., the interval in days from infection to 
## the onset of clinical signs) for foot and mouth disease (FMD) is around 
## 4 days. The upper extent of the incubation period is believed to be 
## in the order of 12 days.

## You visit a farm that is confirmed as FMD positive on 1 December 2021. The
## estimated age of the oldest lesions on that date is 5 days.

## Assuming a detection to end of cleaning and disinfection interval of 12
## days and a backward trace buffer window of 14 days, what are the start and
## end dates of the backward and forward trace windows for this farm?

fmd.trace <- inc.trace(inc = c(1,4,12), 
   edate = as.Date("01-12-2023", format = "\%d-\%m-\%Y"), mlage = 5,
   det.disin = 12, buffer = 14, plot = TRUE)

## The backward trace window starts on 31 October 2023 and ends on 25 November
## 2023.

## The forward trace window starts on 31 October 2023 and ends on 13 December
## 2023.

}

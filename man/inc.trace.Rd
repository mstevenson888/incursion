\name{inc.trace}

\alias{inc.trace}

\title{
Backward and forward trace windows
}

\description{
Lists the start and end date of of tracing windows for an identified infected place. 
}

\usage{
inc.trace(m, b, mlage, edate, det.dis, buffer, conf.level = 0.95)
}

\arguments{
  \item{m}{scalar, median incubation (or latent) period for the disease of interest.}
  \item{b}{scalar, the expected upper confidence limit of the incubation (or latent) )period of the disease of interest.}
  \item{mlage}{scalar, maximum lesion age (in days).}
  \item{edate}{scalar, date of infected place detection in \code{as.Date} format.}
  \item{det.dis}{scalar, interval (in days) from detection to the end of cleaning and disinfection.}
  \item{buffer}{scalar, the length of the buffer period (in days) for backward tracing.}
  \item{conf.level}{magnitude of the returned confidence interval for the incubation (or latent) period of the disease of interest (which in turn determines the length of the calculated tracing windows).}
  
}

\details{
This function can be used to define backward and forward tracing windows for places identified as infected during an infectious disease outbreak response. 

The median incubation-latent period and upper bound of the confidence limit of the incubation-latent period is used to calculate the standard deviation of the interval for a lognormal distribution. The function then calculates the lower and uppper bound of the confidence interval for the incubation-latent period assuming it follows a lognormal distribution, with the width of the confidence interval determined by argument \code{conf.level}.
}

\value{
A list containing the following:
  \item{backward.trace}{a data frame listing \code{start} (the start of the backward trace window) and \code{end} (the end of the backward trace window).}
  \item{forward.trace}{a data frame listing \code{start} (the start of the forward trace window) and \code{end} (the end of the forward trace window).}
    \item{lnorm}{a data frame with columns: \code{meanlog} the mean of the distribution of incubation periods on the log scale and \code{sdlog} the standard deviation of the distribution of incubation periods on the log scale.}
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

fmd.trace <- inc.trace(m = 5, b = 12, mlage = 5, 
   edate = as.Date("01-12-2023", format = "\%d-\%m-\%Y"), 
   det.dis = 12, buffer = 14, conf.level = 0.95)
fmd.trace

## The backward trace window starts on 3 November 2023 and ends on 23 November
## 2023.

## The forward trace window starts on 3 November 2023 and ends on 13 December
## 2023.

\dontrun{
## Frequency histogram of simulated incubation periods with density curve 
## superimposed as a line plot:

library(ggplot2)

rlnorm.df <- data.frame(val = rlnorm(n = 1000, 
   meanlog = fmd.trace$lnorm$meanlog, 
   sdlog = fmd.trace$lnorm$sdlog))

z <- seq(from = 0, to = 15, by = 0.01)
   
dlnorm.df <- data.frame(z = z, val = dlnorm(x = z, 
   meanlog = fmd.trace$lnorm$meanlog, 
   sdlog = fmd.trace$lnorm$sdlog))

ggplot() +
  theme_bw() +
  geom_histogram(data = rlnorm.df, aes(x = val), 
     color = "grey", fill = "dark blue") +
  geom_line(data = dlnorm.df, aes(x = z, y = 500 * val), 
     color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(0,15), name = "Incubation period (days)") +
  scale_y_continuous(limits = c(0,200), name = "Frequency")
}  

}

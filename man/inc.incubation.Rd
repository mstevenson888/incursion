\name{inc.incubation}

\alias{inc.incubation}

\title{
Estimated infection dates and incubation periods
}

\description{
Returns a data frame of infection dates and incubation periods. Where data are missing or unavailable an incubation period is estimated by drawing a random number from a log normal distribution (the parameters of which may be specified by the user).
}

\usage{
inc.incubation(ips, meanlog = log(7), sdlog = log(1.5))
}

\arguments{
  \item{ips}{a 12 column data frame listing details of infected places. Columns (in order): unique place identifier, place type, infected place number, infection date, date of onset of clinical signs, visit date, estimated age of clinical signs at time of examination for pigs, dairy cattle, beef cattle, sheep, deer, and goats. There should be one row for each infected premises.}
  \item{meanlog}{mean of the log normal distribution on the log scale.}
  \item{sdlog}{standard deviation of the log normal distribution on the log scale.}
}

\details{
In the input data frame the variables defining infection date and date of onset of signs are recorded at the place level. These values are used in preference to the species age of signs variables for the incubation period calculations.

The arguments \code{meanlog} and \code{sdlog} are used to define a log normal distribution which is used to estimate incubation period.

If (infection date present, signs date present, species age of signs absent) a random number is drawn from the log normal distribution to estimate incubation period.

If (infection date absent, signs date present, species age of signs absent) a random number is drawn from the log normal distribution to estimate incubation period. Infection date is estimated as signs date minus the estimated incubation period.

If (infection date absent, signs date absent, species age of signs present) a random number is drawn from the log normal distribution to estimate incubation period. Infection date is estimated as visit date minus the largest value of age of signs (across all species) minus the estimated incubation period. 

If (infection date present, signs date present, species age of signs absent) incubation period equals signs date minus infection date.

If (infection date absent, signs date present, species age of signs present) a random number is drawn from the log normal distribution to estimate incubation period. Infection date is estimated as signs date minus the estimated incubation period.

If (infection date present, signs date present, species age of signs present) incubation period equals signs date minus infection date.
}

\value{
A data frame containing the following:
  \item{ip}{the infected place number (taken directly from the input data frame).}
  \item{inf.date}{actual or estimated infection date.}
  \item{inf.est}{indicator for status of infection date: 0 = actual, 1 = estimated.}
  \item{inc}{actual or estimated incubation period.}
  \item{inc.est}{indicator for status of incubation period: 0 = actual, 1 = estimated.}  
}

\references{
Sartwell PE (1950). The distribution of incubation of disease. American Journal of Epidemiology 51: 310 - 318.
}

\note{Incubation period refers to the period from infection to the development of symptomatic disease. Latent period refers to the period between disease initiation and development of symptomatic disease. The term incubation period is equivalent to latent period: incubation period is the term used when dealing with infectious diseases.

The antilog of one standard deviation from the mean log incubation period has been called the dispersion factor (Sartwell 1950). The dispersion factor multiplied by the mean log of the incubation period will define an interval above which 16\% of the intervals will fall, and the mean divided by the dispersion factor will define the period below which 16\% will occur.
}
\examples{
## EXAMPLE 1:
data(inc.nzfmd)
ips <- inc.nzfmd$ips[,c(1:3,8:10,13,16,19,22,25,28)]
fmd.inc <- inc.incubation(ips = ips, meanlog = log(7), sdlog = log(1.5))

## Frequency histogram of incubation periods:
\dontrun{
library(ggplot2); library(scales)

ggplot(data = fmd.inc, aes(x = inc)) +
   theme_bw() +
   geom_histogram(color = "grey", fill = "dark blue", binwidth = 2) +
   scale_x_continuous(name = "Incubation period (days)") +
   scale_y_continuous(name = "Number of observations") 
}

## Compare the distribution of actual incubation periods with those that 
## have been estimated: 
fmd.inc$inc.est <- factor(fmd.inc$inc.est, levels = c(0,1), 
   labels = c("Actual","Estimated"))

\dontrun{
ggplot(data = fmd.inc, aes(x = inc)) +
   theme_bw() +
   geom_histogram(color = "grey", fill = "dark blue", binwidth = 2) +
   facet_grid(~ inc.est) +
   scale_x_continuous(name = "Incubation period (days)") +
   scale_y_continuous(name = "Number of observations") 
}

## Epidemic curve using the augmented data:
\dontrun{
ggplot(data = fmd.inc, aes(x = inf.date)) +
   theme_bw() +
   geom_histogram(color = "grey", fill = "dark blue", binwidth = 2) +
   scale_x_date(breaks = date_breaks("1 week"), labels = date_format("\%d \%b"), 
     name = "Date") +
   scale_y_continuous(name = "Number of incident cases of FMD") 
}
}

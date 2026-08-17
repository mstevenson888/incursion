\name{inc.ripratio}

\alias{inc.ripratio}

\title{
At-risk to infected place ratio
}

\description{
Ratio of the number of at-risk places to the number of identified infected places for a given time frame.
}

\usage{
inc.ripratio(ips, tra, start, period = 14, conf.level = 0.95)
}

\arguments{
  \item{ips}{a 4 column data frame listing details of infected places. Columns (in order): unique place identifier, place type, infected place number, and onset of clinical signs date. There should be one row for each infected place.}
  \item{tra}{a 7 column data frame listing details of tracing events. Columns (in order): source place unique identifier, date of onset of signs on the source place, destination place unique identifier, date of onset of signs on the destination place, episode start date (the date that contact between the two places commenced), and episode end date (the date that contact between the two places ceased).}
  \item{start}{the start date for the analysis.}
  \item{period}{the length of time (in days) to be analysed.}
  \item{conf.level}{scalar, defining the magnitude of the returned confidence interval. Must be a single number between 0 and 1.}
}

\details{
When an epidemic commences there are no controls in place and disease has the potential to spread extensively by the time the index premises is detected. The number of premises with direct or indirect contact with the index premise(s) will have a direct bearing on the number of newly infected premises and hence the estimated dissemination ratio. 

In this function the numerator is the total number of premises exposed to direct or indirect movements off identified infected premises. The denominator is the total number of infected premises identified in the specified time frame. If \code{start} equals 22 Feb 2001 and \code{period} equals 14 days then the time frame is from 22 Feb 2001 to 7 Mar 2001 (inclusive).

Confidence intervals are calculated using the method of Dobson et al. (1991). 
}

\value{
Returns the point estimate and confidence interval of the at-risk to infected place ratio.
}

\references{
Dobson AJ, Kuulasmaa K, Eberle E, Scherer J (1991). Confidence intervals for weighted sums of Poisson parameters. Statistics in Medicine 10: 457 - 462. 
}

\examples{
## EXAMPLE 1:
data(inc.nzfmd)
ips <- inc.nzfmd$ips[,c(1:3,9)]
tra <- inc.nzfmd$tra

start <- as.Date("2001-02-22", format = "\%Y-\%m-\%d")
inc.ripratio(ips = ips, tra = tra, start = start, period = 14, 
   conf.level = 0.95)

## The at-risk infected place ratio for the period 22 February 2001 to 
## 7 March 2001 (inclusive) was 0.16 (95\% CI 0.09 to 0.27).

}



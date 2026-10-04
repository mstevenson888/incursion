\name{inc.selfsurv}

\alias{inc.selfsurv}

\title{
Ratio of public reported cases to surveillance activity cases
}

\description{
Ratio of public reported cases to surveillance activity cases for a given time frame.
}

\usage{
inc.selfsurv(ips, start, period = 14, conf.level = 0.95)
}

\arguments{
  \item{ips}{a 5 column data frame listing details of infected places. Columns (in order): unique place identifier, place type, infected place number, public report flag (0 or 1), onset of clinical signs date. There should be one row for each infected place.}
  \item{start}{the start date for the analysis.}
  \item{period}{the length of time (in days) to be analysed.}
  \item{conf.level}{scalar, defining the magnitude of the returned confidence interval. Must be a single number between 0 and 1.}
}

\details{
In a well managed outbreak most infected places will be identified by surveillance activities conducted by the agency responsible for disease control. The ratio of infected places brought to the attention of authorities by members of the public (i.e. `surprise' cases) to those known to be at risk provides a measure of the effectiveness of control and eradication measures. Ideally, this ratio should be close to zero at all times.

If \code{start} equals 22 Feb 2001 and \code{period} equals 14 days then the time frame is from 22 Feb 2001 to 7 Mar 2001 (inclusive).

Confidence intervals are calculated using the method of Dobson et al. (1991). 
}

\value{
Returns the point estimate and confidence interval of ratio of public reported disease cases to surveillance activity cases.
}

\references{
Dobson AJ, Kuulasmaa K, Eberle E, Scherer J (1991). Confidence intervals for weighted sums of Poisson parameters. Statistics in Medicine 10: 457 - 462. 
}

\examples{
## EXAMPLE 1:
data(inc.nzfmd)
ips <- inc.nzfmd$ips[,c(1:3,6,9)]

start <- as.Date("2001-02-22", format = "\%Y-\%m-\%d")
inc.selfsurv(ips = ips, start = start, period = 14, conf.level = 0.95)

## For the period 22 February 2001 to 7 March 2001 (inclusive) the ratio of 
## public reported cases to surveillance activity cases was 0.14 
## (95\% CI 0.07 to 0.24).

}


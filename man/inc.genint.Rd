\name{inc.genint}

\alias{inc.genint}

\title{
Estimated generation intervals
}

\description{
This function returns the generation interval, the number of days from the onset of clinical signs on a source place to the onset of clinical signs on one or more identified destination place(s).
}

\usage{
inc.genint(tra, use = "all")
}

\arguments{
  \item{tra}{a 7 column data frame listing details of tracing events. Columns (in order): source place unique identifier, date of onset of signs on the source place, destination place unique identifier, date of onset of signs on the destination place, episode start date (the date that contact between the two places commenced), and episode end date (the date that contact between the two places ceased).}
  \item{use}{a character string, indicating the method to be used. Options are \code{all} calculate all generation intervals, or \code{first} calculate the first generation interval for each source place (if there are more than one). Option \code{first} should be used if you want to calculate the earliest generation interval.}
}

\details{
This function uses the unique numeric identifiers for places (as opposed to infected place number).

It is assumed that the input data for this function is a complete record of contact events between infected places. That is, both source and destination places are both infected (and therefore have an onset of clinical signs date).

Generation intervals that are negative (i.e. cases where the onset of clinical signs date on the destination place occurs before the onset of clinical signs on the source place) are ignored.
}

\value{
A data frame containing the following:
  \item{src}{the source place identifier (taken directly from the input data frame).}
  \item{des}{the destination place identifier (taken directly from the input data frame).}
  \item{gen}{the generation interval.}
}

\examples{
## EXAMPLE 1:
data(inc.nzfmd)
tra <- inc.nzfmd$tra

gen01 <- inc.genint(tra = tra, use = "first")
hist(gen01$gen)
summary(gen01$gen)

## When all generation intervals are included, there may be some
## outliers:
gen02 <- inc.genint(tra = tra, use = "all")
hist(gen02$gen)
summary(gen02$gen)
}

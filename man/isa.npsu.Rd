\name{inc.npsu}

\alias{inc.npsu}

\title{
Estimate the number of primary sampling units given the total number of secondary sampling units in a population
}

\description{
Estimates the number of primary sampling units (e.g., herds, households) given an estimate of the total number of secondary sampling units in a population of interest.
}

\usage{
inc.npsu(nssu, psumin, psumode, psumax)
}

\arguments{
  \item{nssu}{scalar, the total number of secondary sampling units in the population.}
  \item{psumin}{scalar, the minimum number of secondary sampling units in each primary sampling unit.}
  \item{psumode}{scalar, the most likely number of secondary sampling units in each primary sampling unit.}
  \item{psumax}{scalar, the maximum number of secondary sampling units in each primary sampling unit.}
}

\value{
A list containing the following:
  \item{npsu}{scalar, the estimated total number of primary sampling units in the population.}
  \item{psusize}{vector, the estimated number of secondary sampling units in each primary sampling unit.}
}

\details{
While this function does not use sampling techniques we use terminology used in the sampling literature. Primary sampling units (PSUs) )are the sampling units chosen in the first stage of selection (e.g., households, herds). Secondary sampling units (SSUs) )are the sampling units chosen in the second stage of selection, individuals (humans or animals).
}

\examples{
## EXAMPLE 1:
## We're told that there are approximately 450 cows in a village. The expected 
## number of cows per herd is 5 (minimum 2; maximum 25). What is the 
## estimated number of cattle herds?

tmp.ng <- inc.npsu(nssu = 450, psumin = 2, psumode = 5, psumax = 25)
tmp.ng$npsu; range(tmp.ng$psusize)

## We estimate that there are 58 herds in this village. The number of cows 
## per herd ranges from 3 to 17. Your results will differ from these 
## because the function takes random draws from a beta PERT distribution, 
## with different results for each draw.

}

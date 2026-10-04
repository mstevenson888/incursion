\name{inc.pmrisk}

\alias{inc.pmrisk}

\title{
Prospective movement risk assessment tool
}

\description{
A prospective animal-fomite movement risk assessment tool for use during an infectious disease outbreak response. 
}

\usage{
inc.pmrisk(crisk, src, des)
}

\arguments{
  \item{crisk}{scalar character. Disease risk in the source herd making the requested movement. Options are: \code{high}, \code{medium}, \code{low}, and \code{negligible}.}
  \item{src}{scalar character. Location details for the herd making the requested movement. Options are: \code{restricted place}, \code{at risk place from high risk zone}, \code{other place from infected zone}, and \code{outside infected zone}.}
  \item{des}{scalar character. Destination of the proposed movement. Options are: \code{into infected zone}, \code{out of infected zone}, and \code{within infected zone}.}
}

\details{
This function can be used to provide a decision for movement permit applications during an infectious disease outbreak response. 

To retrospectively assess movement risk (e.g., as part of tracing activities) see function \code{\link{inc.rmrisk}}.
}

\value{
A character string specifying the movement risk decision: \code{not restricted}, \code{permit conditions}, \code{permitted} or \code{prohibited}.
}

\examples{
## EXAMPLE 1:
## A herd manager from a high risk herd designated as a restricted place
## requests for a permit to move animals to another herd in an infected zone.
## What is your decision?

inc.pmrisk(crisk = "high", src = "restricted place", des = "into infected zone")

## Your decision is to prohibit the movement request.


## EXAMPLE 2:
## Details of requested movement events are recorded into a spreadsheet and 
## imported into R as a data frame:

movt.df <- data.frame(
   crisk = c("Low", "Negligible", "Medium"), 
   src   = c("Restricted place", "At risk place from high risk zone", 
      "Outside infected zone"),
   des   = c("Out of infected zone", "Within infected zone", 
      "Into infected zone"))

## Provide a decision for each of the movement requests.

movt.df$assess <- inc.pmrisk(crisk = movt.df$crisk, src = movt.df$src, 
   des = movt.df$des)
movt.df$assess

## "permit conditions" "not restricted" "not restricted" 
}

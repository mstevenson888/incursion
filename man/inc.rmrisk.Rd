\name{inc.rmrisk}

\alias{inc.rmrisk}

\title{
Retrospective movement risk assessment tool
}

\description{
A retrospective animal-fomite movement risk assessment tool for use during an infectious disease outbreak response. 
}

\usage{
inc.rmrisk(ritem, spp, ldays, incont, dest)
}

\arguments{
  \item{ritem}{scalar character. Use \code{ritem = host} if animals are moved. Use \code{ritem = object} if fomites are moved.}
  \item{spp}{scalar character. If \code{ritem = host} specifies the species to be moved. Options are \code{cattle}, \code{sheep} or \code{pigs}. Ignored if \code{ritem = object}.}
  \item{ldays}{scalar, integer. Number of days prior to the [ESTIMATED DATE OF INFECTION] that the movement event took place.}
  \item{incont}{logical. Did the movement event involve contact with susceptible hosts at the destination premises?}
  \item{dest}{scalar character. Destination of the movement event. Options are \code{within} (within surveillance zone) or \code{outside} (outside of surveillance zone).}
}

\details{
This function can be used to triage movement data collected from infected premises and trace farms for investigation priority. 

To prospectively assess movement risk (e.g., required when issuing movement permits during an outbreak response) see function \code{\link{inc.pmrisk}}.
}

\value{
A character string specifying the movement risk assessment priority: \code{low}, \code{medium} or \code{high}.
}

\examples{
## EXAMPLE 1:
## As part of investigations of infected premises details of off-farm movements 
## of animal hosts and fomites are recorded following herd manager interview. 
## Fourteen days before the estimated herd infection date an off-farm
## movement of cattle occurred to a location outside of the disease 
## surveillance area. The movement involved contact with susceptible hosts 
## at the destination premises. What is your assessment of the risk of this 
## movement event?

inc.rmrisk(ritem = "host", spp = "cattle", ldays = 14, incont = TRUE, 
   dest = "outside")

## This is a medium risk movement event.


## EXAMPLE 2:
## Details of movement events from infected premises that took place before
## the estimated date of infection are recorded into a spreadsheet and
## imported into R as a data frame:

movt.df <- data.frame(ritem = c("host","host"), spp = c("cattle","pigs"), 
   ldays = c(2,21), incont = c(TRUE,FALSE), dest = c("inside","outside"))
movt.df

## Assess the risk of each of the listed movement events and add the risk 
## assessment to the movement data frame:

movt.df$mrisk <- inc.rmrisk(ritem = movt.df$ritem, spp = movt.df$spp, 
   ldays = movt.df$ldays, incont = movt.df$incont, dest = movt.df$dest)
movt.df
}

\name{inc.local}

\alias{inc.local}

\title{
Local spread probabilities
}

\description{
This function returns data that can be used to estimate the probability of local spread as a function of time and distance from an infected source.
}

\usage{
inc.local(ips, par, src.des, neighbours, relative.to = "signs",
   offset = -3)
}

\arguments{
  \item{ips}{a 7 column data frame listing listing details of infected places. Columns (in order): unique place identifier, place type, infected place number, easting coordinate of place centroid, northing coordinate of place centroid, infection date, onset of clinical signs date. There should be one row for each infected place.}
  
  \item{par}{a 6 column data frame listing details of the place population at risk. Columns (in order): unique place identifier, place type, easting coordinate of the place centroid, northing coordinate of the place centroid, date of slaughter, date cleaning and disinfection completed.}
  
  \item{src.des}{a 5 column data frame listing details where transmission of infection has occurred from a source place to a destination place by local spread. Columns (in order): unique identifier of source place, date of onset of signs on the source place, unique identifier of destination place, cause identifier, date of onset of signs on the destination place. See details for further information.}
  
  \item{neighbours}{a list of neighbours within a defined distance band of each place listed in \code{par}.}
  
  \item{relative.to}{a character string, indicating when infectivity starts on the source place. Options are \code{signs}: infectivity starts from the date of onset of clinical signs, \code{infection}: infectivity starts from the date of infection.}
  
  \item{offset}{scalar, indicating the offset to be used for defining the risk period for each place acting as an infected source. See details for further information.}
}

\details{
Local spread is defined as the short distance (generally 5 kilometres or less) spread of disease between locations when there is no clear linkage other than geographical proximity. It is a term most often used when describing the epidemiology of foot and mouth disease in animal populations.

In the \code{src.des} data frame there is a one to many relationship between source and destination places. An infected source can have many destinations. A destination can receive infection from only one source.

Use the \code{dnearneigh} function in the \code{spdep} package to generate data for the \code{neighbours} list. 

Setting \code{relative.to} to \code{signs} and \code{offset} to -3 sets the start of the risk period at three days before the date of onset of clinical signs on the source place. Infectivity of a source place ends on the date that animals on the source place were slaughtered.
}

\value{
A data frame containing the following:
  \item{time}{the number of time periods from the date of onset of infectivity of the source place.}
  \item{n.risk}{the number of places at risk of infection.}
  \item{n.event}{the number of places infected.}
}

\references{
Sanson R, Stevenson M, Moles-Benfell N (2006). Quantifying local spread probabilities for foot-and-mouth disease. In: Proceedings of the 11th International Symposium on Veterinary Epidemiology and Economics. Cairns Convention Centre, Cairns, Australia.
}

\seealso{
\code{\link{inc.incubation}}
}

\examples{
## EXAMPLE 1:
data(inc.nzfmd)
ips <- inc.nzfmd$ips[,c(1:5,8,9)]
par <- inc.nzfmd$par
src.des <- inc.nzfmd$tra
n0001 <- inc.nzfmd$n0001

## Estimate missing infection dates:
tmp <- inc.incubation(ips = inc.nzfmd$ips[,c(1:3,8:10,13,16,19,22,25,28)], 
   meanlog = log(7), sdlog = log(1.5))
ips$infdate <- tmp$inf.date

inc.local(ips = ips, par = par, src.des = src.des, neighbours = n0001,
   relative.to = "signs", offset = -3)
}

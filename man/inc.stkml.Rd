\name{inc.stkml}

\alias{inc.stkml}

\title{
Export point locations and dates of [movement] events to a space-time KML file
}

\description{
Export point locations and dates of [movement] events to a space-time KML (keyhole markup language) file.
}

\usage{
inc.stkml(x, arrow = c(5000, 160), fname)
}

\arguments{
  \item{x}{an object of class \code{igraph} or class \code{data.frame}. An object of class \code{igraph} must contain \code{edate} (event date) as an edge attribute and variables \code{lon} (longitude, in decimal degrees) and \code{lat} (latitude, in decimal degrees) as node attributes. An object of class \code{data.frame} requires variables listing (in order) the location identifier, event date (as class \code{Date}), longitude (in decimal degrees) and latitude (in decimal degrees).} 
  \item{arrow}{a vector of length two defining the size of the arrow used on the edges in the KML file. Ignored if \code{x} is of class \code{data.frame}.}
  \item{fname}{a character string specifying the name of the output KML file.}
}

\value{
If \code{class(x) == igraph} two space-time keyhole markup language (KML) files: \code{fname_vertex.kml} and \code{fname_edge.kml}. \code{fname_vertex.kml} defines event locations. \code{fname_edge.kml} defines the time-stamped connections between event locations.

If \code{class(x) == data.frame} a single space-time keyhole markup language (KML) file \code{fname_vertex.kml}.

}

\examples{
## EXAMPLE 1:
## Data from a simulated outbreak of foot and mouth disease in Australia:
\dontrun{ 
data(inc.aufmd)
aufmd.df <- inc.aufmd 

## Select predicted infected premises from run 17:
id <- aufmd.df$day.infected != -1 & aufmd.df$run == 17; table(id)
aufmd.df <- aufmd.df[id,]
names(aufmd.df)

## Manipulate the data into the correct format:
aufmd.df <- aufmd.df[,c(1,18,9,8)]
names(aufmd.df)[1:4] <- c("oid","edate","lon","lat")
aufmd.df$edate <- as.Date("01-Jan-2024", format = "\%d-\%b-\%Y") + 
   aufmd.df$edate
aufmd.df <- aufmd.df[sort.list(aufmd.df$edate),] 
aufmd.df$oid <- 1:nrow(aufmd.df)
head(aufmd.df)

inc.stkml(x = aufmd.df, arrow = c(5000, 160), fname = "AU")
}

}


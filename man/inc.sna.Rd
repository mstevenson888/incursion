\name{inc.sna}

\alias{inc.sna}

\title{
Manipulate network data in edgelist format
}

\description{
Convert network data in edgelist format to matrix format to be read by a Geographic Information System.
}

\usage{
inc.sna(edge, vertex, epsg, fn = "SNA", shapefile = TRUE, socnetv = TRUE, 
   dichotomise = TRUE)
}

\arguments{
  \item{edge}{a 4 column data frame listing details of movment events. Columns (in order): unique identifier of source, unique identifier of destination, date of transaction, estimated risk score of transaction.}
  \item{vertex}{a data frame of vertex attributes. There must be at least three columns: unique identifier of vertex, easting coordinate of vertex, and northing coordinate of vertex.}
  \item{epsg}{EPSG code defining the coordinate reference system for data frame \code{vertex}.}
  \item{fn}{character string, prefix for output file names.}
  \item{shapefile}{logical, is function to return an ESRI shapefile?}
  \item{socnetv}{logical, is function to return a matrix file suitable for eading into SocNetV?}
  \item{dichotomise}{logical, dichotomise the matrix?}
}

\details{
Unique vertex identifiers can be either character strings or numbers, or combinations of strings and numbers. 
}

\value{
Returns one or more of three file formats: an ESRI shapefile (\code{*.shp, *.shx, *.dbf}) and/or a \code{*.csv} suitable for reading into SocNetV.

When \code{shapefile = TRUE} a vertex (point) and edge (linear) shape file are returned. The vertex shape file lists the identity of each vertex as well as its easting and northing coordinate. The edge shape file lists the identity of the source and destination vertices as well as the date of the recorded transaction.
}

\examples{
## EXAMPLE 1:
\dontrun{
## Here we use data from a hypothetical tracing exercise with location details
## recorded using the New Zealand Transverse Mercator (NZTM) projection,
## EPSG code 2193.
data(inc.network)

inc.sna(edge = inc.network$edg.dat, vertex = inc.network$vert.dat, 
   epsg = 2193, fn = "SNA", shapefile = TRUE, socnetv = TRUE, dichotomise = TRUE)
 }
}


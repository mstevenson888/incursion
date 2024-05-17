\name{inc.about}

\alias{inc.about}

\title{
The library incursion: summary information
}

\description{
Functions for the analysis of infectious disease outbreaks in human and animal populations.
}

\usage{
inc.about()
}

\details{
The most recent version of the incursion package can be obtained from: \url{https://mvs.unimelb.edu.au/research/groups/veterinary-epidemiology-melbourne}.

The following is a summary of the main functions and datasets in the \pkg{incursion} package. An alphabetical list of all functions and datasets is available by typing \code{library(help = incursion)}.

For further information on any of these functions, type \code{help(name)} or \code{?name} where \code{name} is the name of the function or dataset.
}

\section{CONTENTS:}{
A summary of the package functions, by theme, is as follows:
}

\section{1. Data sets}{

\tabular{ll}{
    \code{\link{inc.aucsf}}             \tab Data from a hypothetical outbreak of classical swine fever in Australia\cr
    \code{\link{inc.aufmd}}             \tab Data from a hypothetical outbreak of foot and mouth disease in Australia \cr
    \code{\link{inc.kofmd}}             \tab Actual and simulated outbreaks data from the 2002 outbreak of foot and mouth disease in South Korea\cr
    \code{\link{inc.nzfmd}}             \tab Data from a hypothetical outbreak of foot and mouth disease in New Zealand\cr
    \code{\link{inc.ukbse}}             \tab Counts of confirmed cases of bovine spongiform encephalopathy in the UK, 1992\cr
    \code{\link{inc.ukfmd}}             \tab Details of confirmed holding cases of foot and mouth disease in Cumbria, 2001\cr 
    \code{\link{inc.network}}           \tab Data from a hypothetical tracing exercise\cr       
 }
}
 
\section{2. Assessment of outbreak data quality}{

\tabular{ll}{
    \code{\link{inc.acf}}               \tab Temporal autocorrelation in event frequency data. \cr
    \code{\link{inc.moran}}             \tab Local Moran statistics. \cr
 }
}

\section{3. Querying outbreak data}{

\tabular{ll}{
    \code{\link{inc.closestpoint}}  \tab Which point is closest to a given point location?\cr
    \code{\link{inc.whichpoly}}     \tab In which polygon does a point lie? \cr
    \code{\link{inc.contacts}}          \tab Subset edgelist network data to identify contacts \cr
    \code{\link{inc.trace}}             \tab Backward and forward trace windows \cr
    \code{\link{inc.sna}}               \tab Manipulate network data in edgelist format \cr
 }
}

\section{4. Tools for generating reports}{

\tabular{ll}{
    \code{\link{inc.mapkey}}            \tab Acronyms, HEX colours and shapes recommended for mapping outbreak data\cr
    \code{\link{inc.matfix}}            \tab Re-configures a \code{spatstat} \code{im} object for plotting using \code{ggplot2} \cr
    \code{\link{inc.mformat}}           \tab Format \code{ggplot2} axis labels \cr
    \code{\link{inc.cropraster}}        \tab Crop a raster object to the boundaries of a polygon vector object\cr
 }
}

\section{5. Outbreak progression and response metrics}{

\tabular{ll}{
    \code{\link{inc.den}}            \tab Compute location and individual densities in an area around a defined point location\cr
    \code{\link{inc.edr}}            \tab Compute estimated dissemination ratio \cr
    \code{\link{inc.fdp}}            \tab First day prevalence \cr
    \code{\link{inc.ffi}}            \tab First fortnight incidence \cr
    \code{\link{inc.genint}}         \tab Estimated generation intervals \cr
    \code{\link{inc.incubation}}     \tab Estimated infection dates and incubation periods  \cr
    \code{\link{inc.local}}          \tab Local spread probabilities \cr
    \code{\link{inc.ripratio}}       \tab At-risk to infected place ratio \cr
    \code{\link{inc.selfsurv}}       \tab Ratio of public reported cases to surveillance activity cases \cr
  }
 }

\author{
Mark Stevenson (\email{mark.stevenson1@unimelb.edu.au}), Melbourne Veterinary School, Faculty of Science, University of Melbourne, Parkville, Victoria 3010, Australia.

Robert Sanson, AsureQuality New Zealand, PO Box 1654, Palmerston North 4410, New Zealand.

Andrew McFadden, Investigation and Diagnostic Centre Biosecurity New Zealand, PO Box 40742, Upper Hutt 5018, New Zealand.

Matthew Edwards, EpiVets New Zealand, 565 Mahoe Street, Te Awamutu 3800, New Zealand.
}

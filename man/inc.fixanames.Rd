\name{inc.fixanames}

\alias{inc.fixanames}

\title{
Correct area names using a reference map standard
}

\description{
Correct area names using a reference map standard.
}

\usage{
inc.fixanames(oareas.df, rareas.df, rmap.sf, rareaname, max.dist = 0.10, 
   use = "both", gengine = "osm", gkey = NA)
}

\arguments{
    \item{oareas.df}{a data frame listing observed area details. The first column must list the area name to be corrected (e.g., village). Additional columns provide further location details (e.g., subdistrict, district and province). The additional location detail columns improve the accuracy of geocoding (see below).}
    \item{rareas.df}{a data frame listing reference area details. The variable names for data frame \code{rareas.df} must match the variable names of \code{oreas.df}.}
    \item{rmap.sf}{an \code{sf} polygon object providing reference (i.e., `official') area names and their location details in EPSG 4326 projection. The attribute table of object \code{rmap.sf} should contain only the location variables listed in data frames \code{oareas.df} and \code{rareas.df}.}
  \item{rareaname}{if \code{use = "geocode"} or \code{use = "both"} a character vector, listing the variables in the attribute table of \code{rmap.sf} that correspond to the variables listed in data frames \code{oreas.df} and \code{rareas.df}.}
  \item{max.dist}{scalar number, specifying the maximum distance allowed for a fuzzy text match.}
  \item{use}{character string, specifying the type of matching to use. Options are \code{fuzzy} (fuzzy matching), \code{geocode} or \code{both}.}
  \item{gengine}{character string, specifying the geocoding engine to use. Options are \code{google} (Google Maps) and \code{osm} (Open Street Map). Ignored if \code{use = "fuzzy"}. See details.}
  \item{gkey}{character string, if \code{geocode = "google"} a valid Google Developer Geocode API key. Ignored if \code{use = "fuzzy"}. See details.}
}

\details{
This function would typically be used to `correct' small area (e.g., administrative 3 or 4) names recorded by an authority to make them consistent with official map sources. To do this a two-step approach is used. Firstly, fuzzy matching is used to match the observed area names with the reference list of area names. Here it is assumed that small differences in spelling account for the difference between observed area names and the reference area names. Set \code{max.dist} to a low number (e.g., 0.10) if you require a high degree of agreement between \code{oreas.df} and \code{rareas.df}. 

For the second step, observed area names that can't be matched using fuzzy matching are passed to either the Google Earth or Open Street Map geocoder to return their longitude and latitude centroid coordinates. These coordinates are then plotted over \code{rmap.sf} to return the appropriate reference area name from object \code{rmap.sf}.

To improve the accuracy of geocoding provide as many details as you can to define location in data frame \code{oareas.df}. The first column of data frame \code{oareas.df} should list the area name to be corrected (e.g., village). Additional columns might be subdistrict, district, province and country. The additional columns are used for geocoding so the more information you provide the greater the likelihood that you'll return a correct match. The order of the columns in data frame \code{oareas.df} is important. Work from the lowest level of aggregation sequentially to the highest. For example, village, subdistrict, district, province and then country. 

To reduce processing time only those areas that don't return a match using fuzzy matching are submitted for geocoding.

If you elect to use Google Maps for geocoding you'll need to enter a valid Google Developer Geocode API key.

Computation times can be slow, particularly if you're using several variables (listed in data frame \code{oareas.df}) for matching. The function provides messages to advise on progress.}

\value{
A copy of data frame \code{oareas.df} listing the observed area details and:
  \item{*.f}{the corrected reference area names using fuzzy matching.}
  \item{*.g}{the corrected reference area names using geocoding.}
}

\author{
Katon Dwi Kurniawan (Food and Agriculture Organisation of the United Nations, Jakarta), Mark Stevenson, Sarai Silaban, Hanny Oey and Kona Adhikary (Melbourne Veterinary School, Faculty of Science, The University of Melbourne, Australia). 
}

\examples{
## EXAMPLE 1:
\dontrun{
library(sf)

## Set the directory from which to read the shape file sets:
mdir <- paste(.libPaths(), "/incursion/extdata/", sep = "")

## Map of Australia:
auadm1ll.sf <- st_read(dsn = mdir[1], layer = "auadm1-LL")

## auadm1ll.sf is an sf polygon object of Australian states 
## (i.e., adm1 areas). State names are listed in variable NAME 
## in the object attribute table. Correct the variables listed in the 
## attribute table of auadm1ll.sf:

names(auadm1ll.sf)

auadm1ll.sf <- auadm1ll.sf[,c(3,4)]
auadm1ll.sf$country <- "Australia"
names(auadm1ll.sf)[1:2] <- c("area","state")
auadm1ll.sf <- auadm1ll.sf[,c(1,2,4,3)]
head(auadm1ll.sf)

## Say you have a series of location names, some of which are incorrect:
oareas.df <- data.frame(
  area = c("Clark Island","Robins Island","Frenches Island","Kings island",
     "Finders Island"), 
  state = c("Tasmania","Tasmania","Victoria","Tasmania","Tasmania"),
  country = rep("Australia", times = 5))

rareas.df <- data.frame(
  area = auadm1ll.sf$area, 
  state = auadm1ll.sf$state,
  country = auadm1ll.sf$country)

## Corrections using fuzzy matching:
cname.df <- inc.fixanames(oareas.df = oareas.df, rareas.df = rareas.df, 
   rmap.sf = auadm1ll.sf, rareaname = c("area","state","country"), 
   max.dist = 0.10, use = "fuzzy", gengine = "osm", gkey = NA)
cname.df

## No matches. Change max.dist to 0.20:
cname.df <- inc.fixanames(oareas.df = oareas.df, rareas.df = rareas.df, 
   rmap.sf = auadm1ll.sf, rareaname = c("area","state","country"), 
   max.dist = 0.20, use = "fuzzy", gengine = "osm", gkey = NA)
cname.df

## All areas corrected.
}
}

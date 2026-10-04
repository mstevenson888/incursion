\name{inc.fdp}

\alias{inc.fdp}

\title{
First day prevalence
}

\description{
Returns a data frame of the within-place frequency of disease, expressed as first day prevalence.
}

\usage{
inc.fdp(ips, use = "species", conf.level = 0.95)
}

\arguments{
  \item{ips}{a data frame listing listing details of infected places. Columns (in order): unique place identifier, place type, infected place number, and: (1) the number of inviduals present, (2) the number of inviduals initially affected, and (3) the estimated age of clinical signs at time of examination. Counts can be provided for individual species (i.e. pigs, dairy cattle, beef cattle, sheep, deer, and goats) or for all susceptible species on an infected place. If the individual animal species format is used, data frame \code{ips} should have 21 columns (three columns for place identifier, place type and infected place number and then six groups of three columns listing the numbers of animals present, the number of animals with signs on first examination and mean age of clinical signs on first examination for pigs, dairy cattle, beef cattle, sheep, deer and goats, respectively). If the all suceptible animal species format is used, data frame \code{ips} should have six columns (three columns for place identifier, place type and infected place number and three columns listing the total number of susceptible animals present, the total number of animals with signs on first examination and mean age of clinical signs on first examination). There should be one row for each infected place.}
  
  \item{use}{a character string specifying which group to use as the denominator for the incidence risk calculations. Only used if the individual animal species data format is used (see above). Options are \code{species}: select the species with the oldest lesions and express first day incidence as the number of that species with signs divided by the total number of that species present on the infected place, \code{all}: select the species with the oldest lesions and express first day incidence as the number of that species with signs divided by the total number of susceptible species present on the infected place}
  
  \item{conf.level}{scalar, defining the magnitude of the returned confidence interval. Must be a single number between 0 and 1.}
}

\details{
First day incidence was a term coined by Hutber and Kitching (1996) to denote the number of animals showing clinical signs on the first day of a herd outbreak. The terminology used by Hutber and Kitching was incorrect --- assessment of disease status of individuals at a single point in time provides an estimate of disease prevalence (not incidence) so the function has been named accordingly.

First day prevalence provides an indication of the number of individuals infected at the time of examination. A low first day prevalence would suggest that the premises has been detected quickly. A high first day prevalence would indicate that the premises has been infected for some time and therefore of greater forward risk potential.

Confidence intervals for the prevalence estimates are calculated using the exact method (see Collett 1999, page 24).

The function takes each infected place in turn and chooses the species with the oldest clinical signs at the time of examination. First day prevalence equals the number of that species with signs divided by the total number of that species present on the infected place. Where there are two or more animal species with the oldest clinical signs the function chooses one of them at random.
}

\value{
A data frame containing the following:
  \item{placeid}{the unique place identifier (taken directly from the input data frame).}
  \item{ipid}{the infected premises number (taken directly from the input data frame).}
  \item{ncas}{number of individuals with signs on first examination.}
  \item{natrisk}{number of individuals at risk.}
  \item{est}{the point estimate of first day prevalence.}
  \item{lower}{the lower bound of first day prevalence.}
  \item{upper}{the upper bound of first day prevalence.}
}

\references{
Collett D (1999). Modelling Binary Data. Chapman & Hall/CRC, Boca Raton Florida, p. 24.

Hutber AM, Kitching RP (1996). The use of vector transition in modelling of intra-herd foot-and-mouth disease. Environmental and Ecological Statistics 3: 245 - 255. 
}

\examples{
## EXAMPLE 1:
data(inc.nzfmd)
ips <- inc.nzfmd$ips

## Individual animal species format:
ips.spp <- inc.nzfmd$ips[,c(1:3,11:28)]

rval.df <- inc.fdp(ips = ips.spp, use = "species", conf.level = 0.95)
rval.df$rank <- rank(rval.df$est, ties.method	= "random")
rval.df <- rval.df[sort.list(rval.df$rank),] 

\dontrun{
library(ggplot2)

ggplot(rval.df, aes(x = rank, y = est)) +
   theme_bw() + 
   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
   geom_point() +
   xlab("Rank") +
   ylab("First day prevalence")
}

## All suceptible animal species format:
no <- apply(ips[,c(11,14,17,20,23,26)], MARGIN = 1, FUN = sum)
ini <- apply(ips[,c(12,15,18,21,24,27)], MARGIN = 1, FUN = sum)
age <- apply(ips[,c(13,16,19,22,25,28)], MARGIN = 1, FUN = mean)
ips.all <- data.frame(ips[,1:3], no = no, ini = ini, age = age)

rval.df <- inc.fdp(ips = ips.all, conf.level = 0.95)
rval.df$rank <- rank(rval.df$est, ties.method	= "random")
rval.df <- rval.df[sort.list(rval.df$rank),] 

\dontrun{
library(ggplot2)

ggplot(rval.df, aes(x = rank, y = est)) + 
   theme_bw() +
   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
   geom_point() +
   xlab("Rank") +
   ylab("First day prevalence")
 }
}

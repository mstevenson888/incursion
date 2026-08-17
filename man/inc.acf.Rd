\name{inc.acf}

\alias{inc.acf}

\title{
Temporal autocorrelation in event frequency data
}

\description{
Returns the temporal autocorrelation for event counts as a function of time.
}

\usage{
inc.acf(x, minlag.n, maxlag.n, minlag.acf, maxlag.acf)
}

\arguments{
  \item{x}{a numeric vector listing event frequencies (e.g., the number of disease cases identified per day).}
  \item{minlag.n}{scalar, the minimum lag to be evaluated.}
  \item{maxlag.n}{scalar, the maximum lag to be evaluated.}
  \item{minlag.acf}{scalar, the expected ACF value at the minimum lag if this time series was consistent with that of an infectious disease.}
  \item{maxlag.acf}{scalar, the expected ACF value at the maximum lag if this time series was consistent with that of an infectious disease.}
}

\details{
The function returns a list with data suitable for a Moran scatter plot and for mapping.
}

\value{
A list containing:
  \item{acf}{a data frame listing the temporal lag \code{lag}, the autocorrelation estimate for each temporal lag \code{acf} and the expected  autocorrelation if this time series was consistent with that of an infectious disease \code{tacf}.}
  \item{residual}{the sum of the squared difference between \code{acf} and \code{tacf}.}
  \item{call}{a data frame listing \code{minlag.n}, \code{maxlag.n}, \code{minlag.acf} and \code{maxlag.acf} as entered by the user.}
}

\examples{
## EXAMPLE 1:
## FMD cases in Cumbria, United Kingdom, 2001.
\dontrun{
library(tidyverse); library(dplyr); library(ggplot2); library(scales)

data(inc.ukfmd)
ukfmd.df <- inc.ukfmd
head(ukfmd.df)

## Aggregate the data by day:
ukfmd.df <- ukfmd.df \%>\%
   mutate(cssdate = as.Date(cssdate, format = "\%Y-\%m-\%d")) \%>\%
   group_by(cssdate)
ukfmd.df <- data.frame(dplyr::summarise(ukfmd.df, ncas = n()))
head(ukfmd.df)

## Generate a data frame listing a sequence of calendar dates and the 
## number of disease events detected on each date (with zero for those 
## dates on which no disease events were detected:
ukfmd.df <- ukfmd.df \%>\%
   complete(cssdate = seq.Date(min(cssdate, na.rm = TRUE), 
    max(cssdate, na.rm = TRUE), by = "day")) \%>\%
   replace_na(list(ncas = 0))
ukfmd.df <- data.frame(ukfmd.df)
head(ukfmd.df)                                                                                         

## Plot the FMD data as an epidemic curve using ggplot2:
ggplot(data = ukfmd.df, aes(x = cssdate, weight = ncas)) +
   theme_bw() +
   geom_histogram(binwidth = 1, fill = "dark blue", colour = "gray") +
   scale_x_date(breaks = date_breaks("1 month"), 
    labels = date_format("\%b \%Y"), name = "Date of onset of signs") +
   scale_y_continuous(breaks = seq(from = 0, to = 20, by = 5), 
    limits = c(0,20), name = "Number of confirmed cases") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Assess the FMD time series for autocorrelation consistent with an 
## infectious disease:
ukfmd.acf <- inc.acf(x = ukfmd.df$ncas, minlag.n = 2, maxlag.n = 20, 
   minlag.acf = 0.65, maxlag.acf = 0.10)

## ACF plot with theoretical ACF shown as a solid red line:
ggplot(data = ukfmd.acf$acf, aes(x = lag, y = acf)) +
   theme_bw() +
   geom_segment(mapping = aes(xend = lag, yend = 0)) +
   geom_line(mapping = aes(x = lag, y = tacf), col = "red") +
   scale_x_continuous(breaks = 0:max(ukfmd.acf$acf$lag), name = "Lag") +
   scale_y_continuous(breaks = seq(from = -0.10, to = 1.0, by = 0.25), 
    limits = c(-0.10,1.0), name = "ACF") +
   geom_hline(yintercept = 0, linetype = "dashed")

## Sum of squared residuals:
ukfmd.acf$residual

 }
}


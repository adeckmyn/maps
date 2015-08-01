\name{iso3166}
\alias{iso3166}
\alias{ISOlist}
\title{
  Identify countries by ISO 3166-1 alpha-2 code
}
\description{
  This simple function performs a lookup in a table of 2-letter codes.
  }
\usage{
iso3166(x)
}
\arguments{
  \item{x}{A vector of 2-letter country codes}
}
\details{
  The ISO 3166-1 alpha-2 standard identifies countries by a 2 letter code. This function translates these codes into the country names as used by the \code{world} data base.
}
\value{
  A vector the same length as \code{x}, with the names of the countries.
}
\references{
\url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}
}
\examples{
iso3166(c("BE","GB","UK")) # yes, some countries have 2 ISO codes
map('world',iso3166("BE"))
}

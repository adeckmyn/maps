\name{iso3166}
\alias{sov.expand}
\alias{iso.expand}
\alias{iso.alpha}
\title{
  Identify countries by ISO 3166 codes (2 or 3 letters) or by Sovereignty.
}
\description{
  This data set and the simple look-up functions allow to build lists of counrtries for the world map.
  }
\usage{
	iso.expand(a)
        iso.alpha(x,n=2)
	sov.expand(sov)
}
\arguments{
  \item{a}{A vector of ISO codes. All elements should have the same length, either 2 or 3 letters. Not case sensitive.}
  \item{sov}{A vector of country names. The result is a list of all countries that fall under their sovereignty. Case sensitive, must fit completeley.}
  \item{x}{Vector of country names, may include colons.}
  \item{n}{An integer identitying which ISO code is required. Allowed values are 2 and 3.}
}
\details{
  The ISO 3166-1 standard identifies countries by a 2 and 3 letter codes. \code{iso.expand} translates these codes into the country names as used by the \code{world} data base. \code{iso.alpha} does the reverse.  Note that these functions will not work well with the legacy data base. Also, \code{iso.alpha} uses only the first part of the name, up to any colon. As a result, the few countries with seperate ISO codes but which are written as a sub-country (e.g. China:Hong Kong), may give unintended results.
}
\value{
  \code{iso.expand} returns vector of country names. When used as input for \code{map} it will plot all the countries as identified either by their sovereignty or by ISO codes. The length of the vector may be shorter or longer than the sum of the lengths of the three parameters. 
\code{ISOalpha} returns a vector of the same length as the input,  containing the 2- or 3-letter codes 
}
\references{
\url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}
}
\examples{
# France and all its overseas departments, territories etc.
sov.expand("France") # France and all its overseas departments, territories etc.
# Canary Islands are not included in map("Spain")
iso.expand("ES")
sov.expand("Spain")
}

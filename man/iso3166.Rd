\name{iso3166}
\alias{ISOexpand}
\alias{ISOalpha}
\title{
  Identify countries by ISO 3166 codes (2 or 3 letters) or by Sovereignty.
}
\description{
  This data set and the simple look-up functions allow to build lists of counrtries for the world map.
  }
\usage{
	ISOexpand(sov=NULL,a2=NULL,a3=NULL)
        ISOalpha(x,a=2)
}
\arguments{
  \item{x}{A vector of countries, not case sensitive. The countries may contain clons for sub-divisions, but only the first part is taken into account for getting the 2- or 3-letter ISO code.}
  \item{sov}{A vector of country names. The result is a list of all countries that fall under their sovereignty. Case sensitive, must fit completeley.
  \item(a2,a3}{Vector of 2-letter and 3-letter ISO country codes.}
  \item{a}{An integer identitying which ISO code is required. Allowed values are 2 and 3.}
}
\details{
  The ISO 3166-1 standard identifies countries by a 2 and 3 letter codes. This function translates these codes into the country names as used by the \code{world} data base. Note that these functions will not work well with the legacy data base.
}
\value{
  \code{ISOexpand} A vector of country names. When used as input for \code{map} it will plot all the countries as identified either by their sovereignty or by ISO codes. The length of the vector may be shorter or longer than the sum of the lengths of the three parameters. 
\code{ISOalpha} returns a vector of the same length as the input,  containing the 2- or 3-letter codes 
}
\references{
\url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}
}
\examples{
# France and all its overseas departments, territories etc.
ISOexpand(sov="France") # France and all its overseas departments, territories etc.
# Canary Islands are not included in map("Spain")
ISOexpand(a2="ES")
ISOexpand(sov=c("Netherlands","France","Spain"),a2=c("PT"))
}

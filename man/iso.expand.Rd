\name{iso.expand}
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
	iso.expand(a, regex=TRUE)
	sov.expand(sov, regex=TRUE)
        iso.alpha(x, n=2)
}
\arguments{
  \item{a}{A vector of ISO codes. All elements should have the same length, either 2 or 3 letters. Not case sensitive.}
  \item{sov}{A vector of country names. The result is a list of all countries that fall under their sovereignty. Case sensitive, must fit completeley.}
  \item{regex}{If TRUE (default), the return vector has the same length as the input (\code{a} or \code{sov}), but the entries may be regular expressions.
If FALSE, the result is a vector of polygon names. This may be more readable, but the return vector may be longer than the input.}
  \item{x}{Vector of country names, may include colons.}
  \item{n}{An integer identitying which ISO code is required. Allowed values are 2 and 3.}
}
\details{
  The ISO 3166-1 standard identifies countries by a 2 and 3 letter codes.
\code{iso.expand} translates these codes into the country names as used by 
the \code{world} data base. \code{iso.alpha} does the reverse.  
Note that these functions will not work well with the legacy world data base. 
Some countries have different ISO codes for different regions 
(e.g. China:Hong Kong has ISO code HK). In such cases, \code{iso.alpha} will 
return the main code, but \code{iso.expand} will return a regular expression 
that excludes some parts.
}
\value{
  \code{iso.expand} returns vector of country names. When used as input for \code{map} it will plot all the countries as identified either by their sovereignty or by ISO codes. If \code{regex=FALSE} the length of the vector may be shorter or longer than the input. If \code{regex=TRUE}, the results are concatenated in regular expressions. This format is less readable, but can be used as input e.g. for \code{match.map}.
\code{iso.alpha} always returns a vector of the same length as the input,  containing the 2- or 3-letter codes.
}
\section{NOTE}{
These functions use regular expressions and the results will often not work well with \code{map(...,exact=TRUE)}.
}
\seealso{
\code{\link{match.map}},\code{\link{map.text}},\code{\link{iso3166}}
}
\references{
\url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}
}
\examples{
# France and all its overseas departments, territories etc.
sov.expand("France") # France and all its overseas departments, territories etc.

# Canary Islands are not included in map("Spain")
iso.expand("ES")
map(regions=sov.expand("Spain"))

# draw a map with ISO codes as labels:
wm <- map("world", fill=TRUE, col=0, xlim=c(-10,40), ylim=c(30,60))
# take out islands, but you loose e.g. UK, New Zealand, small island states
nam <- grep(":", wm$names, inv=TRUE, val=TRUE)
# ad ISO codes as label
map.text(wm, regions=nam, label=iso.alpha(nam), col=2, exact=TRUE, add=TRUE)
}

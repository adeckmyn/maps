\name{iso3166}
\alias{iso3166}
\docType{data}
\title{
  ISO 3166 country codes (2 or 3 letters) and sovereignty.
}
\description{
  This data set lists all ISO3166 country codes and the sovereignty for each country in the list. Some entries are regular expressions.
}
\details{
  The ISO 3166-1 standard identifies countries by a 2 and 3 letter codes. This table listst these for all countries on the world map. This data set also serves as basis for the function iso.expand() and its siblings.}
\format{
A data frame with 5 columns: "a2", "a3", "name", "mapname", "sovereignty". These contain the 2- and 3-letter ISO code, the official name,  the (possibly shorter) name used in the map data base, and the sovereign country.
}
\section{NOTE}{
Some countries have different ISO codes for some regions. To deal with such particular cases, the "mapname" column may sometimes contain (perl-style) regular expressions rather than simply a country name. For instance, "FI" has mapname "Finland(?!:Aland)", because the Aland islands have a different ISO code. Other codes may appear in two rows if certain parts of countries are not written with the main country as base name. Usually, that is for compatibility with the legacy world data base. 
}

\seealso{
\code{\link{iso.expand}}
}
\references{
\url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}
}
\keyword{datasets}

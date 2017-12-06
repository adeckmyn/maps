\name{world.cities}
\docType{data}
\alias{world.cities}
\title{
Database of world cities
}

\description{
This is a database of world cities of population greater than
 5,000.  Also included are capital cities of any population size.

}
\usage{data(world.cities)}
\format{
A list with 7 components, namely "name", "country.etc", "pop", "lat",
"long", "capital" and "date", containing the city name, the country name,
approximate population, latitude, longitude and
capital status indication (0 for non-capital, 1 for capital, 2 for China
Municipalities, and 3 for China Provincial capitals). The last column gives the date on which the data was valid.
}
\details{
This data was obtained from Geonames \url{http://download.geonames.org/export/dump/cities5000.zip}.

It is freely available under the Creative Commons Attribution 3.0 license \url{http://creativecommons.org/licenses/by/3.0/}

}
\seealso{
\code{\link{map.cities}}
}
\keyword{datasets}

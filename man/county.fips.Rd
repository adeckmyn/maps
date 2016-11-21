\name{county.fips}
\docType{data}
\alias{county.fips}
\title{
FIPS county codes for US County Map
}
\description{
A database matching FIPS codes to maps package county and state names.
}
\usage{data(county.fips)}
\format{
A list with 2 components, namely "fips" and "polyname", containing the
FIPS number and respective state or county polygon name. Note that "fips" is represented as an integer, so any leading zero (which is part of the fips code) is not shown by default.
}
\seealso{
\code{\link{state.fips}}
}
\keyword{datasets}

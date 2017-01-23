\name{state.fips}
\docType{data}
\alias{state.fips}
\title{
FIPS state codes for US 48 State Map
}
\description{
A database matching FIPS codes to maps package state names.
}
\usage{data(state.fips)}
\format{
A list with 6 components, namely "fips", "ssa", "region", "division",
"abb" and "polyname", containing the US Census Bureau FIPS, SSA, REGION
and DIVISION numbers, the standard state abbreviation and the respective
state polygon name. Note that "fips" is represented as an integer, so any leading zero (which is part of the fips code) is not shown by default.
}
\seealso{
\code{\link{county.fips}}
}
\keyword{datasets}

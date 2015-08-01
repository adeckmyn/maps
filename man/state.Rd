\name{state}
\docType{data}
\alias{state}
\alias{stateMapEnv}
\title{
United States State Boundaries Map
}
\usage{data(stateMapEnv)}
\description{

This database produces a map of the states of the United States mainland
generated from US Department of the Census data (see the reference).
}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR} for the datasets in the
maps package) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.
}
\seealso{
\code{\link{map}}.
}
\examples{
map('state', fill = TRUE, col = palette())
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT\&T Bell Laboratories Statistics Research Report [93.2], 1993.}

Richard A. Becker, and Allan R. Wilks,
"Constructing a Geographical Database",
\emph{AT\&T Bell Laboratories Statistics Research Report [95.2], 1995.}

US Department of Commerce, Census Bureau,
\emph{County Boundary File,}
computer tape, available from Customer Services,
Bureau of the Census, Washingdon DC 20233.
}
\keyword{datasets}

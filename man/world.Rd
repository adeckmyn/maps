\name{world}
\docType{data}
\alias{world}
\alias{worldMapEnv}
\title{
Low (mid) resolution World Map
}
\usage{data(worldMapEnv)}
\description{
This updated world map is imported from the Natural Earth project (the 1:50m resolution version). It is up to date (2013) and replaces a much older version based on the CIA World Data Bank II data.
That older version is still included in the package and can reached as \code{legacy_world}.
}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR_WORLD}) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.

During a transition period (\code{maps} version 3.0) it is possible to switch to the old legacy map using the function \code{world.legacy(TRUE)} or by setting the setting the environment variable R_MAP_DATA_LEGACY=TRUE prior to loading the package.

}
\source{
The Natural Earth data set is available from \url{http://www.naturalearthdata.com}.

The legacy CIA World Data Bank II is currently (mid-2003) available from 
\url{https://www.evl.uic.edu/pape/data/WDB/}.
}
\seealso{
\code{\link{map}},\code{\link{world.legacy}},\code{\link{legacy_world}}
}
\examples{
map('world', fill = TRUE, col = 1:10)
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT\&T Bell Laboratories Statistics Research Report [93.2], 1993.}

Richard A. Becker, and Allan R. Wilks,
"Constructing a Geographical Database",
\emph{AT\&T Bell Laboratories Statistics Research Report [95.2], 1995.}
}
\keyword{datasets}

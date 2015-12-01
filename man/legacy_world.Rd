\name{legacy_world}
\docType{data}
\alias{legacy_world}
\alias{legacy_worldMapEnv}
\alias{legacy_world2}
\alias{legacy_world2MapEnv}
\title{ Legacy low resolution World Map.}
\usage{data(worldMapEnv)}
\description{
 This world database comes from a thinned cleaned-up version of the CIA World Data Bank II data and contains approximately 30,000 points representing the world coastlines and national boundaries approximatly around 1990.
}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR} for the datasets in the
maps package) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.

During a transition period (\code{maps} version 3.0) it is possible to switch to the old legacy map using the function \code{world.legacy(TRUE)} or by setting the setting the environment variable R_MAP_DATA_LEGACY=TRUE prior to loading the package.

}
\source{
The CIA World Data Bank II is currently (2015) still available from 
\url{https://www.evl.uic.edu/pape/data/WDB/}.
}
\seealso{
\code{\link{map}},\code{\link{world.legacy}},\code{\link{world}}
}
\examples{
map('legacy_world', fill = TRUE, col = 1:10)
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

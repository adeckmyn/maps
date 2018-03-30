\name{world}
\docType{data}
\alias{world}
\alias{worldMapEnv}
\title{
Low (mid) resolution World Map
}
\usage{data(worldMapEnv)}
\description{
This updated (2013) world map is imported from the Natural Earth project (the 1:50m resolution version). It replaces a much older version based on the CIA World Data Bank II data.
That older version is still temporarily included in the package (but deprecated) and can reached as \code{legacy_world}. In the future this old package will only be available via txpeh \code{mapdata} (v2.3.0).
}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR_WORLD}) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.

During a transition period (\code{maps} version 3.0--1) it is possible to switch to the old legacy map using the function \code{world.legacy(TRUE)} or by setting the setting the environment variable R_MAP_DATA_LEGACY=TRUE prior to loading the package.

}
\source{
The Natural Earth data set is available from \url{http://www.naturalearthdata.com}.
}
\details{
As of version 3.1, the \code{world} database no longer contains any lakes. These have been moved to a separate database called \code{lakes}.
}
\seealso{
\code{\link{map}},\code{\link{world.legacy}},\code{\link{legacy_world}},\code{\link{lakes}}
}
\examples{
# notice how some polygons extend beyond the [-180,180] interval:
map('world', fill = TRUE, col = 1:10)
# if you wrap at [-180,180], you also can get a clean closure of Antarctica
map('world', fill = TRUE, col = 1:10, wrap=c(-180,180) )
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

\name{world2}
\docType{data}
\alias{world2}
\alias{world2MapEnv}
\title{
Pacific Centric Low resolution World Map
}
\usage{data(world2MapEnv)}
\description{
This is an alternative version of the \code{world} database 
based on latitudes [0, 360), which then has the Pacific
Ocean in the centre of the map.
}
\section{NOTE}{
Almost the same effect can in fact be obtained by using a projection:

map("world",projection="rectangular",parameter=0,orientation=c(90,0,180),
             wrap=TRUE)

By changing the third component of 'orientation', the longitude shift can also be set to any other value besides 180 or 0 (but then fill=TRUE will usually give artefacts at the borders). You may even need to set 'resolution=0' if the wrapping algorithm fails because of the thinning.

This projected version will not work so well with \code{fill=TRUE}, though. There may be jagged edges at the boundaries.

}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR_WORLD} for the datasets in the
maps package) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.

During a transition period (\code{maps} version 3.0--1) it is possible to switch to the old legacy map using the function \code{world.legacy(TRUE)} or by setting the setting the environment variable R_MAP_DATA_LEGACY=TRUE prior to loading the package.

}
\source{
The Natural Earth data set is available from \url{http://www.naturalearthdata.com}.
}
\seealso{
\code{\link{map}}, \code{\link{world}},\code{\link{legacy_world2}}
}
\examples{
map('world2', xlim = c(100, 300))
map.axes()
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

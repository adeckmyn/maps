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
This data set is in fact largely obsolete. Often the same (more general) result can be obtained by using wrapping:

map("world", wrap=c(0,360))

This will also work fine with \code{fill=TRUE} or any other appropriate longitude interval (e.g. \code{c(-90,270)}).

However, \code{world2} is useful when setting \code{xlim} to an interval crossing the 180 meridian.
}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR_WORLD} for the datasets in the
maps package) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.
}
\source{
The Natural Earth data set is available from \url{http://www.naturalearthdata.com}.
}
\seealso{
\code{\link{map}}, \code{\link{world}}
}
\examples{
map('world2', xlim = c(100, 300))
map.axes()
# xlim is performed before wrapping:
map('world', wrap=c(0,360), xlim = c(100, 300))
# so to emulate "world2":
ww2 <- map('world', wrap=c(0,360), plot=FALSE, fill=TRUE)
map(ww2, xlim = c(100, 300), fill=TRUE)
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

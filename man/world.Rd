\name{world}
\docType{data}
\alias{world}
\alias{worldMapEnv}
\title{
Low (mid) resolution World Map
}
\usage{data(worldMapEnv)}
\description{
This world map (updated in 2013) is imported from the public domain Natural Earth project (the 1:50m resolution version). It replaces a much older version based on the CIA World Data Bank II data.
The old legacy data is still available in the package \code{mapdata} (v2.3.0).
}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR_WORLD}) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.

}
\source{
The Natural Earth data set is in the public domain and available from \url{https://www.naturalearthdata.com}.
}
\details{
As of version 3.1, the \code{world} database no longer contains any lakes. These have been moved to a separate database called \code{lakes}.
The legacy world map (dating from around 1990) has been removed from the package and is now available from the \code{mapdata} package in two different resolutions (worldHires and worldLores).
}
\seealso{
\code{\link{map}},\code{\link{lakes}}
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

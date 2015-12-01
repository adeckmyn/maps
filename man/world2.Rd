\name{world2}
\docType{data}
\alias{world2}
\alias{world2MapEnv}
\title{
Pacific Centric Low resolution World Map
}
\usage{data(world2MapEnv)}
\description{

This world database comes from a thinned cleaned-up version of the CIA
World Data Bank II data and contains approximately 30,000 points
representing the world coastlines and national boundaries.  The full
database of nearly 2 million points is found in the mapdata package.
This map is based on latitudes [0, 360), which then has the Pacific
Ocean in the centre of the map.

}
\section{NOTE}{
The same effect can in fact be obtained by using a projection:

map("world",projection="rectangular",parameter=0,orientation=c(90,0,180),
             wrap=TRUE)

By changing the third component of 'orientation', the longitude shift can also be set to any other value besides 180 or 0 (but then fill=TRUE will usually give artefacts at the borders). You may even need to set 'resolution=0' if the wrapping algorithm fails because of the thinning.

This method is in fact to be preferred, as it will also render correctly with fill=TRUE. Currently, the wrapping code does not work well with world2.

}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR_WORLD} for the datasets in the
maps package) is set at package load time \emph{if it does not
already exist}.  Hence setting the environment variable before loading
the package can override the default location of the binary datasets.

It is also (during a transition phase) possible to switch to the old legacy world map using the function \code{world.legacy()}.

}
\source{
The Natural Earth data set is available from \url{http://www.naturalearthdata.com}.

The legacy CIA World Data Bank II is currently (mid-2003) available from 
\url{https://www.evl.uic.edu/pape/data/WDB/}.
}
\seealso{
\code{\link{map}}, \code{\link{world}},\code{\link{world.legacy}}
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

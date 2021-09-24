\name{lakes}
\docType{data}
\alias{lakes}
\alias{lakesMapEnv}
\title{
World lakes database
}
\usage{data(lakesMapEnv)}
\description{

This database contains a selection of large lakes (and islands within) taken from the Natural Earth 1:50m map, the same data source as the (v3.0) world map. The lake boundaries are consistent with the 'world' database.
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
map('world')
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
}
\source{
The data in this data base is derived from the public domain GIS project Natural Earth, the file "ne_50m_lakes". The Natural Earth data set is available from \url{https://www.naturalearthdata.com}.}
\references{
\emph{Natural Earth project}
\url{https://www.naturalearthdata.com}
}
\keyword{datasets}

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
map('lakes', add=TRUE, fill=TRUE, col='white', boundary=1)
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT\&T Bell Laboratories Statistics Research Report [93.2], 1993.}

Richard A. Becker, and Allan R. Wilks,
"Constructing a Geographical Database",
\emph{AT\&T Bell Laboratories Statistics Research Report [95.2], 1995.}

\emph{Natural Earth project}
\url{http://www.naturalearthdata.com}

}
\keyword{datasets}

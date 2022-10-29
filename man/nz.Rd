\name{nz}
\docType{data}
\alias{nz}
\alias{nzMapEnv}
\title{
New Zealand Basic Map
}
\usage{data(nzMapEnv)}
\description{
This database produce a map of New Zealand at a basic level of
detail.  The `"nz"' database includes the 3 main Islands and 19 smaller
coastal islands.
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
\code{\link{map}}
}
\examples{
map('nz')
map('nz', xlim = c(166, 179), ylim = c(-48, -34))
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT&T Bell Laboratories Statistics Research Report [93.2], 1993.}

Richard A. Becker, and Allan R. Wilks,
"Constructing a Geographical Database",
\emph{AT&T Bell Laboratories Statistics Research Report [95.2], 1995.}
}
\keyword{datasets}

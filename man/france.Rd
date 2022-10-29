\name{france}
\docType{data}
\alias{france}
\alias{franceMapEnv}
\title{
France Map
}
\usage{data(franceMapEnv)}
\description{
This france database comes from the NUTS III (Tertiary Administrative
Units of the European Community) database of the United Nations
Environment Programme (UNEP) GRID-Geneva data sets. These were prepared
around 1989, and so may be somewhat out of date.

Users of data sets supplied through UNEP/GRID are requested to
incorporate in output products and reports acknowledgements to the
originator of the data and to the fact that they were acquired through
UNEP/GRID. Appropriate wording may be "UNESCO (1987) through
UNEP/GRID-Geneva".
}
\format{
The data file is merely a character string which
specifies the name of an environment variable which contains the
base location of the binary files used by the map drawing functions.
This environment variable (\code{R_MAP_DATA_DIR} for the datasets in the
maps package) is set at package load time \emph{if it does not
already exist}. Hence setting the environment variable before loading
the package can override the default location of the binary datasets.
}
\details{
  This map database can now easily be replaced by data taken directly from free sources, e.g. Natural Earth (see example).
}
\seealso{
\code{\link{map}}
}
\examples{
map('france', fill = TRUE, col = 1:10)
# replace by a public domain map at higher resolution:
# fr1 <- rnaturalearth::ne_states("france")
# this still includes overseas domains, so we remove those:
# france2 <- map(fr1, xlim=c(-20, 20), ylim=c(30, 60), lforce="e",
#                fill=TRUE, plot=FALSE)
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

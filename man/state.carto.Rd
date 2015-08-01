\name{state.carto}
\docType{data}
\alias{state.carto}
\alias{state.cartoMapEnv}
\alias{state.carto.center}
\title{
United States State Population Cartogram Map
}
\usage{
data(stateMapEnv)
data(state.carto.center)
}
\description{

This database produces a cartogram of the states of the United States
mainland based on CartoDraw, roughly proportional to population (see
references).

\code{state.carto.center} are coordinates of the state centers
for annotation purposes.
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
map('state.carto', fill = TRUE, col = palette())
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT\&T Bell Laboratories Statistics Research Report [93.2], 1993.}

Richard A. Becker, and Allan R. Wilks,
"Constructing a Geographical Database",
\emph{AT\&T Bell Laboratories Statistics Research Report [95.2], 1995.}

CartoDraw,
\url{http://www.computer.org/csdl/trans/tg/2004/01/v0095-abs.html}
}
\keyword{datasets}

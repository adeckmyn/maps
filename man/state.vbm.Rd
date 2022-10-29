\name{state.vbm}
\docType{data}
\alias{state.vbm}
\alias{state.vbmMapEnv}
\alias{state.vbm.center}
\title{
United States State Visibility Base Map
}
\usage{
data(state.vbmMapEnv)
data(state.vbm.center)
}
\description{

This database produces a map of the states of the United States
mainland. The Visibility Base Map was created by Mark Monmonier
to provide simplified state shapes with sufficient areas to allow
annotations in even the small states.

\code{state.vbm.center} are coordinates of the state centers
for annotation purposes. The states are alphabetically ordered, in the same order as the map. So state names can be matched via e.g. \code{map(state.vbm, plot=FALSE)$name}.
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
map('state.vbm', fill = TRUE, col = palette())
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT&T Bell Laboratories Statistics Research Report [93.2], 1993.}

Richard A. Becker, and Allan R. Wilks,
"Constructing a Geographical Database",
\emph{AT&T Bell Laboratories Statistics Research Report [95.2], 1995.}

Mark Monmonier and George Schnell,
"The Study of Population",
\emph{Elements, Patterns, Processes. Charles E. Merrill. Columbus, OH. 1982.}
}
\keyword{datasets}

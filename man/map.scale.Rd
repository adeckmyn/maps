\name{map.scale}
\alias{map.scale}
\title{
Add Scale to Existing Unprojected Map
}
\description{
Adds a scale to an existing map, both as a ratio and a distance gauge.
}
\section{NOTE}{
  This function is meaningful only if no projection= has been specified
  in the call to map().
}
\usage{
map.scale(x, y, relwidth = 0.15, metric = TRUE, ratio = TRUE, \dots)
}
\arguments{
\item{x}{
}
\item{y}{
Location of left end of distance gauge.  If not specified, this will be
taken to be near the lower left corner of the map.
}
\item{relwidth}{
Proportion of width of display to be used for the scale.  The default is
0.15 (15\%).
}
\item{metric}{
  If \code{TRUE}, the distance gauge will be in km, otherwise miles.
}
\item{ratio}{
  If \code{FALSE}, the scale ratio of the map is not displayed.
}
\item{...}{
Further plotting parameters may be specified as for the command text().
}}
\value{
The exact calculated scale is returned.
}
\section{Side Effects}{
A scale is added to the currently displayed map. This takes the form of
an approximate 1:n scale (containing 2-3 significant digits), above a
distance gauge which is reasonably accurate for the latitude at which it
appears.  The circumference at the given latitude is interpolated from a
radius of 6356.78 km at the pole and 6378.16 km at the equator.
}
\details{
The scale is calculated from the displayed graph's plotting parameters,
and the latitude of the location at which the distance gauge will be
displayed.
}
\seealso{
\code{\link{map.axes}}
}
\examples{
map("world", "China")
map.scale()
}
\keyword{hplot}

\author{Tom Minka}
\name{area.map}
\alias{area.map}
\title{
  Area of projected map regions
}
\usage{
area.map(m, regions = ".", sqmi=TRUE, ...)
}
\description{Computes the areas of regions in a projected map.}
\arguments{
  \item{m}{a map object containing named polygons (created with
  \code{fill = TRUE}).}
  \item{regions}{a character vector naming one of more regions, as in
    \code{\link{map}}.}
  \item{sqmi}{If \code{TRUE}, measure area in square miles.  Otherwise
    keep the units of \code{m}.}
  \item{...}{additional arguments to \code{\link{match.map}}}
}
\details{
  The area of each matching region in the map is computed, and
  regions which match the same element of \code{regions} have their
  areas combined.  Each region is assumed planar, with vertices
  specified by the \code{x} and \code{y} components of the map object.

  The correct use of this function is to first use \code{\link{map}} to
  create polygons and project the coordinates onto a plane, then apply
  \code{area.map} to compute the area of the projected regions.
  If the projection is area-preserving (such as \code{albers}),
  then these areas will match the area on the globe, up to a
  constant.  To get an absolute area in square miles, the \code{sqmi}
  option will scale the result, depending on the projection.

  The coordinates from \code{\link{map}} are affected by 
  its \code{resolution} argument, so use \code{resolution=0} for the most
  accurate areas.
}
\section{NOTE}{
  The \code{sqmi} option assumes the coordinates have been projected
  with the \link[mapproj]{mapproject} function.
}
\value{
  a named vector of region areas.
}
\seealso{\code{area.polygon}, \code{apply.polygon}}
\examples{
# because the projection is rectangular, these are not true areas on the globe.
m = map("state", fill = TRUE, plot = FALSE)
area.map(m)
area.map(m, ".*dakota")
area.map(m, c("North Dakota", "South Dakota"))

if(require(mapproj)) {
  # true areas on the globe
  m = map("state", proj="bonne", param=45, fill=TRUE, plot=FALSE)
  # North Dakota is listed as 70,704 square miles
  area.map(m, "North Dakota")
}
}
\keyword{dplot}

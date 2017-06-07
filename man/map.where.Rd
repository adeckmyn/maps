\author{Tom Minka}
\name{map.where}
\alias{map.where}
\title{
  Locate points on a map
}
\description{
  Returns the region names containing given locations.
}
\usage{
map.where(database = "world", x, y, ...)
}
\arguments{
  \item{database}{character string naming a geographical database, or a list of
    \code{x}, \code{y}, and \code{names}.
    See the documentation for \link{map} for more details.}
  \item{x}{vector of longitudes.}
  \item{y}{vector of latitudes.}
  \item{...}{Options for \code{SpatialPolygons2map}, only used if \code{database} is of type \code{SpatialPolygonsDataFrame}}.
}
\value{
  A list of character strings, naming the map region that each
  (longitude, latitude) pair falls into.
}
\seealso{\code{in.polygon}}
\note{
  This function may give erroneous results if the database contains
  enclaves. For instance, a point in San Marino may also be identified 
  as being in Italy.
}
\examples{
# NYC
map.where("state", -73.8, 41)
# Auckland
map.where("nz", 174.6, -36.92)
# find both in the world
map.where(x = c(174.6, -73.8), y = c(-36.92, 41))
# with a map object:
m = map("state", "new york", fill = TRUE, plot = FALSE)
map.where(m, -73.8, 41)
}
\keyword{iplot}

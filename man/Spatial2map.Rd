\name{Spatial2map}
\alias{Spatial2map}
\alias{SpatialPolygons2map}
\alias{SpatialLines2map}
\title{
  Read SpatialPolygons and SpatialLines objects
}
\description{
  These functions transform some classes provided by the package \code{sp} into a simple list that can be used by map().
}
\usage{
	SpatialPolygons2map(database, namefield=NULL)
	SpatialLines2map(database, namefield=NULL)
}
\arguments{
  \item{database}{A \code{SpatialPolygons} or \code{SpatialLines} object.}
  \item{namefield}{The name of a data column in \code{database} to be used for naming the polygons (or lines). If it is a vector of names, these are all used and separated by a colon ':'. Not case sensitive. So if the database contains columns that only differ by case, you get a warning and namefield is not used at all.}
}
\value{
A list with four components: \code{x, y, names, range}, similar to the return value of \code{map()}. This data can be used as a database for \code{map()}. The lines and polygons are separated by NA.}
\details{The 'map' list object only preserves co-ordinates and polygon names. All other information available in the original data is lost.

The option \code{namefield} is only taken into account if \code{database} is class \code{Spatial[]DataFrame}. \code{namefield} may be a vector of column names, e.g. to get polygons named as 'country:state'.}
\seealso{
\code{\link{map}},\code{\link[sp]{SpatialPolygons}} (in the \code{sp} library).
}

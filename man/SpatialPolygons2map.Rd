\name{SpatialPolygons2map}
\alias{SpatialPolygons2map}
\title{
  Read SpatialPolygons[DataFrame] objects
}
\description{
  This function transforms a spatial object containg polygons into a simple list that can be used by map()
  }
\usage{
	SpatialPolygons2map(database, a2code=NULL, namefield="name")
}
\arguments{
  \item{database}{A \code{SpatialPolygons} object.}
  \item{a2code}{A vector of ISO country codes to be selected from the databse. Default is to select all countries.}
  \item{namefield}{The name of a data column in \code{database} to be used for naming the polygons. }
}
\value{
A list with three components: \code{x, y, names}. The polygons are separated by NA. The options \code{a2code} and \code{namefield} are only taken into account if \code{database} is class \code{SpatialPolygonsDataFrame}.
}
\seealso{
\code{\link{map}}
}

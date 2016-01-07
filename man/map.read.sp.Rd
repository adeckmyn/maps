\name{map.read.sp}
\alias{map.read.sp}
\title{
  Read SpatialPolygonsDataFrame objects
}
\description{
  This function transforms a spatial object containg polygons into a simple list that can be used by map()
  }
\usage{
	map.read.sp(database, a2code=NULL, namefield="name")
}
\arguments{
  \item{database}{A \code{SpatialPolygonsDataFrame} object.}
  \item{a2code}{A vector of ISO country codes to be selected from the databse. Default is to select all countries.}
  \item{namefield}{The name of a data column in \code{database} to be used for naming the polygons. }
}
\value{
A list with three components: \code{x, y, names}. The polygons are separated by NA.
}
\seealso{
\code{\link{map}}
}

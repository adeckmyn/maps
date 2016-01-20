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
  \item{namefield}{The name of a data column in \code{database} to be used for naming the polygons. If it is a vector of names, these are all used and separated by a colon ':'. Not case sensitive. So if the database contains columns that only differ by case, you get a warning and namefield is not used at all.}
}
\value{
A list with four components: \code{x, y, names, range}. The polygons are separated by NA.}
\details{ The options \code{a2code} and \code{namefield} are only taken into account if \code{database} is class \code{SpatialPolygonsDataFrame}. \code{namefield} may be a vector of column names, e.g. to get polygons named as 'country:state'. }
\seealso{
\code{\link{map}}
}

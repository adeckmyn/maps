\name{map.cities}
\alias{map.cities}
\title{
Add Cities to Existing Map
}
\usage{
map.cities(x = world.cities, country = "", label = NULL, minpop = 0,
maxpop = Inf, capitals = 0, cex = par("cex"), projection = FALSE,
parameters = NULL, orientation = NULL, pch = 1, \dots)
}
\description{
  Adds city locations and (optionally) names to an existing map using a
  specified database.
}
\arguments{
\item{x}{
Name of database. See \code{\link{world.cities}} to determine the structure of
the database.
}
\item{country}{
If the string country is specified, limit the displayed cities to be
from within the specified country, province or state (depending on how
the database has been constructed).
}
\item{label}{
If \code{TRUE}, label all cities.  If \code{NULL},
the cities will be labelled unless there are 20 or more.
}
\item{minpop}{
The minimum value of population below which a particular city will not
be shown.
}
\item{maxpop}{
The maximum value of population above which a particular city will not
be shown.
}
\item{capitals}{
Selection of capitals-only display. Capitals may be 1 (country capital),
2 (provincial, state, or regional capital) or 3 (local capital). See
\code{\link{world.cities}} for further information.
}
\item{cex}{
The value of cex acts to override the current value of character size
expansion.
}
\item{projection}{
Boolean or character value.  If \code{FALSE} (the default), no
projection is assumed, if \code{TRUE}, the previous projection is used,
otherwise a character string that names a map projection to use.
See \code{\link[mapproj]{mapproject}} (in the \code{mapproj} library).
}
\item{parameters}{
numeric vector of parameters for use with the \code{projection}
argument.  This argument is optional only in the sense that certain
projections do not require additional parameters.  If a projection
does require additional parameters, these must be given in the
\code{parameters} argument.
}
\item{orientation}{
a vector \code{c(latitude, longitude, rotation)} describing where
the map should be centered and a clockwise rotation (in degrees)
about this center.
}
\item{pch}{
plotting character to use for marking city location. See
\code{\link{points}} for options.
}
\item{...}{
Further plotting parameters may be specified as for the commands
\code{\link{points}} and \code{\link{text}}.
}}
\value{
No value is returned from map.cities.
}
\section{Side Effects}{
All cities within the boundaries of the plot containing the current map
are added to the plot.  Note that it is possible that the boundaries of
the plot exceed the boundaries of the map requested, and so more cities
than were expected might be shown.
}
\details{
The database is searched for all cities matching the specified criteria
and fitting within the limits of the plot currently displayed.  The
default database is of all cities that have a population greater than a
certain threshold or which are capital cities of a country or island
territory.  The threshold varies from country to country, but in
general is no higher than about 40,000.  The data were originally obtained from Stefan Helders' website (http://www.world-gazetteer.com), which no longer exists. There are no recent updates available.

There are three supplied databases, world.cities (the default), us.cities
and canada.cities.  The latter two, which need to be made available by
using a \code{'data()'} call, include the state or province name with the
city name (thanks to John Woodruff \email{jpwoodruff@irisinternet.net}
for the state and province information).

Note that if the underlying map is "Pacific-centric", i.e. longitudes
exceed 180 degrees, and a projection is used, then the map.cities data
must be transformed appropriately.
}
\seealso{
\code{\link{world.cities}}, \code{\link{canada.cities}},
\code{\link{us.cities}}
}
\examples{
map("world", "China")
map.cities(country = "China", capitals = 2)
map("state", "New Jersey")
data(us.cities)
map.cities(us.cities, country="NJ")
}
\keyword{hplot}

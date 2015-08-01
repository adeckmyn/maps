\author{Tom Minka}
\name{map.text}
\alias{map.text}
\title{
  Draw a map with labeled regions
}
\description{
  Like \code{\link{map}}, but labels the regions.
}
\usage{
map.text(database, regions = ".", exact = FALSE, labels, cex = 0.75,
add = FALSE, move = FALSE, ...)
}
\arguments{
  \item{database}{character string naming a geographical database, or a list of
    \code{x}, \code{y}, and \code{names} obtained from a previous call to
    \code{map}.}
  \item{regions}{character vector that names the polygons to draw.}
  \item{exact}{If 'TRUE', only exact matches with 'regions' are selected
    for drawing.}
  \item{labels}{character vector of labels, one for each region
    selected.  Defaults to the names in the database.}
  \item{cex}{character expansion factor.}
  \item{add}{If \code{FALSE}, a map is drawn, then labels placed on top.  If
    \code{TRUE}, labels are added to the existing map.}
  \item{move}{If \code{TRUE}, labels are moved so that they don't
    overlap.  Requires the \code{mining} library (not in CRAN, contact
    tpminka@media.mit.edu).}
  \item{...}{Other arguments are the same as in \code{\link{map}}.}
}
\value{
  If \code{add = FALSE}, a map is drawn by calling \code{\link{map}}.
  Then the label for each region is placed at the centroid of the region
  polygon.

  The return value is a map object, as from \code{\link{map}}.
}
\examples{
map.text("world", "ira")    # iran and iraq
map.text("state", "penn")
map.text("county", "penn")  # Pennsylvania counties
map.text("county", "new jersey")  # New Jersey counties
}
\keyword{hplot}

\author{Tom Minka}
\name{identify.map}
\alias{identify.map}
\title{
  Identify regions on a map
}
\description{
  Identifies the map regions clicked by the user.
}
\usage{
\method{identify}{map}(x, n = 1, index = FALSE, ...)
}
\arguments{
  \item{x}{a map object containing named polygons.}
  \item{n}{the number of clicks to wait for.}
  \item{index}{If TRUE, returns the index of the polygon, rather than
    its name.}
  \item{...}{additional arguments passed to \code{\link{identify.default}}.}
}
\details{
  The current algorithm is somewhat crude --- selects the region whose
  centroid is closest to the click.  A more sophisticated approach would
  use \code{\link{map.where}}.
}
\value{
  a character vector of length \code{n}, naming the selected regions.
}
\seealso{\code{\link{identify}}, \code{\link{map.where}}}
\examples{
identify(map("state", fill = TRUE, col = 0))
if(require(mapproj))
  identify(map("world", proj = "lagrange", fill = TRUE, col = 0))
}
\keyword{iplot}

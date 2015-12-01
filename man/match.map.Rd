\author{Tom Minka}
\name{match.map}
\alias{match.map}
\title{
Index map regions
}
\description{
Assigns an index to each map region, useful for map coloring.
}
\usage{
match.map(database, regions, exact = FALSE, warn = TRUE)
}
\arguments{
  \item{database}{
    character string naming a geographical database, or a map object.
    See the documentation for \code{\link{map}} for more details.
  }
  \item{regions}{
    a vector of names, or more generally regular expressions
    to match against the map region names.
  }
  \item{exact}{If \code{TRUE}, only exact matches with \code{regions}
    are considered.  Otherwise each element of \code{regions} is assumed
    to be a regular expression.
    Matches are always case-insensitive.}
  \item{warn}{If \code{TRUE}, a warning is printed when an element of
    \code{regions} matches nothing in the map.}
}
\value{
  Returns an integer vector giving an index to each region in the database.
  The index is the index of the string in \code{regions} which matches the
  region name.  Matching is done as in \code{\link{map}}.
  More specifically, all regions \code{r} whose name matches 
  \code{regions[i]} will have index \code{i}.
  Unmatched regions will have index \code{NA}.
  Overlapping matches cause an error.

  This behavior differs from \code{\link{pmatch}} because a single entry
  in \code{regions} may match several entries in the map.
}
\seealso{\code{\link{grep}}}
\examples{
# filled map showing Republican vote in 1900
# (figure 6 in the reference)
data(state, package = "datasets")
data(votes.repub)
state.to.map <- match.map("state", state.name)
x <- votes.repub[state.to.map, "1900"]
gray.colors <- function(n) gray(rev(0:(n - 1))/n)
color <- gray.colors(100)[floor(x)]
map("state", fill = TRUE, col = color); map("state", add = TRUE)
}
\references{
Richard A. Becker, and Allan R. Wilks,
"Maps in S",
\emph{AT\&T Bell Laboratories Statistics Research Report, 1991.}
\url{http://ect.bell-labs.com/sl/doc/93.2.ps}
}
\keyword{dplot}

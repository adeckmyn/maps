\author{Tom Minka}
\name{smooth.map}
\alias{smooth.map}
\title{
  Smooth out aggregated data
}
\description{
  Increases the resolution of data aggregated over map regions,
  by either smoothing or interpolation.
  Also fills in missing values.
}
\usage{
smooth.map(m, z, res = 50, span = 1/10, averages = FALSE, type = c("smooth",
"interp"), merge = FALSE)
}
\arguments{
  \item{m}{a map object}
  \item{z}{a named vector}
  \item{res}{a vector of length two, specifying the resolution of the
    sampling grid in
    each dimension.  If a single number, it is taken as the vertical
    resolution, with double taken as the horizontal resolution.}
  \item{span}{kernel parameter (larger = smoother).
    \code{span = Inf} is a special case which invokes the cubic spline kernel.
    \code{span} is automatically scaled by the map size, and is
    independent of \code{res}.}
  \item{averages}{If \code{TRUE}, the values in \code{z} are interpreted
    as averages over the regions.  Otherwise they are interpreted as totals.}
  \item{type}{see details.}
  \item{merge}{If \code{TRUE}, a region named in \code{z} includes
    all matching regions in the map (according to
    \code{\link{match.map}}).
    If \code{FALSE}, a region named in
    \code{z} is assumed to refer to exactly one region on the map.}
}
\details{
  For \code{type = "smooth"}, the region totals are first converted
  into point measurements on the
  sampling grid, by dividing the total for a region among all sample
  points inside it.  Then it is a regular kernel smoothing 
  problem.  Note that the region totals are not preserved.
  
  The prediction \eqn{z_o}{zo} for
  location \eqn{x_o}{xo} (a vector) is the average of \code{z} for
  nearby sample points:
  
  \deqn{z_o = \frac{\sum_x k(x, x_o) z(x)}{\sum_x k(x, x_o)}}{%
    zo = (sum_x k(x, xo) z(x))/(sum_x k(x, xo))}
  \deqn{k(x, x_o) = exp(-\lambda ||x - x_o||^2)}{%
    k(x, xo) = exp(-lambda ||x - xo||^2)}
  \eqn{\lambda}{lambda} is determined from \code{span}.
  Note that \eqn{x_o}{xo} is over the same sampling grid as
  \eqn{x}{x}, but \eqn{z_o}{zo} is not necessarily the same as
  \eqn{z(x_o)}{z(xo)}.
  
  For \code{type = "interp"}, the region totals are preserved by the
  higher-resolution function.
  The function is assumed to come from a
  Gaussian process with kernel \eqn{k}{k}.  The measurement \code{z[r]}
  is assumed to be the sum of the function over the discrete sample
  points inside region \code{r}.
  This leads to a simple formula for the covariance matrix of \code{z}
  and the cross-covariance between \code{zo} and \code{z}.
  The prediction is the cross-covariance times the inverse covariance
  times \code{z}.  Unlike Tobler's method, the predictions are not
  constrained to live within the original data range,
  so there tends to be "ringing" effects.

  See the references for more details.
}
\value{
  A data frame with columns \code{x}, \code{y}, and \code{z}
  giving the smoothed value \code{z} for locations (x, y).
  Currently the (x, y) values form a grid, but this is not
  guaranteed in the future.
}
\references{
  W.F. Eddy and A. Mockus. 
  An example of the estimation and display of a smoothly varying
  function of time and space - the incidence of disease mumps.
  \emph{Journal of the American Society for Information Science},
  45(9):686-693, 1994.
  \url{https://web.eecs.utk.edu/~audris/papers/jasis.pdf}
  
  W. R. Tobler.  Smooth pycnophylactic interpolation for
  geographical regions.  \emph{Journal of the American Statistical
  Association} 74:519-530, 1979.
}
\examples{
# compare to the example for match.map
data(state, package = "datasets")
data(votes.repub)
z = votes.repub[, "1900"]
m = map("state", fill = TRUE, plot = FALSE)
# use a small span to fill in, but not smooth, the data
# increase the resolution to get better results
fit = smooth.map(m, z, span = 1/100, merge = TRUE, ave = TRUE)
mat = tapply(fit$z, fit[1:2], mean)
gray.colors <- function(n) gray(rev(0:(n - 1))/n)
par(bg = "blue")
filled.contour(mat, color.palette = gray.colors, nlev = 32, asp = 1)
# another way to visualize:
image(mat, col = gray.colors(100))

# for a higher degree of smoothing:
# fit = smooth.map(m, z, merge = TRUE, ave = TRUE)
# interpolation, state averages are preserved:
# fit = smooth.map(m, z, merge = TRUE, ave = TRUE, type = "interp")
}
\keyword{dplot}

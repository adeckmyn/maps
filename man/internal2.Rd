\name{internal2}
\alias{internal2}
\alias{char.to.ascii}
\alias{is.regexp}
\alias{indicators.factor}
\alias{insert}
\alias{match.map.slow}
\alias{match.map.grep}
\alias{map.poly}
\alias{map.wrap}
\alias{map.wrap.poly}
\alias{map.clip.poly}
\alias{subgroup}
\alias{gp.smooth}
\alias{kernel.smooth}
\alias{kernel.region.region}
\alias{kernel.region.x}
\alias{.map.range}
\title{Internally Required Functions}

\usage{
char.to.ascii(s)
is.regexp(s)
indicators.factor(y)
insert(x, i, v)
match.map.slow(nam, regions, warn = FALSE)
match.map.grep(nam, regions, warn = FALSE)
map.poly(database, regions = ".", exact = FALSE, xlim = NULL, ylim = NULL,
         boundary = TRUE, interior = TRUE, fill = FALSE, as.polygon = FALSE,
         namefield="name")
map.wrap(p, xlim=NULL)
map.wrap.poly(data, xlim, poly = FALSE, antarctica = -89.5)
map.clip.poly(data, xlim = c(NA, NA), ylim = c(NA, NA), poly = FALSE)
subgroup(x, i)
gp.smooth(x, z, xo, lambda, r)
kernel.smooth(x, z, xo, lambda, region = NULL, normalize = TRUE)
kernel.region.region(x, region, lambda)
kernel.region.x(x, region, z, lambda)
.map.range(new)
}

\description{
These functions are called internally and will generally not be
required by the user.
}

\keyword{internal}

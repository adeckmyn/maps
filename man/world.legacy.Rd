\name{world.legacy}
\alias{world.legacy}
\title{Legacy world map}
\description{A function to switch the "world" map to the old (1990) version and back.}
\usage{
world.legacy(wl=FALSE)
}
\arguments{
\item{wl}{Set to TRUE to switch to the old world map. Set FALSE to switch back to the new map. In either case, the old legacy map can always be reached as \code{legacy_world}.}
}
\details{
This function switches the "world" database to the old (1990) version. It is provided temporarily for cases where the new updated world map causes problems. In this way, code may run unchanged and still use the legacy world data base. It is highly recommended, though, to use this possibility only as a last resource. Addressing \code{legacy_world} directly or updating code to use the new data base is th erecommended option.

The new version is updated to about 2013, so has many new countries that did not exist in 1990 (e.g. in Central and Eastern Europe).

This function may be removed from the \code{maps} package in future releases. The legacy world database is also available directly as \code{legacy_world}.
}
\seealso{\code{\link{map}},\code{\link{world}},\code{\link{legacy_world}}}

\name{map}
\alias{map}
\title{
  Draw Geographical Maps
}
\description{Draw lines and polygons as specified by a map database.}
\usage{
map(database = "world", regions = ".", exact = FALSE, boundary = TRUE,
  interior = TRUE, projection = "", parameters = NULL, orientation = NULL,
  fill = FALSE, col = 1, plot = TRUE, add = FALSE, namesonly = FALSE,
  xlim = NULL, ylim = NULL, wrap = FALSE, resolution = if (plot) 1 else 0,
  type = "l", bg = par("bg"), mar = c(4.1, 4.1, par("mar")[3], 0.1),
  myborder = 0.01, namefield="name", lforce="n", ...)
}
\arguments{
  \item{database}{
    character string naming a geographical database, a list of
    \code{x}, \code{y}, and \code{names} obtained from a previous call to
    \code{map} or a spatial object of class \code{SpatialPolygons} or \code{SpatialLines}.
    The string choices include a \code{\link{world}} map,
    three USA databases (\code{\link{usa}}, \code{\link{state}},
    \code{\link{county}}), and more (type \code{help(package='maps')} to see the package index). If the required database is in a different package that has not been attached, the string may be started with "packagename::". The
    location of the map databases may be overridden by setting the
    \code{R_MAP_DATA_DIR} environment variable.
  }
  \item{regions}{
    character vector that names the polygons to draw.
    Each database is composed of a collection of polygons, and each polygon has
    a unique name.
    When a region is composed of more than one polygon, the individual polygons
    have the name of the region, followed by a colon and a qualifier,
    as in \code{michigan:north} and \code{michigan:south}.
    Each element of \code{regions} is matched
    against the polygon names in the database and, according to
    \code{exact}, a subset is selected for drawing. The regions may also be 
    defined using (perl) regular expressions. This makes it possible to use 
    'negative' expressions like \code{"Norway(?!:Svalbard)"}, which means Norway
    and all islands except Svalbard. All entries are case insensitive.
    The default selects all polygons in the database.
  }
  \item{exact}{
    If \code{TRUE}, only exact matches with \code{regions} are
    selected for drawing.
    If \code{FALSE}, each element of \code{regions} is matched
    as a regular expression
    against the polygon names in the database and all matches are selected
    for drawing.
  }
  \item{boundary}{
    If \code{FALSE}, boundary segments are not drawn.
    A boundary segment is a line segment of the map that bounds only
    one of the polygons to be drawn.
    This argument is ignored if \code{fill} is \code{TRUE}.
  }
  \item{interior}{
    If \code{FALSE}, interior segments are not drawn.
    An interior segment is a line segment of the map that bounds two
    of the polygons to be drawn.
    This argument is ignored if \code{fill} is \code{TRUE}.
  }
  \item{projection}{
    character string that names a map projection to use.
    See \code{\link[mapproj]{mapproject}} (in the \code{mapproj} library).
    The default is to use a rectangular projection with the aspect ratio
    chosen so that longitude and latitude scales are equivalent at the
    center of the picture.
  }
  \item{parameters}{
    numeric vector of parameters for use with the
    \code{projection} argument.
    This argument is optional only in the sense that certain
    projections do not require additional parameters.
    If a projection does require additional parameters, these
    must be given in the \code{parameters} argument.
  }
  \item{orientation}{
    a vector \code{c(latitude, longitude, rotation)} describing where
    the map should be centered and a clockwise rotation (in degrees)
    about this center.
  }
  \item{fill}{
    logical flag that says whether to draw lines or fill areas.
    If \code{FALSE}, the lines bounding each region will be drawn (but
    only once, for interior lines).
    If \code{TRUE}, each region will be filled using colors from the
    \code{col =} argument, and bounding lines are drawn by default using \code{par("fg")}.
    To hide the bounding lines, use \code{border=NA} (see \code{...}).
  }
  \item{col}{
    vector of colors.
    If \code{fill} is \code{FALSE}, the first color is used for plotting
    all lines, and any other colors are ignored.
    Otherwise, the colors are matched one-one with the polygons
    that get selected by the \code{region} argument (and are reused
    cyclically, if necessary). If \code{fill = TRUE}, the default boundary line colour 
    is given by \code{par("fg")}. To change this, you can use the \code{border} 
    argument (see '...').
    A color of \code{NA} causes the corresponding region to be deleted
    from the list of polygons to be drawn.
    Polygon colors are assigned \emph{after} polygons are deleted due to values
    of the \code{xlim} and \code{ylim} arguments.
  }
  \item{plot}{
    logical flag that specifies whether plotting
    should be done.
    If \code{plot} is \code{TRUE} the return value of \code{map} will
    not be printed automatically .
  }
  \item{add}{
    logical flag that specifies whether to add to the
    current plot.
    If \code{FALSE}, a new plot is begun, and a new coordinate
    system is set up.
  }
  \item{namesonly}{
    If \code{TRUE}, the return value will be a character vector of
    the names of the selected polygons.  See the Value section below.
  }
  \item{xlim}{
    two element numeric
    vector giving a range of longitudes, expressed
    in degrees, to which drawing
    should be restricted.
    Longitude is measured in degrees east of Greenwich, so that, in particular,
    locations in the USA have negative longitude.
    If \code{fill = TRUE}, polygons selected by \code{region} must be entirely inside
    the \code{xlim} range.
    The default value of this argument spans the entire longitude range
    of the \code{database}.
  }
  \item{ylim}{
    two element 
    numeric vector giving a range of latitudes,
    expressed in degrees, to which drawing
    should be restricted.
    Latitude is measured in degrees north of the
    equator, so that, in particular,
    locations in the USA have positive latitude.
    If \code{fill = TRUE}, polygons selected by \code{region} must be entirely inside
    the \code{ylim} range.
    The default value of this argument spans the entire latitude range
    of the \code{database}.
  }
  \item{wrap}{Boolean or a numeric vector. If TRUE, lines that cross too far across the map
    (due to a strange projection) are omitted. If wrap is a vector of length 2 or more, it is interpreted as the longitude range to be used for a global map, e.g. \code{c(-180,180)} or \code{c(0,360)}. This wrapping even works when \code{fill=TRUE} and is performed before any projection (so the range must always be in degrees). However, the wrapping is performed \emph{after} \code{xlim, ylim} are applied, so these options should probably never be combined. If there is a third component, this signifies the latitude at which Antarctica will be "closed". The default value is -89.9. Special values are \code{NA} (don't draw Antarctica at all) and 0 (draw the line at the latitude of the extremal points, not at a fixed lower latitude). }
  \item{resolution}{
    number that specifies the resolution with which
    to draw the map.
    Resolution 0 is the full resolution of the database.
    Otherwise, just before polylines are plotted they are thinned:
    roughly speaking, successive points on the polyline that are
    within \code{resolution} device pixels of one another are collapsed
    to a single point (see the Reference for further details).
    Thinning is not performed if \code{plot = FALSE} or when polygons are drawn (\code{fill = TRUE} or \code{database} is a list of polygons).
  }
  \item{type}{
    character string that controls drawing of the map.
    Aside from the default \code{type = "l"}, the value \code{type = "n"}
    can be used
    to set up the coordinate system and projection for a map that will
    be added to in later calls.
  }
  \item{bg}{
    background color.
  }
  \item{mar}{
    margins, as in \code{\link{par}}.  Defaults allow for map.axes().
  }
  \item{myborder}{
    scalar or vector of length 2 specifying the porportion of the plot
    to add to the defined or computed limits as borders.
  }
  \item{namefield}{
    A vector of column names to be used as region name if \code{database} is a \code{SpatialPolygonsDataFrame}. Ignored in all other cases.
  }
  \item{lforce}{Limit enforcement. Only taken into account if \code{xlim} and/or \code{ylim} are defined and (for "s" and "l") a projection is used. Possible values are \code{"n"} (none) \code{"e"} (exact),\code{"s"} (small),\code{"l"} (large). 
If \code{lforce="e"}, map data is restricted exactly to the given limits prior to projection. This option affects the output value of the map, not only the plot window.
If \code{lforce="s"}, the four corners defined by \code{xlim} and \code{ylim} are projected and the plot window is restricted to the largest rectangle inside this region. 
\code{"l"} will result in a larger rectangle that includes the four corners. However, the parts that fall outside the original limits may not be complete (only polygons that partially fall inside the boundaries are included). This will also work with \code{fill=TRUE}. In this case the output value of the data is not changed, only the plot window is affected. These options are only useful for some projections.}
  \item{...}{Extra arguments passed to \code{polygon} or \code{lines}. Of particular interest may be the options \code{border} and\code{lty} that control the color and line type of the polygon borders when \code{fill = TRUE}.}
}
\details{
The simplest form of use of this function is:
\preformatted{map(mymap)}
where \code{mymap} is the returned value from a previous call to
\code{map()}.
}
\value{
  If \code{plot = TRUE}, a plot is made where 
  the polygons selected from \code{database}, through the
  \code{regions}, \code{xlim}, and \code{ylim} arguments, are outlined
  (\code{fill} is \code{FALSE}) or filled (\code{fill} is \code{TRUE})
  with the colors in \code{col}.  

  The return value is a list with
  \code{x}, \code{y}, \code{range}, and \code{names} components.  
  This object can be used as a \code{database} for successive calls
  to \code{map} and functions.
  If \code{fill} is \code{FALSE}, the \code{x} and \code{y} vectors are
  the coordinates of successive polylines, separated by \code{NA}s.  If
  \code{fill} is \code{TRUE}, the \code{x} and \code{y} vectors have
  coordinates of successive polygons, again separated by \code{NA}s.
  Thus the return value can be handed directly to \code{lines} or
  \code{polygon}, as appropriate.

  When \code{namesonly} is \code{TRUE}, only the names component is returned.

  After a call to \code{map} for which the \code{projection} argument was
  specified there will be a global variable \code{.Last.projection}
  containing information about the projection used.
  This will be consulted in subsequent calls to \code{map} which use
  \code{projection = ''}.
}
\examples{
map()	# low resolution map of the world
map(wrap = c(0,360), fill = TRUE, col = 2) # pacific-centered map of the world
map(wrap = c(0, 360, NA), fill = TRUE, col = 2) # idem, without Antarctica
map('usa')	# national boundaries
map('county', 'new jersey')	# county map of New Jersey
map('state', region = c('new york', 'new jersey', 'penn'))	# map of three states
map("state", ".*dakota", myborder = 0)	# map of the dakotas
map.axes()				# show the effect of myborder = 0
if(require(mapproj))
  map('state', proj = 'bonne', param = 45)	# Bonne equal-area projection of states

# names of the San Juan islands in Washington state
map('county', 'washington,san', names = TRUE, plot = FALSE)

# national boundaries in one linetype, states in another
# (figure 5 in the reference)
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)

# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
if(require(mapproj)) {	# mapproj is used for  projection="polyconic"
  # color US county map by 2009 unemployment rate
  # match counties to map using FIPS county codes
  # Based on J's solution to the "Choropleth Challenge"
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

  # load data
  # unemp includes data for some counties not on the "lower 48 states" county
  # map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
  #  cities
  data(unemp)
  data(county.fips)

  # define color buckets
  colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
  unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
  leg.txt <- c("<2\%", "2-4\%", "4-6\%", "6-8\%", "8-10\%", ">10\%")

  # align data with map definitions by (partial) matching state,county
  # names, which include multiple polygons for some counties
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
    county.fips$polyname)]
  colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]

  # draw map
  map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
  map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")
  title("unemployment by county, 2009")
  legend("topright", leg.txt, horiz = TRUE, fill = colors)

  # Choropleth Challenge example, based on J's solution, see:
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  # To see the faint county boundaries, use RGui menu:  File/SaveAs/PDF
}
}
\seealso{
  \code{\link{map.text}}, \code{\link{map.axes}},
  \code{\link{map.scale}}, \code{\link[mapproj]{map.grid}} (in the \code{mapproj} library),
  \code{\link[graphics]{polygon}}, \code{\link{SpatialPolygons2map}}
}
\references{
  Richard A. Becker, and Allan R. Wilks,
  "Maps in S",
  \emph{AT&T Bell Laboratories Statistics Research Report [93.2], 1993.}
  \url{https://web.archive.org/web/20050825145143/http://www.research.att.com/areas/stat/doc/93.2.ps}

  Richard A. Becker, and Allan R. Wilks,
  "Constructing a Geographical Database",
  \emph{AT&T Bell Laboratories Statistics Research Report [95.2], 1995.}
  \url{https://web.archive.org/web/20050825145143/http://www.research.att.com/areas/stat/doc/95.2.ps}
}
\keyword{hplot}
% Converted by Sd2Rd version 0.3-3.

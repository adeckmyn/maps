"mapthin" <-
function(xy, delta, symmetric = TRUE)
{
  x <- xy$x
  y <- xy$y
  xy <- .C("mapthin", PACKAGE="maps",
    x = as.double(x),
    y = as.double(y),
    n = as.integer(length(x)),
    as.double(delta),
    as.integer(symmetric),
    NAOK = TRUE)[c("x", "y", "n")]
  length(xy$x) <- xy$n
  length(xy$y) <- xy$n
  xy[c("x", "y")]
}

# add axes to a map
"map.axes" <-
function(...)
{
  axis(1, ...)
  axis(2, ...)
  box(...)
  invisible()
}

"map.cities" <-
function (x = world.cities, country = "", label = NULL, minpop = 0, 
  maxpop = Inf, capitals = 0, cex = par("cex"), projection = FALSE,
  parameters = NULL, orientation = NULL, pch = 1, ...) 
{
  if (missing(x)) {
    # data("world.cities", package = "maps")	# uses lazy evaluation
    world.cities <- get("world.cities")
  }
  usr <- par("usr")
  if (!missing(projection) && projection != FALSE) {
    if (requireNamespace("mapproj", quietly = TRUE)) {
      if (is.character(projection)) {
        projx <- mapproj::mapproject(x$long, x$lat, projection = projection,
          parameters = parameters, orientation = orientation)
      } else {
        if (nchar(mapproj::.Last.projection()$projection) > 0) {
          projx <- mapproj::mapproject(x$long, x$lat)
        } else stop("No projection defined\n")
      }
      x$long <- projx$x
      x$lat <- projx$y
    } else stop("mapproj package not available\n")
  } else {
    if (usr[2] > (180 + 0.04*(usr[2] - usr[1]))) 
      x$long[x$long < 0] <- 360 + x$long[x$long < 0]
  }
  selection <- x$long >= usr[1] & x$long <= usr[2] & x$lat >= usr[3] &
    x$lat <= usr[4] & (x$pop >= minpop & x$pop <= maxpop) & ((capitals == 0) |
    (x$capital >= 1))
  if (country != "") 
    selection <- selection & x$country.etc == country
  selection0 <- selection & (x$capital == 0) & (capitals == 0)
  selection01 <- selection & (x$capital <= 1) & (capitals <= 1)
  selection1 <- selection & (x$capital == 1) & (capitals == 1)
  selection2 <- selection & (x$capital == 2) & (capitals == 2)
  selection3 <- selection & (x$capital == 3) & (capitals == 3)
  if (is.null(label)) 
    label <- sum(selection) < 20
  cxy <- par("cxy")
  if (sum(selection01) > 0) 
    points(x$long[selection01], x$lat[selection01], pch = pch, 
      cex = cex * 0.6, ...)
  if (sum(selection0) > 0) 
    if (label) 
      text(x$long[selection0], x$lat[selection0] + cxy[2] * cex * 0.7,
        paste(" ", x$name[selection0], sep = ""), cex = cex * 0.7, ...)
  if (sum(selection1) > 0) {
    points(x$long[selection1], x$lat[selection1], pch = pch, cex = cex, ...)
    if (label) {
      text(x$long[selection1], x$lat[selection1] + cxy[2] * cex,
        paste(" ", x$name[selection1], sep = ""), cex = cex * 1.2, ...)
    }
  }
  if (sum(selection2) > 0) {
    points(x$long[selection2], x$lat[selection2], pch = pch, cex = cex, ...)
    if (label) {
      text(x$long[selection2], x$lat[selection2] + cxy[2] * cex * 1.1,
        paste(" ", x$name[selection2], sep = ""), cex = cex * 1.1, ...)
    }
  }
  if (sum(selection3) > 0) {
    points(x$long[selection3], x$lat[selection3], pch = pch, cex = cex, ...)
    if (label) {
      text(x$long[selection3], x$lat[selection3] + cxy[2] * cex * 0.9,
        paste(" ", x$name[selection3], sep = ""), cex = cex * 0.9, ...)
    }
  }
  invisible()
}

# draw a scale bar on a map
"map.scale" <-
function (x, y, relwidth = 0.15, metric = TRUE, ratio = TRUE, ...) 
{
  # old version
  # format.pretty <- function(x) {
  #   as.character(pretty(x * c(0.99, 1.01), n = 2)[2])
  # }
  # minka: new version
  format.pretty <- function(x, digits = 2) {
  x = signif(x, 2)
  prettyNum(formatC(x, format = "fg", digits = digits), big.mark = ",")
  }
  usr <- par("usr")
  if (missing(y)) 
  y <- (9 * usr[3] + usr[4])/10
  if (abs(y) >= 90) 
  warning("location of scale out of this world!")
  if (missing(x)) 
  #x <- (0.9 - relwidth) * usr[2] + (0.1 + relwidth) * usr[1]
  x <- (9 * usr[1] + usr[2])/10
  cosy <- cos((2 * pi * y)/360)
  perdeg <- (2 * pi * (6356.78 + 21.38 * cosy) * cosy)/360
  scale <- (perdeg * 100000)/(2.54 * (par("pin")/diff(par("usr"))[-2])[1])
  if (metric) 
  unit <- "km"
  else {
  perdeg <- perdeg * 0.6213712
  unit <- "mi"
  }
  len <- perdeg * relwidth * (usr[2] - usr[1])
  ats <- pretty(c(0, len), n = 2)
  nats <- length(ats)
  labs <- as.character(ats)
  labs[nats] <- paste(labs[nats], unit)
  linexy <- matrix(NA, ncol = 2, nrow = 3 * nats)
  colnames(linexy) <- c("x", "y")
  cxy <- par("cxy")
  dy <- cxy[2] * par("tcl")
  dx <- ats[nats]/perdeg/(nats - 1)
  linexy[1, ] <- c(x, y)
  linexy[2, ] <- c(x, y + dy)
  for (i in 1:(nats - 1)) {
  linexy[3 * i, ] <- c(x + (i - 1) * dx, y)
  linexy[3 * i + 1, ] <- c(x + i * dx, y)
  linexy[3 * i + 2, ] <- c(x + i * dx, y + dy)
  }
  lines(linexy)
  # minka: this is broken
  text(x + ats/perdeg, y + dy - 0.5 * cxy[2], labs, adj = c(0.4, 0.5), ...)
  # minka: added ratio option
  if(ratio)
  text(x, y + 0.5 * cxy[2],
     paste("scale approx 1:", format.pretty(scale), sep = ""),
     adj = 0, ...)
  invisible(scale)
}

map.wrap <- function(p) {
  # insert NAs to break lines that wrap around the globe.
  # does not work properly with polygons.
  # p is list of x and y vectors
  dx = abs(diff(p$x))
  dax = abs(diff(abs(p$x)))
  j = which(dx/dax > 50)
  j = c(j, length(p$x))
  start = 1
  x = c()
  y = c()
  for(i in j) {
  if(length(x) > 0) {
    x = c(x, NA)
    y = c(y, NA)
  }
  x = c(x, p$x[start:i])
  y = c(y, p$y[start:i])
  start = i + 1
  }
## AD: this appears to be BUG. Why take out the first point?
## It is not NA. Maybe the length(x)>0 check came later...
  list(x = x[2:length(x)], y = y[2:length(y)])
}

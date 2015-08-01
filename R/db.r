"mapgetg" <-
function(database = "world", gons, fill = FALSE, xlim = c(-1e30, 1e30),
	ylim = c(-1e30, 1e30))
{
	ngon <- length(gons)
	gnames <- names(gons)
	dbname <- paste(database, "MapEnv", sep = "")
	# data(list = dbname)
	mapbase <- paste(Sys.getenv(get(dbname)), database, sep = "")
	z <- .C("mapgetg", PACKAGE="maps",
		as.character(mapbase),
		gons = as.integer(gons),
		as.integer(ngon),
		sizes = integer(ngon),
		error = as.integer(0),
		as.double(c(xlim, ylim)),
		as.integer(fill))[c("gons", "sizes", "error")]
	gons <- z$gons
	sizes <- z$sizes
	if(z$error < 0)
		stop("error in reading polygon headers")
	z <- .C("mapgetg", PACKAGE="maps",
		as.character(mapbase),
		as.integer(gons),
		as.integer(ngon),
		lines = integer(sum(sizes)),
		error = as.integer(1),
		as.double(c(xlim, ylim)),
		as.integer(fill))[c("lines", "error")]
	if(z$error < 0)
		stop("error in reading polyline numbers")
	lines <- z$lines
	ok <- sizes > 0
	list(number = lines, size = sizes[ok], name = gnames[ok])
}

"mapgetl" <-
function(database = "world", lines, xlim = c(-1e30, 1e30), ylim = c(-1e30,
	1e30), fill = FALSE)
{
	nline <- as.integer(length(lines))
	if(nline == 0)
		return(integer(0))
	dbname <- paste(database, "MapEnv", sep = "")
	# data(list = dbname)
	mapbase <- paste(Sys.getenv(get(dbname)), database, sep = "")
	z <- .C("mapgetl", PACKAGE="maps",
		as.character(mapbase),
		linesize = as.integer(lines),
		error = as.integer(nline),
		as.integer(0),
		as.double(0),
		as.double(0),
		as.double(c(xlim, ylim)),
		as.integer(fill))[c("linesize", "error")]
	if(z$error < 0)
		return(integer(0))
	ok <- z$linesize != 0
	lines <- lines[ok]
	nline <- length(lines)
	if(nline == 0)
		return(integer(0))
	linesize <- z$linesize[ok]
	N <- sum(linesize) + nline - 1
	xy <- .C("mapgetl", PACKAGE="maps",
		as.character(mapbase),
		as.integer(lines),
		as.integer(nline),
		as.integer(1),
		x = double(N),
		y = double(N),
		range = double(4),
		as.integer(fill))[c("x", "y", "range")]
	flip <- c(1, -1, 1, -1)
	box <- flip * pmax(flip * c(xlim, ylim), flip * xy$range)
	if(any(diff(box)[-2] <= 0)) {
		if(missing(xlim) || missing(ylim))
			stop("nothing to draw: data range and limits don't intersect"
				)
		else box <- c(xlim, ylim)
	}
	xy$range <- box
	xy
}

"mapname" <-
function(database = "world", patterns, exact = FALSE)
{
  dbname <- paste(database, "MapEnv", sep = "")
  # data(list = dbname)
  mapbase <- paste(Sys.getenv(get(dbname)), database, sep = "")
  # rewritten by Tom Minka
  fname <- paste(sep = "", mapbase, ".N")
  cnames <- read.delim(fname, as.is = TRUE, header = FALSE)
  nam <- as.character(cnames[[1]])
  if(exact) {
    i = match(patterns, nam)
    if(any(is.na(i))) i = NULL
  } else {
    regexp <- paste("(^", patterns, ")", sep = "", collapse = "|")
    i <- grep(regexp, nam, ignore.case = TRUE)
  }
  if(length(i) == 0) return(NULL)
  r <- cnames[i, 2]
  names(r) <- nam[i]
  return(r)
}

"maptype" <-
function(database = "world")
{
  if(is.character(database)) {
	dbname <- paste(database, "MapEnv", sep = "")
	# data(list = dbname)
	mapbase <- paste(Sys.getenv(get(dbname)), database, sep = "")
        # minka: maptypes are now 1,2 instead of 0,1
	switch(.C("maptype", PACKAGE="maps",
		as.character(mapbase),
		integer(1))[[2]] + 2, "unknown", "spherical", "planar", "spherical")
  } else {
    # map object
    "spherical"
  }
}

char.to.ascii <- function(s) {
  # returns the ascii code for a character (0 for an empty string)
  n = length(s)
  .C("char_to_ascii", PACKAGE="maps",
    as.integer(n), as.character(s), integer(n))[[2]]
}
is.regexp <- function(s) {
  # test: is.regexp(c("too", ".*dak", "kj[t]"))
  pattern = ".*[.?*[].*"
  result = logical(length(s))
  result[grep(pattern, s)] = TRUE
  result
}
match.map <- function(database, regions, exact = FALSE, warn = TRUE) {
  invperm <- function(p) {
    # returns the inverse permutation, so that invperm(p)[p] = 1:n
    ip = p
    ip[p] = 1:length(p)
    ip
  }
  if(is.character(database)) {
    dbname <- paste(database, "MapEnv", sep = "")
    # data(list = dbname)
    mapbase <- paste(Sys.getenv(get(dbname)), database, sep = "")
    fname <- paste(sep = "", mapbase, ".N")
    x <- read.delim(fname, header = FALSE)
    nam <- as.character(x[[1]])
  }
  else {
    nam <- database$names
  }
  nam = tolower(nam)
  regions = tolower(regions)
  if(!exact && any(is.regexp(regions))) {
    match.map.grep(nam, regions, warn)
  } else {
    # sort regions and table
    ord.nam = order(nam)
    nam = nam[ord.nam]
    ord.regions = order(regions)
    regions = regions[ord.regions]
    result = .C("map_match", PACKAGE="maps",
      as.integer(length(nam)), as.character(nam),
      as.integer(length(regions)), as.character(regions),
      result = integer(length(nam)), as.integer(exact))[["result"]]
    # 0 -> NA
    is.na(result[result == 0]) = TRUE
    # back to original order
    ord.regions[result[invperm(ord.nam)]]
    #result
  }
}
match.map.slow <- function(nam, regions, warn = FALSE) {
  result <- rep(NA, length = length(nam))
  # use hashing to prune the potential prefix matches
  # doesn't work for regexp
  let = min(nchar(regions))
  names(nam) = NULL
  hash = factor(char.to.ascii(substr(nam, let, let)))
  nam.bin = split(nam, hash)
  index.bin = split(1:length(nam), hash)
  for(i in 1:length(regions)) {
    pattern = regions[i]
    regexp <- paste("(^", pattern, ")", sep = "", collapse = "|")
    #r <- grep(regexp, nam)
    region.hash = as.character(char.to.ascii(substr(pattern, let, let)))
    r <- grep(regexp, nam.bin[[region.hash]])
    if(length(r) > 0) {
      r = index.bin[[region.hash]][r]
      result[r] <- i
    } else if(warn) warning(paste(pattern, "is not in the map"))
  }  
  result
}
match.map.grep <- function(nam, regions, warn = FALSE) {
  result <- rep(NA, length = length(nam))
  for(i in 1:length(regions)) {
    pattern = regions[i]
    regexp <- paste("(^", pattern, ")", sep = "", collapse = "|")
    r <- grep(regexp, nam)
    if(length(r) > 0) {
      result[r] <- i
    } else if(warn) warning(paste(pattern, "is not in the map"))
  }  
  result
}

# Some routines to create "map" objects from other formats

### 1. SP package
# transform a SpatialPolygons[DataFrame] into a list of polygons for map()
SpatialPolygons2map <- function(database, namefield=NULL){
  if (!inherits(database,"SpatialPolygons")) {
    stop("database must be a SpatialPolygons[DataFrame] object.")
  }
  region.names <- NULL
  if (inherits(database,"SpatialPolygonsDataFrame") & !is.null(namefield) ) {
    namcol <- lapply(namefield, function(x) which(tolower(names(database)) == tolower(x)))
    if (any(lapply(namcol, length) != 1)) {
      zz <- which(lapply(namcol, length) != 1)
      warning(paste0("database does not (uniquely) contain the field '",namefield[zz],"'."))
    } else {
      zz <- as.data.frame(lapply(database@data[unlist(namcol)], as.character), stringsAsFactors=FALSE)
      region.names <- vapply(1:dim(zz)[1], function(x) paste(zz[x,],collapse=":"),FUN.VALUE="a")
    }
  }
  if (is.null(region.names)) region.names <- unlist(lapply(database@polygons, function(x) x@ID))

  nregions <- length(region.names)

  # count the number of polygons in every "region"
  ngon <- vapply(1:nregions,
                 FUN=function(i) length(database@polygons[[i]]@Polygons),
                 FUN.VALUE=1)
  # if a region contains several polygons, an index is added to the name: "region:n"
  gon.names <- unlist(lapply(1:dim(database)[1], function(i) {
             if (ngon[i]==1) region.names[i]
             else paste(region.names[i],1:ngon[i],sep=":")}))

  # extract all polygon data to a list
  allpoly <- lapply(database@polygons,
                    function(x) lapply(x@Polygons, function(y) y@coords))
## allpoly is a list of lists of Nx2 matrices (not data frames)
## first flatten the list, then add NA to every row, then rbind and remove one NA
#  p1 <- do.call(c, allpoly)
#  p2 <- lapply(p1, function(x) rbind(c(NA,NA),x))
#  p3 <- do.call(rbind,p2)[-1,]
  mymap <- do.call(rbind, lapply(do.call(c,allpoly),
                                  function(x) rbind(c(NA,NA),x)))[-1,]
  result <- list(x = mymap[,1], y = mymap[,2], names = gon.names,
       range = c(range(mymap[,1], na.rm = TRUE),range(mymap[,2], na.rm = TRUE)))
  class(result) <- "map"
  result
}

# transform a SpatialLines[DataFrame] into a list of polylines for map()
SpatialLines2map <- function(database, namefield=NULL){
  if(!inherits(database,"SpatialLines")) stop("database must be a SpatialLines[DataFrame] object.")

  line.names <- NULL
  if (inherits(database,"SpatialLinesDataFrame") & !is.null(namefield) ) {
    namcol <- lapply(namefield, function(x) which(tolower(names(database)) == tolower(x)))
    if (any(lapply(namcol, length) != 1)) {
      zz <- which(lapply(namcol, length) != 1)
      warning(paste0("database does not (uniquely) contain the field '",namefield[zz],"'."))
    } else {
      zz <- as.data.frame(lapply(database@data[unlist(namcol)], as.character), stringsAsFactors=FALSE)
      line.names <- vapply(1:dim(zz)[1], function(x) paste(zz[x,],collapse=":"),FUN.VALUE="a")
    }
  }
  if (is.null(line.names)) line.names <- unlist(lapply(database@lines, function(x) x@ID))

  nlines <- length(line.names)

  # count the number of line segments in every "line"
  nseg <- vapply(1:nlines,
                 FUN=function(i) length(database@lines[[i]]@Lines),
                 FUN.VALUE=1)
  # if a line contains several sub-lines (segments), an index is added to the name: "line:n"
  line.names <- unlist(lapply(1:dim(database)[1], function(i) {
             if (nseg[i]==1) line.names[i]
             else paste(line.names[i],1:nseg[i],sep=":")}))

  # extract all polyline data to a list
  allpoly <- lapply(database@lines,
                    function(x) lapply(x@Lines, function(y) y@coords))
## allpoly is a list of lists of Nx2 matrices (not data frames)
## first flatten the list, then add NA to every row, then rbind and remove one NA
#  p1 <- do.call(c, allpoly)
#  p2 <- lapply(p1, function(x) rbind(c(NA,NA),x))
#  p3 <- do.call(rbind,p2)[-1,]
  mymap <- do.call(rbind, lapply(do.call(c,allpoly),
                                  function(x) rbind(c(NA,NA),x)))[-1,]
  result <- list(x = mymap[,1], y = mymap[,2], names = line.names,
       range = c(range(mymap[,1], na.rm = TRUE),range(mymap[,2], na.rm = TRUE)))
  class(result) <- "map"
  result
}

### 2. SF package (replacing sp)
sf2map <- function(database, namefield="name"){
  if (!inherits(database, "sf")) {
    stop("database must be a sf object.")
  }
  gname <- attr(database, "sf_column") # often ="geometry"
  tt = c("sfc_LINESTRING", "sfc_MULTILINESTRING", "sfc_POLYGON", "sfc_MULTIPOLYGON")
  if (!any(vapply(tt, FUN=function(x) inherits(database[[gname]], x), FUN.VALUE=TRUE))) {
    stop("sf2map currently only works for types ", paste(tt, collapse=" "))
  }
  nplines <- dim(database)[1]
  # in some cases (e.g. LINESTRING) various lines may have the same name, but we don't check
  if (!is.null(namefield) && namefield %in% names(database)) {
    # NOTE: the namefield may be a factor (e.g. tmap::World), so make sure to transform to vector
    line_names <- as.vector(database[[namefield]])
  } else {
    line_names <- 1:nplines
  }
  if (inherits(database[[gname]], "sfc_MULTILINESTRING") ||
      inherits(database[[gname]], "sfc_MULTIPOLYGON")) {
    nlines <- vapply(1:nplines,
                    FUN=function(r) sum(vapply(1:length(database[[gname]][[r]]),
                                               FUN=function(p) length(database[[gname]][[r]][[p]]),
                                               FUN.VALUE=1)),
                    FUN.VALUE=1)
    # if a multiline contains several lines, an index is added to the name: "line:n"
    line_names <- unlist(lapply(1:nplines, function(i) {
                    if (nlines[i]==1) line_names[i]
                    else paste(line_names[i], 1:nlines[i], sep=":")}))

    all_lines <- lapply(database[[gname]],
                      function(x) do.call(c, x))
  } else {
    all_lines <- database[[gname]]
  }
  # extract all line data to a list

## all_lines is a list of lists of Nx2 matrices (not data frames)
## first flatten the list, then add NA to every row, then rbind and remove one NA
#  p1 <- do.call(c, all_lines)
#  p2 <- lapply(p1, function(x) rbind(c(NA,NA),x))
#  p3 <- do.call(rbind,p2)[-1,]
  mymap <- do.call(rbind, lapply(do.call(c,all_lines),
                                  function(x) rbind(c(NA,NA),x)))[-1,]
  result <- list(x = mymap[,1], y = mymap[,2], names = line_names,
                 range = c(range(mymap[,1], na.rm = TRUE),
                           range(mymap[,2], na.rm = TRUE)))
  class(result) <- "map"
  result
}


###########################################




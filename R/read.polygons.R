# transform a SpatialPolygons[DataFrame] into a list of polygons for map()
# somewhat geared towards Natural Earth data (which has 'name' and 'iso_a2' columns)
SpatialPolygons2map <- function(database, a2code=NULL, namefield="name"){
#  if (is.character(database)) database <- readShapePoly(database)
  if(!inherits(database,"SpatialPolygons")) stop("database must be a SpatialPolygons[DataFrame] object.")

  region.names <- NULL
  if (inherits(database,"SpatialPolygonsDataFrame")) {
    if (!is.null(a2code)) {
      a2col <- which(tolower(names(database)) == "iso_a2")
      if (length(a2col) == 1) {
        a2 <- database@data[[a2col]]
        database <- database[a2 %in% a2code, ]
      } else warning("No column iso_a2 found.")
    }
    if (!is.null(namefield)) {
      namcol <- which(tolower(names(database)) == tolower(namefield))
      if (length(namcol) == 1) region.names <- as.character(database@data[[namcol]])
      else {
        warning(paste0("database does not contain the field '",namefield,"'."))
      }
    }
  }
  if (is.null(region.names)) region.names <- unlist(lapply(database@polygons, function(x) x@ID))

  nregions <- length(region.names)

  # count the number of polygons in every "region"
  ngon <- vapply(1:nregions,
                 FUN=function(i) length(database@polygons[[i]]@Polygons),
                 FUN.VALUE=1)

  gon.names <- unlist(lapply(1:dim(database)[1], function(i) {
             if (ngon[i]==1) region.names[i]
             else paste(region.names[i],1:ngon[i],sep=":")}))

  # extract all polygon data to a list
  allpoly <- lapply(database@polygons,
                    function(x) lapply(x@Polygons, function(y) y@coords))
## allpoly is a list of lists of Nx2 matrices (not data frames)
## first flatten the list, then add NA to every row, then rbind and remove last NA
#  p1 <- do.call(c, allpoly)
#  p2 <- lapply(p1, function(x) rbind(c(NA,NA),x))
#  p3 <- do.call(rbind,p2)
  result <- do.call(rbind, lapply(do.call(c,allpoly),
                                  function(x) rbind(c(NA,NA),x)))
  list(x = result[-1,1], y = result[-1,2], names = gon.names,
       range = c(range(result[,1], na.rm = TRUE),range(result[,2], na.rm = TRUE)))
}


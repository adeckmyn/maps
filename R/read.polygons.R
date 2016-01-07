# transform a SpatialPolygons[DataFrame] into a list of polygons for map()
# somewhat geared towards Natural Earth data (which has 'name' and 'iso_a2' columns)
SpatialPolygons2map <- function(database, a2code=NULL, namefield=NULL){
#  if (is.character(database)) database <- readShapePoly(database)
  if(!inherits(database,"SpatialPolygons")) stop("database must be a SpatialPolygons[DataFrame] object.")

  if (inherits(database,"SpatialPolygonsDataFrame")) {
    if (!is.null(a2code)) {
      a2col <- which(tolower(names(database)) == "iso_a2")
      if (length(a2col) == 1) {
        a2 <- database@data[[a2col]]
        database <- database[a2 %in% a2code, ]
      } else warning("No column iso_a2 found.")
    }
    if (is.null(namefield)) {
# we use as default:
      namefield <- "name"
# but we don't enforce this
      lforce <- FALSE
    } else lforce <- TRUE
    namcol <- which(tolower(names(database)) == tolower(namefield))
    if (length(namcol) == 1) region.names <- as.character(database@data[[namcol]])
    else {
      if (lforce) stop(paste0("database does not contain the field '",namefield,"'."))
#      warning(paste0("database does not contain the field '",namefield,"'."))
      region.names <- unlist(lapply(database@polygons, function(x) x@ID))
    }
  } else {
    region.names <- unlist(lapply(database@polygons, function(x) x@ID))
  }
  nregions <- length(region.names)

  ngon <- vapply(1:nregions,
                FUN=function(i) length(database@polygons[[i]]@Polygons),
                FUN.VALUE=1)

  gon.names <- unlist(lapply(1:dim(database)[1], function(i) {
             if (ngon[i]==1) region.names[i] 
             else paste(region.names[i],1:ngon[i],sep=":")}))

  allpoly <- lapply(database@polygons, 
                    function(x) lapply(x@Polygons, function(y) y@coords))
## allpoly is a list of lists of Nx2 matrices (not data frames)
## first flatten the list, then add NA to every row, then rbind and remove last NA
#  p1 <- do.call(c, allpoly)
#  p2 <- lapply(p1, function(x) rbind(x,c(NA,NA)))
#  p3 <- do.call(rbind,p2)
  result <- do.call(rbind, lapply(do.call(c,allpoly), 
                                  function(x) rbind(x,c(NA,NA))))
  list(x = head(result[,1],-1), y = head(result[,2],-1), names = gon.names,
       range = c(range(result[,1], na.rm = TRUE),range(result[,2], na.rm = TRUE)))
}


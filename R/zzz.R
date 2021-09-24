.onLoad <- function(lib, pkg) {
  if (Sys.getenv("R_MAP_DATA_DIR") == "")
    Sys.setenv("R_MAP_DATA_DIR" = paste(lib, pkg, "mapdata/", sep="/"))
  library.dynam("maps", pkg, lib)
}


.onLoad <- function(lib, pkg) {
  if (Sys.getenv("R_MAP_DATA_DIR") == "") {
    Sys.setenv("R_MAP_DATA_DIR" = paste(lib, pkg, "mapdata/", sep="/"))
  }
  ## for now, we keep the option of having a different world map
  if (Sys.getenv("R_MAP_DATA_DIR_WORLD") == "") {
    Sys.setenv("R_MAP_DATA_DIR_WORLD" = Sys.getenv("R_MAP_DATA_DIR"))
  }
  library.dynam("maps", pkg, lib)
}


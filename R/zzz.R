.onAttach <- function(lib,pkg) {
  packageStartupMessage("\n",
#      " ############################################################\n",
      " # ATTENTION: maps v3.1 has an updated 'world' map.        #\n",
      " # All lakes have been moved to the new 'lakes' database.  #\n",
      " # Type '?world' or 'news(package=\"maps\")'. See README_v3. #\n",
#      " ############################################################\n")
      "\n")
}

.onLoad <- function(lib, pkg) {
  if (Sys.getenv("R_MAP_DATA_DIR") == "")
    Sys.setenv("R_MAP_DATA_DIR" = paste(lib, pkg, "mapdata/", sep="/"))
## temporarily, we add the possibility of changing for "world" only,
## to point at the /legacy subdirectory
  if (Sys.getenv("R_MAP_DATA_DIR_WORLD") == "") {
    if(Sys.getenv("R_MAP_DATA_LEGACY")=="TRUE") Sys.setenv("R_MAP_DATA_DIR_WORLD" = paste(Sys.getenv("R_MAP_DATA_DIR"), "legacy_",sep=""))
    else Sys.setenv("R_MAP_DATA_DIR_WORLD" = Sys.getenv("R_MAP_DATA_DIR"))
  }
  library.dynam("maps", pkg, lib)
}


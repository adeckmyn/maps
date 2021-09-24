#! /usr/bin/env Rscript

MAPENV <- "R_MAP_DATA_DIR"
OUTDIR <- "../data"

#datalist <- c("world", "world2", "lakes",
datalist <- c("lakes",
              "county", "state", "usa", "state", "state.vbm", "state.carto",
              "italy", "france", "nz")

for (dd in datalist) {
  dname <- paste0(dd, "MapEnv")
  assign(dname, MAPENV)
  save(list=dname, file=paste0(OUTDIR,"/",dname,".rda"))
}
# some (temporary?) exceptions:
for (dd in c("world", "world2")) {
  dname <- paste0(dd, "MapEnv")
  assign(dname, "R_MAP_DATA_DIR_WORLD")
  save(list=dname, file=paste0(OUTDIR,"/",dname,".rda"))
}



datalist <- c("county.fips", "canada.cities", "iso3166", "ozone",
              "state.carto.center", "state.fips", "state.vbm.center",
              "unemp", "us.cities", "votes.repub", "world.cities")

rda2tsv <- function(dlist=datalist) {
  for (dname in dlist) {
    infile <- paste0("../data/",dname,".rda")
    load(infile)
    outfile <- paste0("../data-raw/",dname,".tsv")
    write.table(eval(parse(text=dname)), file=outfile,
                row.names=FALSE, quote=TRUE, sep="\t")
  }
}

tsv2rda <-  function(dlist=datalist) {
  for (dname in dlist) {
    infile <- paste0("../data-raw/",dname,".tsv")
    outfile <- paste0("../data/",dname,".rda")

    zz <- read.table(infile, stringsAsFactors=FALSE, 
                     sep=" ", header=TRUE)
    assign(dname, zz)
    save(list=dname, file=outfile)
  }
}


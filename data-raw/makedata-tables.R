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

tsv2rda <-  function(dlist) {
  # don't run this on all data sets at once! Some need special care.
  for (dname in dlist) {
    infile <- paste0("../data-raw/",dname,".tsv")
    outfile <- paste0("../data/",dname,".rda")
    # in iso3166, there are no missing entries and "NA" is Namibia
    # but in other tables, we must account for missing values
    nas <- if (dname=="iso3166") "" else "NA"

    # Some data sets are saved as a MATRIX, not a data.table
    zz <- read.table(infile, stringsAsFactors=FALSE,
                     sep="\t", header=TRUE, na.strings=nas)
    assign(dname, zz)
    save(list=dname, file=outfile)
  }
}


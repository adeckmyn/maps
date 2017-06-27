srcdata="http://download.geonames.org/export/dump/cities5000.zip"
zipfile=basename(srcdata)
infile="cities5000.txt"

download.file(srcdata, zipfile)

c1 <- read.table(unz(zipfile, infile), stringsAsFactor=FALSE, sep="\t", header=FALSE, quote="")
c1 <- c1[,c(3,5,6,8,9,15,19)]
names(c1) <- c("name","lat","lon","code","country","pop","date")

### compare/validate
c0 <- maps::world.cities

# $code has 15 possible values:
# [1] "PPLA"  "PPLC"  "PPL"   "PPLA2" "PPLA3" "PPLX"  "PPLA4" "PPLL"  "PPLG" 
# [10] "PPLH"  "PPLQ"  "PPLS"  "PPLR"  "PPLF"  "PPLW" 
# 
c1$capital <- 0
c1$capital[c1$code=="PPLC"] <- 1


# compare by name, lat, lon, country...
# if pop > 5000, it should be in both

# ~22000 same names = ~50%
t1 <- intersect(c1$name, c0$name)
# ATTENTION: sometimes many cities with same name -> use lat/lon or country...
# how check the rest? does it matter?




## save
world.cities2 <- c1
save("world.cities2", file="../data/world.cities2.rda")
write.table(world.cities2,  file="./world.cities2.tsv", row.names=FALSE, quote=TRUE, sep="\t")


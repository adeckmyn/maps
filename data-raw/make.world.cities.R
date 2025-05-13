# This is a work-in-progress file, absolutely not meant for common users
srcdata="http://download.geonames.org/export/dump/"
zipfile="cities5000.zip"
infile="cities5000.txt"

# NaturalEarth Populated Places has >7000 entries
#nnpps="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_populated_places_simple.zip"

download.file(paste0(srcdata, zipfile), zipfile)
#download.file(paste0(srcdata, "readme.txt"), "readme_cities5000.txt")
#download.file(paste0(srcdata, "featureCodes_en.txt"), "featureCodes_cities5000.txt")

#1 geonameid         : integer id of record in geonames database
#2 name              : name of geographical point (utf8) varchar(200)
#3 asciiname         : name of geographical point in plain ascii characters, varchar(200)
#4 alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
#5 latitude          : latitude in decimal degrees (wgs84)
#6 longitude         : longitude in decimal degrees (wgs84)
#7 feature class     : see http://www.geonames.org/export/codes.html, char(1)
#8 feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
#9 country code      : ISO-3166 2-letter country code, 2 characters
#10 cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
#11 admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
#12 admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
#13 admin3 code       : code for third level administrative division, varchar(20)
#14 admin4 code       : code for fourth level administrative division, varchar(20)
#15 population        : bigint (8 byte int) 
#16 elevation         : in meters, integer
#17 dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
#18 timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
#19 modification date : date of last modification in yyyy-MM-dd format

# $code has 18 possible values:
#P.PPL	populated place	a city, town, village, or other agglomeration of buildings where people live and work
#P.PPLA	seat of a first-order administrative division	seat of a first-order administrative division (PPLC takes precedence over PPLA)
#P.PPLA2	seat of a second-order administrative division	
#P.PPLA3	seat of a third-order administrative division	
#P.PPLA4	seat of a fourth-order administrative division	
#P.PPLA5	seat of a fifth-order administrative division	
#P.PPLC	capital of a political entity	
#P.PPLCH	historical capital of a political entity	a former capital of a political entity
#P.PPLF	farm village	a populated place where the population is largely engaged in agricultural activities
#P.PPLG	seat of government of a political entity	
#P.PPLH	historical populated place	a populated place that no longer exists
#P.PPLL	populated locality	an area similar to a locality but with a small group of dwellings or other buildings
#P.PPLQ	abandoned populated place	
#P.PPLR	religious populated place	a populated place whose population is largely engaged in religious occupations
#P.PPLS	populated places	cities, towns, villages, or other agglomerations of buildings where people live and work
#P.PPLW	destroyed populated place	a village, town or city destroyed by a natural disaster, or by war
#P.PPLX	section of populated place	
#P.STLMT	israeli settlement

cities5000 <- read.table(unz(zipfile, infile), stringsAsFactor=FALSE, sep="\t", header=FALSE, quote="", na.strings="")
# NOTE: country code "NA" (Namibia) is replaced by <NA> if you don't set na.strings=""
c1 <- cities5000[,c(3,5,6,15,19)]
names(c1) <- c("name","lat","lon","pop","date")
# FIXME: this gives few weird regex's if you don't set regex=FALSE
# but setting TRUE gives different # results. Some countries have 2 codes, etc.
# for instance, "NO" is Norway (polygon) but not Bouvet Island, Svalbard ...
# "CN" is China and Paracel islands, but not Hong Kong&Macao
# BUT in this context, we just want to know it's in Norway
#  so iso.expand is not really helpful. It needs a "simple=TRUE" option :-)
# NOTE: e.g. Longyearbyen has [[9]]==SJ [[10]]=NO, old table has "Svalbard and Jan Mayen"
c1$"country.etc" <- maps::iso.expand(cities5000[[9]], regex=TRUE)
### compare/validate
c0 <- maps::world.cities

# capital?
# unique(c1$code) : 
#[1] "PPLA"  "PPLC"  "PPL"   "PPLA2" "PPLW"  "PPLA3" "PPLX"  "PPLA4" "PPLL" 
#[10] "PPLQ"  "PPLA5" "PPLG"  "PPLS"  "PPLCH" "STLMT" "PPLR"  "PPLF"  "PPLH" 

#capital status indication (0 for non-capital, 1 for capital, 2 for China Municipalities, and 3 for
#     China Provincial capitals)

c1$capital <- 0
c1$capital[cities5000[[8]]=="PPLC"] <- 1
ttt=vapply(c1$name, FUN=function(x) x %in% c0$name, FUN.VAL=TRUE)
# 26522 "matches" on 59844 new names
ttt=vapply(c0$name, FUN=function(x) x %in% c1$name, FUN.VAL=TRUE)
# 25151 "matches" from 43645 old names
# so roughly 50% of city names match exactly.
# NOTE: there are duplicated city names, the actual number of common names:
t1 <- intersect(c1$name, c0$name)


# compare by name, lat, lon, country...
# if pop > 5000, it should be in both

# ~22000 same names = ~50%
#t1 <- intersect(c1$name, c0$name)
# ATTENTION: sometimes many cities with same name -> use lat/lon or country...
# how check the rest? does it matter?




## save
world.cities2 <- c1
save("world.cities2", file="../data/world.cities2.rda")
write.table(world.cities2,  file="./world.cities2.tsv", row.names=FALSE, quote=TRUE, sep="\t")

# Various checks
# Capitals
cap0 <- c0[c0$capital==1,]
cap1 <- c1[c1$capital==1,]
# 230 vs 241 -> missing capitals?
# Many differences in spelling, e.g.
# Luxemburg <-> Luxembourg
# Ulaanbaatar <-> Ulan Bator
# Havana <-> Havanna
# Beirut <-> Bayrut
# Kiev <-> Kyiv
# I think cities5000 is more up-to-date on correct spelling, but it varies...
# NOTE: in geonames, there is no official capital for Israel because Jerusalem is controversial...
setdiff(cap0$name, cap1$name)



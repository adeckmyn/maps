### translate ISO 3166-1 alpha-2 codes to country names
iso.exceptions <- c("China:Hong Kong","China:Macao","Norway:Svalbard",
                    "Norway:Jan Mayen","Finland:Aland")

iso.expand <- function(a){
  iso3166 <- get("iso3166")
  if (all(nchar(a)==2)) sel <- which(iso3166$a2 %in% toupper(a))
  else if (all(nchar(a)==3)) sel <- which(iso3166$a3 %in% toupper(a))
  else stop("All codes must be equal length, 2 or 3 characters.")
  iso3166$mapname[sel]
}

sov.expand <- function(sov){
  sel <- which(tolower(iso3166$sovereignty) %in% tolower(sov))
## Subtility: we have allowed a few ISO zones as sub-countries (China:Macao)...
## So if both "China" and "China:Macao" are present, we get a doubled polygon.
## That is not really a disaster
## On the ther hand, ISOexpand("CN") will now also draw Hong Kong and Macao/
## That may be wished, but not necessarily. Strictly speaking it is a bug.
## Can we do some magic with regular expressions? Or just hard-code the exceptions.
## if we use grep(..,perl=TRUE):
## replace e.g. "China" by "China(?![:Hong|:Macao])"
  iso3166$mapname[sel]
}

### the inverse:

## I don't really like this ad hoc solution, but here it goes:
## solution: do a match for name:names, then on the basenames
## another solution may be to return CN(?![:Hong|:Macao])
## not exactly beautiful, but it would work if we use grep(...,perl=TRUE)
iso.alpha <- function(x,n=2){
  lx <- tolower(x)
  lmap <- tolower(iso3166$mapname)

  iso3166 <- get("iso3166")
  nam1 <- vapply(strsplit(lx,":"),function(nn) nn[1],FUN.VALUE="str")
  sel1 <- match(nam1,lmap)

#  regexp <- paste("(^", iso.exceptions, ")", sep = "", collapse = "|")
#  exp <- grep(regexp, x, ignore.case = TRUE)
#  if (length(exp)>0) {
    nam2 <- vapply(strsplit(lx,":"),function(x) paste(x[1:2],collapse=":"),
                 FUN.VALUE="str")
    sel2 <- match(nam2,lmap)
    sel <- ifelse(is.na(sel2),sel1,sel2)
#  }
  
  if (n==2) iso3166$a2[sel]
  else if (n==3) iso3166$a3[sel]
  else stop("n must be 2 or 3.")
}


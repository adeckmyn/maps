### translate ISO 3166-1 alpha-2 codes to country names

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

## BUG: "China:Hong Kong" will return "CN" etc
## I don't really like this ad hoc solution, but here it goes:
## solution: first do a match for complete names, than on the remaining basenames
## another solution may be to return CN(?![:Hong|:Macao])
## not exactly beautiful, but it would work if we use grep(...,perl=TRUE)
## However, then we still need to check for the exceptions.
iso.alpha <- function(x,a=2){
  iso3166 <- get("iso3166")
  nam <- as.vector(lapply(strsplit(x,":"),function(nn) nn[1]))
  sel <- match(nam,iso3166$mapname)

  exceptions <- c("China:Hong Kong","China:Macao","Norway:Svalbard",
                  "Norway:Jan Mayen","Finland:Aland")
  regexp <- paste("(^", exceptions, ")", sep = "", collapse = "|")
  exp <- grep(regexp, x, ignore.case = TRUE)
  if (length(exp)>0) {
    s2 <- vapply(x,function(x) paste(strsplit(x,":")[[1]][1:2],collapse=":"),
                 FUN.VALUE="str")
    sel2 <- match(s2,iso3166$mapname)
    sel[!is.na(sel2)] <- sel2[!is.na(sel2)]
  }
  
  if (a==2) iso3166$a2[sel]
  else if (a==3) iso3166$a3[sel]
  else stop("a must be 2 or 3.")
}

## Current exceptions:
# ISOexceptions <- c("China:Hong Kong","China:Macao","Finland:Aland Islands")
# MAYBE: "Norway:Svalbard","Denmark:Faroe Islands", 



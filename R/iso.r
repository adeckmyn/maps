### translate ISO 3166-1 alpha-2 codes to country names

ISOexpand <- function(sov=NULL,a2=NULL,a3=NULL){
  iso3166 <- get("iso3166")
  if (!is.null(sov)) sel1 <- which(tolower(iso3166$sovereignty) %in% tolower(sov))
  if (!is.null(a2))  sel2 <- which(iso3166$a2 %in% toupper(a2))
  if (!is.null(a3))  sel3 <- which(iso3166$a3 %in% toupper(a3))
  sel <- unique(c(sel1,sel2,sel3))
  iso3166$mapname[sel]

}

### the inverse:

ISOalpha <- function(x,a=2){
  iso3166 <- get("iso3166")
  nam <- as.vector(lapply(strsplit(x,":"),function(nn) nn[1]))
  sel <- match(nam,iso3166$mapname)
  if (a==2) iso3166$a2[sel]
  else if (a==3) iso3166$a3[sel]
  else stop("a must be 2 or 3.")
}




### translate ISO 3166-1 alpha-2 codes to country names

iso3166 <- function(x){
  ISOlist <- get("ISOlist")
  ISOlist$name[match(x,ISOlist$code)]
}

### for the inverse:

iso3166inv <- function(x){
  ISOlist <- get("ISOlist")
  nam <- as.vector(lapply(strsplit(x,":"),function(nn) nn[1]))
  ISOlist$code[match(nam,ISOlist$name)]
}

 

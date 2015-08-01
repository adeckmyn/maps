### translate ISO 3166-1 alpha-2 codes to country names

iso3166 <- function(x){
  ISOlist <- get("ISOlist")
  ISOlist$name[match(x,ISOlist$code)]
}


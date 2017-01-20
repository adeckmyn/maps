map.wrap.poly <- function(data, xlim, poly=FALSE, antarctica=-89, debug=FALSE) {
  MAXWRAP=100;
  len_in <- length(data$x)
  len_out <- 2*len_in
  wrap <- .C("mapwrap", 
               xin=data$x, yin=data$y, nin=as.integer(len_in),
               wraplist=integer(MAXWRAP), nsegments=integer(MAXWRAP),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               poly=as.integer(poly), xmin=xlim[1], xmax=xlim[2],
               antarctica=as.numeric(antarctica),
               NAOK=TRUE, PACKAGE="maps")
  if(debug) return(wrap)

  xlen <- wrap$nout
  if (!is.null(data$names) && poly) {
    newnames <- data$names
    nwrap <- sum(wrap$nsegments > 0)
    if (nwrap > 0) {
      for (i in 1:nwrap) {
        nsegments <- wrap$nsegments[i]
        pn <- wrap$wraplist[i]
        nam <- newnames[pn]
        splitnam <- paste0(nam,":split",1:nsegments)
        newnames <- c(head(newnames,pn-1), splitnam, 
                      tail(newnames,-(pn+1)))
      }
    }
    result <- list(x=wrap$xout[1:xlen], y=wrap$yout[1:xlen], names=newnames)
  } else result <- list(x=wrap$xout[1:xlen], y=wrap$yout[1:xlen])
  if (inherits(data, "map")) class(result) <- "map"
  result
}

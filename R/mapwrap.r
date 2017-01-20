map.wrap.poly <- function(data, xlim, poly=FALSE, antarctica=-89) {
  MAXWRAP=sum(is.na(data$x))+1
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

  xlen <- wrap$nout
  result <- list(x=wrap$xout[1:xlen], y=wrap$yout[1:xlen])
  if (!is.null(data$names) && poly) {
#    unique names for all partial polygons?
#    newnames <- rep(data$names, times=wrap$nsegments)
    newnames <- do.call(c, lapply(1:length(data$names), function(i)
                  paste0(data$names[i],":p",1:wrap$npoly[i])))
    result$names <- newnames
  }
  if (inherits(data, "map")) class(result) <- "map"
  result
}

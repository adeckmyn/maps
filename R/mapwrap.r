map.wrap.poly <- function(data, xlim, poly=FALSE, antarctica=-89) {
  MAXWRAP=sum(is.na(data$x))+1
  len_in <- length(data$x)
  len_out <- 2*len_in
  wrap <- .C("map_wrap",
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
#    result$names <- rep(data$names, times=wrap$nsegments)
  # unique names for all partial polygons
    result$names <- do.call(c, lapply(1:length(data$names), function(i)
         if (wrap$nsegments[i]>1) paste0(data$names[i],":p",
           1:wrap$nsegments[i]) else data$names[i]  ))
  }
  result$range <- c(range(result$x, na.rm=TRUE), range(result$y, na.rm=TRUE)) 
  if (inherits(data, "map")) class(result) <- "map"
  result
}


map.restrict <- function(data, xlim=NULL, ylim=NULL) {
  if (!is.null(xlim)) {
    len_in <- length(data$x)
    len_out <- 2*len_in
    d1 <- .C("map_restrict",
               xin=as.numeric(data$x), yin=as.numeric(data$y),
               nin=as.integer(len_in),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               xmin=as.numeric(xlim[1]), xmax=as.numeric(xlim[2]),
               NAOK=TRUE, PACKAGE="maps")
    xlen <- d1$nout
    data$x <- d1$xout[1:xlen]
    data$y <- d1$yout[1:xlen]
    if (!is.null(data$range)) data$range[1:2] <- xlim
  }
  if (!is.null(ylim)) {
    len_in <- length(data$x)
    len_out <- 2*len_in
    d2 <- .C("map_restrict", 
               yin=data$y, xin=data$x, nin=as.integer(len_in),
               yout=numeric(len_out), xout=numeric(len_out), 
               nout=as.integer(len_out),
               ymin=ylim[1], ymax=ylim[2],
               NAOK=TRUE, PACKAGE="maps")
    xlen <- d2$nout
    data$x <- d2$xout[1:xlen]
    data$y <- d2$yout[1:xlen]
    if (!is.null(data$range)) data$range[3:4] <- ylim
  }
  data
}

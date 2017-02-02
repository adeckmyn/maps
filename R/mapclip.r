map.wrap.poly <- function(data, xlim, poly=FALSE, antarctica=TRUE) {
  nseg <- sum(is.na(data$x))+1
  len_in <- length(data$x)
  len_out <- 2*len_in
  wrap <- .C("map_wrap_poly",
               xin=data$x, yin=data$y, nin=as.integer(len_in),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               xmin=as.numeric(xlim[1]), xmax=as.numeric(xlim[2]),
               poly=as.integer(poly), npoly=integer(nseg),
               antarctica=as.integer(antarctica),
               NAOK=TRUE, PACKAGE="maps")

  xlen <- wrap$nout
  data$x <- wrap$xout[1:xlen]
  data$y <- wrap$yout[1:xlen]
  if (!is.null(data$names) && poly) {
    data$names <- rep(data$names, times=wrap$npoly)
  }
  data$range <- c(range(data$x, na.rm=TRUE), range(data$y, na.rm=TRUE)) 
  data
}

map.clip.poly <- function(data, xlim=NULL, ylim=NULL, poly=FALSE) {
  nam <- data$names

  if (!is.null(xlim)) {
    len_in <- length(data$x)
    len_out <- 2*len_in
    nseg <- sum(is.na(data$x)) + 1
    dd <- .C("map_clip_poly",
               xin=as.numeric(data$x), yin=as.numeric(data$y),
               nin=as.integer(len_in),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               xlim=as.numeric(xlim[1]), inside=as.integer(1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE, PACKAGE="maps")
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
    len_in <- dd$nout
    len_out <- 2*len_in
    nseg <- sum(is.na(dd$xout[1:dd$nout])) + 1
    dd <- .C("map_clip_poly",
               xin=as.numeric(dd$xout), yin=as.numeric(dd$yout),
               nin=as.integer(len_in),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               xlim=as.numeric(xlim[2]), inside=as.integer(-1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE, PACKAGE="maps")
    data$x <- dd$xout[1:dd$nout]
    data$y <- dd$yout[1:dd$nout]
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
  }
  if (!is.null(ylim)) {
    len_in <- length(data$x)
    len_out <- 2*len_in
    nseg <- sum(is.na(data$x)) + 1
    dd <- .C("map_clip_poly",
               yin=as.numeric(data$y), xin=as.numeric(data$x),
               nin=as.integer(len_in),
               yout=numeric(len_out), xout=numeric(len_out), 
               nout=as.integer(len_out),
               ylim=as.numeric(ylim[1]), inside=as.integer(1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE, PACKAGE="maps")
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
    len_in <- dd$nout
    len_out <- 2*len_in
    nseg <- sum(is.na(dd$xout[1:dd$nout])) + 1
    dd <- .C("map_clip_poly",
               yin=as.numeric(dd$yout), xin=as.numeric(dd$xout),
               nin=as.integer(len_in),
               yout=numeric(len_out), xout=numeric(len_out), 
               nout=as.integer(len_out),
               ylim=as.numeric(ylim[2]), inside=as.integer(-1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE, PACKAGE="maps")
    data$x <- dd$xout[1:dd$nout]
    data$y <- dd$yout[1:dd$nout]
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
  }
  if (!is.null(nam) && poly) data$names <- nam
  data$range <- c(range(data$x, na.rm=TRUE), range(data$y, na.rm=TRUE))
  data
}

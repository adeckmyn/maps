map.wrap.poly <- function(data, xlim, poly = FALSE, antarctica = -89.5) {
  nseg <- sum(is.na(data$x))+1
  len_in <- length(data$x)
  # usually, the result will be less then twice as long,
  # but every wrapped boundary (poly=TRUE) adds 10 interpolation points
  # so for a small data set, this can be more than x2
  len_out <- 2*len_in + 100
  wrap <- .C(C_map_wrap_poly,
               xin=data$x, yin=data$y, nin=as.integer(len_in),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               xmin=as.numeric(xlim[1]), xmax=as.numeric(xlim[2]),
               poly=as.integer(poly), npoly=integer(nseg),
               antarctica=as.double(antarctica),
               NAOK=TRUE)

  xlen <- wrap$nout
  data$x <- wrap$xout[1:xlen]
  data$y <- wrap$yout[1:xlen]
  if (!is.null(data$names) && poly) {
    data$names <- rep(data$names, times=wrap$npoly)
  }
  data$range <- c(range(data$x, na.rm=TRUE), range(data$y, na.rm=TRUE)) 
  data
}

map.clip.poly <- function(data, xlim=c(NA, NA), ylim=c(NA, NA), poly=FALSE) {
  nam <- data$names
# Make sure that x & y have the same NA delimiters!
  data$x[is.na(data$y)] <- NA
  data$y[is.na(data$x)] <- NA
# >= xlim[1]
  if (!is.na(xlim[1])) {
    len_in <- length(data$x)
    len_out <- 2*len_in
    nseg <- sum(is.na(data$x)) + 1
    dd <- .C(C_map_clip_poly,
               xin=as.numeric(data$x), yin=as.numeric(data$y),
               nin=as.integer(len_in),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               xlim=as.numeric(xlim[1]), inside=as.integer(1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE)
    data$x <- dd$xout[1:dd$nout]
    data$y <- dd$yout[1:dd$nout]
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
  }

# <= xlim[2]
  if (!is.na(xlim[2])) {
    len_in <- length(data$x)
    len_out <- 2*len_in
    nseg <- sum(is.na(data$x)) + 1
    dd <- .C(C_map_clip_poly,
               xin=as.numeric(data$x), yin=as.numeric(data$y),
               nin=as.integer(len_in),
               xout=numeric(len_out), yout=numeric(len_out), 
               nout=as.integer(len_out),
               xlim=as.numeric(xlim[2]), inside=as.integer(-1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE)
    data$x <- dd$xout[1:dd$nout]
    data$y <- dd$yout[1:dd$nout]
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
  }

# >= ylim[1]
  if (!is.na(ylim[1])) {
    len_in <- length(data$y)
    len_out <- 2*len_in
    nseg <- sum(is.na(data$y)) + 1
    dd <- .C(C_map_clip_poly,
               yin=as.numeric(data$y), xin=as.numeric(data$x),
               nin=as.integer(len_in),
               yout=numeric(len_out), xout=numeric(len_out), 
               nout=as.integer(len_out),
               ylim=as.numeric(ylim[1]), inside=as.integer(1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE)
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
    data$x <- dd$xout[1:dd$nout]
    data$y <- dd$yout[1:dd$nout]
  }
 
# <= ylim[2]
  if (!is.na(ylim[2])) {
    len_in <- length(data$y)
    len_out <- 2*len_in
    nseg <- sum(is.na(data$y)) + 1
    dd <- .C(C_map_clip_poly,
               yin=as.numeric(data$y), xin=as.numeric(data$x),
               nin=as.integer(len_in),
               yout=numeric(len_out), xout=numeric(len_out), 
               nout=as.integer(len_out),
               ylim=as.numeric(ylim[2]), inside=as.integer(-1),
               poly=as.integer(poly), npoly=integer(nseg),
               NAOK=TRUE)
    data$x <- dd$xout[1:dd$nout]
    data$y <- dd$yout[1:dd$nout]
    if (!is.null(nam) && poly) nam <- rep(nam, times=dd$npoly)
  }

  if (!is.null(nam) && poly) data$names <- nam
  data$range <- c(range(data$x, na.rm=TRUE), range(data$y, na.rm=TRUE))
  data
}

// Polygon wrapping
// Part of R-package 'maps'
// distributed under GPL-2
// (c) Alex Deckmyn, 2017
#include "R.h"

#define MAX_SEGMENTS 50
void split_poly(double *xout, double *yout, int *npos, int line_start, 
                int *segment_list, int *nsegments, double xmin, double xmax);
void sort_segments(double *yval, int *ysort, int nsegments);
void mapwrap(double *xin, double *yin, int *nin,
             int *wraplist, int *npoly,
             double *xout, double *yout, int *nout,
             int *poly, double *xmin, double *xmax, double *antarctica);
void close_antarctica(double *xout, double *yout, int *npos,
                      int line_start, int *segment_list, 
                      double xmin, double xmax,double antarctica);

// BUG: crashes on Antarctica
// which has only 1 crossing, not 2.
// So it is a very special (unique) case.
// How do we solve it?
void mapwrap(double *xin, double *yin, int *nin,
             int *wraplist, int *npoly,
             double *xout, double *yout, int *nout,
             int *poly, double *xmin, double *xmax, double *antarctica) {

  int i,j,k;
  int npos, count_wrap, count_crossings, line_start, line_finish, count_line;
  int segment_list[MAX_SEGMENTS];
  double period, xi, ymid;
  char is_closed;

  period = *xmax - *xmin;

  npos=0;
  count_wrap=0; // count how many polylines have been wrapped
  count_line=0;  // keep track of which polyline we're treating

  for (i=0 ; i <= *nin ; i++) {
    xi = (i< *nin) ? xin[i] : NA_REAL;
    if (!ISNA(xi)) {
      // bring inside [ xmin, xmax ]
      while (xi < *xmin) xi += period; 
      while (xi > *xmax) xi -= period;
      // is it the first point of a new line? 
      // (don't look at xout for this: it has more NA's)
      if (i==0 || ISNA(xin[i-1])) {
        count_crossings = 0;
        count_line++;
        line_start = npos;
//        Rprintf("New polyline %i at position=%i/%i\n", count_line, i, npos);
        xout[npos] = xi; yout[npos] = yin[i]; npos++;
        if (npos >= *nout) Rf_error("overflow\n");
      }
      // have we just crossed the boundary?
      else if (abs(xi - xout[npos-1]) > period/2.) {
//        Rprintf(".....crossing at %i/%i?\n", i, npos);
        // very special case if the boundary is only "touched" e.g. (-.1, 0, -.1) -> not a real jump 
        // BUG : only do this if poly ???
        if (xi == *xmin) {
          xi= *xmax;
          xout[npos] = xi; yout[npos] = yin[i]; npos++;
          if (npos >= *nout) Rf_error("overflow\n");
        }
        else if (xi== *xmax) {
          xi = *xmin;
          xout[npos] = xi; yout[npos] = yin[i]; npos++;
          if (npos >= *nout) Rf_error("overflow\n");
        }
        else {
          count_crossings++;
//          Rprintf("    Boundary crossing %i at position=%i/%i\n", count_crossings, i, npos);
          // if this is the first segment, report this polyline as being wrapped
          if (count_crossings==1) wraplist[count_wrap++] = count_line;
          // if we were exactly on the boundary: no need to interpolate
          if (xout[npos-1] == *xmin) {
            if (npos+2 > *nout) Rf_error("Output vector too short.");
            xout[npos] = NA_REAL; xout[npos+1]= *xmax; xout[npos+2]=xi;
            yout[npos] = NA_REAL; yout[npos+1]= yout[npos-1]; yout[npos+2]=yin[i];
            segment_list[count_crossings-1] = npos + 1;
            npos += 3;
          }
          else if (xout[npos-1] == *xmax) {
            if (npos+2 > *nout) Rf_error("Output vector too short.");
            xout[npos] = NA_REAL; xout[npos+1]= *xmin; xout[npos+2]=xi;
            yout[npos] = NA_REAL; yout[npos+1]= yout[npos-1]; yout[npos+2]=yin[i];
            segment_list[count_crossings-1] = npos + 1;
            npos += 3;
          }
          else { // normal case
            if (npos >= *nout-4) Rf_error("overflow\n");
            // create interpolated points at boundaries
            if (xi < xout[npos-1]) {
              xout[npos]= *xmax; xout[npos+1]=NA_REAL; xout[npos+2]= *xmin; xout[npos+3]=xi;
              ymid = yin[i] + (yin[i]-yout[npos-1]) / (xi + period - xout[npos-1])
                                                     * (*xmax - xi - period);
            }
            else {
              xout[npos]= *xmin; xout[npos+1]=NA_REAL; xout[npos+2]= *xmax; xout[npos+3]=xi;
              ymid = yin[i] + (yin[i]-yout[npos-1]) / (xi - period - xout[npos-1])
                                                     * (*xmin - xi + period);
            }
            yout[npos]=ymid; yout[npos+1]=NA_REAL; yout[npos+2]=ymid; yout[npos+3]=yin[i];
//            Rprintf("x: %lf %lf %lf %lf\n",xout[npos-1],xout[npos],xout[npos+2],xout[npos+3]);
//            Rprintf("y: %lf %lf %lf %lf\n",yout[npos-1],yout[npos],yout[npos+2],yout[npos+3]);
            // store the start location of this new segment
            // note that the first segment is not stored in this way, but as line_start
            segment_list[count_crossings-1] = npos + 2;
            if (count_crossings >= MAX_SEGMENTS) Rf_error("Too many segments in line %i.\n",count_line);
            npos += 4;
          }
        }
      }
      // just an internal point
      else  {
        xout[npos] = xi; yout[npos] = yin[i]; npos++;
        if (npos >= *nout) Rf_error("overflow\n");
      }
    }
    else { // it is a NA entry that separates 2 polylines
// BUGLET: what if xin starts with 1 or more NA's ???
//   nothing much happens: counters are adapted when you have a real line starting
      line_finish = npos-1;
      xout[npos] = yout[npos] = NA_REAL; npos++;
      if (npos >= *nout) Rf_error("overflow\n");
      // xout may be different if they fall exactly on the boundary
      is_closed = (xout[line_start] == xout[line_finish]);
//      Rprintf("finished polyline %i, nseg=%i, line_start=%i, line_finish=%i, closed=%i\n", 
//              count_line, count_crossings, line_start, line_finish, is_closed);
      // if the polygon doesn't close, that counts as a crossing
      if ( (count_crossings || !(is_closed)) && *poly) {
        if (yout[line_start] != yout[line_finish]) 
          Rf_error("Not a closed polygon. Are you sure this is polygon data?");
        // total length of the segments is npos - line_start + 1
        // but this can change a bit!
        // so npos can change upon return!
        if (count_crossings + !(is_closed) == 1) {//Antarctica
//          Rprintf("Antarctica!\n");
          close_antarctica(xout, yout, &npos, line_start,
                           segment_list, *xmin, *xmax, *antarctica);
        }
        else {
          split_poly(xout, yout, &npos, line_start, segment_list, &count_crossings, *xmin, *xmax);
          // on return, count_crossings contains the corrected number of crossings
          npoly[count_wrap-1]=(count_crossings / 2) + 1;
        }
      }
    }
  }
  *nout = npos-1; // drop the last NA
}

void close_antarctica(double *xout, double *yout, int *npos,
                      int line_start, int *segment_list, 
                      double xmin, double ymax,double antarctica) {
    // three options:
    //   - don't close the polygon, just re-align
    //   - close without "esthetic" extensions to fixed latitude
    //   - close with extra line at e.g. -90S.
  int i, j, k, line_length, line_finish;
  double *xbuf, *ybuf, *xo, *yo;
  double dx;
  char is_closed;

  line_length = *npos - line_start + 1;
  line_finish = *npos - 2 ;
  is_closed = (xout[line_start] == xout[line_finish]);
  if (is_closed) {
    xbuf = (double*) malloc((line_length + 10) * sizeof(double));
    ybuf = (double*) malloc((line_length + 10) * sizeof(double));
//    Rprintf("Buffer length: %i\n",line_length+10);
//    Rprintf("line_start=%i, line_finish=%i, line_length=%i, npos=%i, seg1=%i\n",line_start,line_finish,line_length,*npos,segment_list[0]);
  // write to buffer and re-align
    j=0;
    for (i=segment_list[0]; i < line_finish; i++) {
      xbuf[j] = xout[i];
      ybuf[j] = yout[i];
      j++;
    }
    for (i=line_start+1; i<segment_list[0]-1; i++) {
      xbuf[j] = xout[i];
      ybuf[j] = yout[i];
      j++;
    }

    // write back to xout
//    Rprintf("Write the cold island back to xout. %i \n", j);
    i=line_start;
    for (k=0; k < j  ; k++) { xout[i] = xbuf[k]; yout[i] = ybuf[k]; i++; }
//    Rprintf("Freeing the buffers................... \n");
    free(xbuf);
    free(ybuf);
  } else i=*npos-1; // the polyline was correctly alligned: nothing needs changing

  // estethics...
  if (antarctica < 0) {
//    Rprintf("Making pretty closure for Antarctica.\n");
    xout[i] = xout[i-1]; yout[i] = antarctica; i++;
    dx = (xout[i] - xout[line_start])/3.;
    for (j=0 ; j<3 ; j++) {
      yout[i] = antarctica;
      xout[i] = xout[i-1] - dx;
      i++;
    }
    xout[i] = xout[line_start];
    yout[i] = antarctica;
    i++;
    xout[i] = xout[line_start];
    yout[i] = yout[line_start];
    i++;
  }
  xout[i] = NA_REAL;
  yout[i] = NA_REAL;
  i++;
  *npos = i;
}

void split_poly(double *xout, double *yout, int* npos, int line_start, 
                int *segment_list, int *nsegments, double xmin, double xmax) {
  int i,j,k,m, ep, firstseg,  finishpoint, npoly, newlength, segnum, compare, line_length, line_finish;
  int llen, is_constant;
  char closed, no_append;
  int ysort[MAX_SEGMENTS], lused[MAX_SEGMENTS], poly[MAX_SEGMENTS];
  double ystart[MAX_SEGMENTS], yend[MAX_SEGMENTS];
  double *xbuf, *ybuf;
  double *xo, *yo, *xb, *yb;

  // the first segment may not start at the boundary
  // in that case, it should be appended to the last segment.
  // unless there is a boundary jump just there. Unlikely but possible.
  // IF we are SURE the polygon was 'closed', we can always just copy the first segment
  // to the back (without the repeated point!).
  // IS IT OK TO COMPARE Y VALUES AS DOUBLE?
  // Usually, *npos is pointing at the next unused position
  // so we should take *npos-2 for the last point of the current polyline
  // unless it is the last polyline of the dataset: then *npos - 1
  line_length = *npos - line_start + 1;
  line_finish = *npos - 2 ;
  no_append=0 ; //usually, the start of the line is appended to the last poly-segment
  if (yout[line_start] != yout[line_finish]) {
//    Rprintf("(%lf,%lf) vs (%lf,%lf)\n",xout[line_finish], yout[line_finish], xout[line_start],yout[line_start]);
    Rf_error("Polygon was not correctly closed! Can not wrap.\n");
  }
  if ( (xout[line_start]== xmin && xout[line_finish==xmax] ) ||
       (xout[line_start]== xmax && xout[line_finish==xmin] ) ) {
    is_constant = 1;
    llen = segment_list[0] - 2; // end of the first part ("pre-segment")
//    Rprintf("Checking line structure. line_start=%i, llen=%i\n",line_start,llen);
    i=line_start;
    while (i < llen && is_constant) is_constant = (xout[i++] == xout[line_start]); 
    if (is_constant) {
//      Rprintf("---Actually 2 crossings are bogus!\n");
      for (i=line_start; i < llen; i++) xout[i] = xout[line_finish];
      // eliminating the first line break: all segment locations change etc 
      for (i=llen; i<*npos - 2; i++) {xout[i]=xout[i+2]; yout[i]=yout[i+2];}
      *npos -= 2;
      (*nsegments)--;
      if (*nsegments == 0) return;
      for (i=0; i< *nsegments; i++) segment_list[i] = segment_list[i+1] - 2;
    }
    else { // use the beginning as an extra segment
//      Rprintf("---Line start is an extra segment.\n");
      segment_list[(*nsegments)++] = line_start;
      no_append=1;
    }
  }
  else if (xout[line_start] != xout[line_finish]) {
//    Rprintf("(%lf,%lf) vs (%lf,%lf)\n",xout[line_finish], yout[line_finish], xout[line_start],yout[line_start]);
    Rf_error("Polygon was not correctly closed! Can not wrap.\n");
  }

  xbuf = (double*) malloc((line_length + 3* *nsegments) * sizeof(double));
  ybuf = (double*) malloc((line_length + 3* *nsegments) * sizeof(double));
//  Rprintf("Buffer length: %i\n",line_length+3* *nsegments);
  if (*nsegments%2 == 1) {
    Rf_error("nsegments must be even!\n");
  }
  // make vector of starting y values, sort it by value
  
  npoly = *nsegments / 2 + 1;
//  Rprintf("Making segment vector.\n");
  for (i=0; i< *nsegments; i++) {
    ystart[i] = yout[segment_list[i]];
//    Rprintf("%i : %lf %lf\n",i,xout[segment_list[i]], ystart[i]);
    if (i) yend[i-1] = ystart[i] ;
    lused[i] = 0;
  }
//  Rprintf("Calling sort.\n");
  sort_segments(ystart, ysort, *nsegments);
//  for (i=0; i< *nsegments; i++) Rprintf("    sorted %i is actual segment %i\n", i, ysort[i]);

  xb = xbuf;
  yb = ybuf;
  newlength=0;
  // build the polygons
  firstseg=0;
  for (i=0; i< npoly; i++) {
    while (lused[firstseg] && firstseg < MAX_SEGMENTS) firstseg++;
    if (firstseg==MAX_SEGMENTS) Rf_error("segment closure error\n");
    // firstseg and finishpoint follow the order along the Y axis (not along the polygon)
    finishpoint = (firstseg % 2) ? firstseg - 1 : firstseg + 1;
//    Rprintf("polygon %i : start at sorted point %i, finish at %i\n",i, firstseg, finishpoint);
    j = firstseg;
    closed = 0;
    k = 0;
    while (!closed) {
//      Rprintf("polygon %i : add line %i/%i\n",i, j, ysort[j]);
      lused[j] = 1;
      if (k >= MAX_SEGMENTS) Rf_error("Polygon error.");
      poly[k++] = j;
      // now find out at which (ordered) point the current segment ends
      // we know that ysort[j]+1 is the next line segment along the polygon
      // so it's start point is the end point of segment j
      // SO: find ep such that ysort[ep] == ysort[j]+1
      // IF ysort[j]+1 == nsegments, replace by 0
      compare = (ysort[j]== *nsegments-1) ? 0 : ysort[j] + 1;
      if (compare >= *nsegments) Rf_error("nsegment overshoot.");
//      ep = firstseg+1;
      ep=0;
      while (ysort[ep] != compare && ep < *nsegments) ep++;
      if (ep == *nsegments) Rf_error("ep overshoot.");
//      Rprintf("    ep=%i, compare=%i, finishpoint=%i\n",ep, compare, finishpoint);
      // OK, now see if ep also closes our sub-polygon:
      // if not, we add the line starting in ep as a new segment
      if (ep == finishpoint) closed=1;
      else {
        j = (ep%2) ? ep - 1 : ep + 1;
      }
    }
    // write a polygon to buffer in the right order
//    Rprintf("Write segment %i (length %i) to buffer.\n",i,k );
    for (m=0; m<k; m++) {
      segnum = ysort[poly[m]];
//      Rprintf("    segment %i:%i (actual %i) at position %i\n", m, poly[m], segnum, segment_list[segnum]);
//      Rprintf("      buffer position at start:%i\n", newlength);
      xo = xout + segment_list[segnum];
      yo = yout + segment_list[segnum];
      // the very last polyline is not followed by NA!!!
      while (!ISNA(*xo) && xo < xout+ *npos) {xbuf[newlength] = *xo++; ybuf[newlength] = *yo++; newlength++;}
      if (segnum== *nsegments-1 && !no_append) { // append the first part of polyline to last segment
//        Rprintf("append beginning of polyline. newlength=%i\n", newlength);
        xo = xout + line_start + 1;
        yo = yout + line_start + 1;
        while (!ISNA(*xo)) {xbuf[newlength] = *xo++; ybuf[newlength] = *yo++; newlength++;}
      }
    }
    // repeat the first point of this segment to close the polygon
    xbuf[newlength] = xout[segment_list[ysort[poly[0]]]];
    ybuf[newlength] = yout[segment_list[ysort[poly[0]]]];
    newlength++;
    // separator
    xbuf[newlength] = ybuf[newlength] = NA_REAL;
    newlength++;
  }
 
  // overwrite xout with buffer
//  Rprintf("Copy buffer to output vector.\n");
  xo = xout + line_start;
  yo = yout + line_start;
  for (i=0; i<newlength; i++) {*(xo++) = xbuf[i]; *(yo++) = ybuf[i];}
//  Rprintf("Output length: start at %i; len from %i to %i.\n",line_start, *npos-line_start+1, newlength);
  *npos = line_start + newlength;

  // finished
  free(xbuf);
  free(ybuf);
}

// A very simple sorting algorithm
// No point in making anything more sophisticated, since we will typically be
// dealing with 2 or 4 values at most.
void sort_segments(double *yval, int *ysort, int nsegments) {
  int i, j, cc;
//  Rprintf("sort:\n");
  for (i=0; i<nsegments ; i++) {
    cc=0;
    for (j=0; j<nsegments; j++) if (yval[i] < yval[j]) cc++;
    ysort[cc] = i;
//    Rprintf("%i: %lf, order=%i\n",i,yval[i],cc);
  }
}


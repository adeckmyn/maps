// Line & Polygon clipping for global maps
// Part of R-package 'maps'
// (c) Alex Deckmyn, 2017
// distributed under GPL-2

/*
Method:
  - boundary crossings are interpolated to the boundary value
  - all crossings are indexed and sorted by latitude value
  - new sub-polygons are constructed one by one
*/

#include "R.h"

#define MAX_SEGMENTS 50
void map_restrict(double *xin, double *yin, int *nin,
                 double *xout, double *yout, int *nout,
                 double *xmin, double *xmax);
void map_restrict_poly (double* xin, double *yin, int *nin,
                        double* xout, double *yout, int *nout,
                        double *xlim, int *inside, int *poly, int *npoly);
void construct_poly(double *xout, double *yout, int *segment_start_list, int *segment_finish_list, 
                    int count_segments, int merge, int *line_end);

/* ============================================================= */

// Very simple line clipping: no polygon awareness
// Normally, the final output vector will be shorter than input,
// but it is theoretically possible to have an output vector that is longer!
void map_restrict(double *xin,  double *yin,  int *nin,
                  double *xout, double *yout, int *nout,
                  double *xmin, double *xmax) {

  int i,j;

  i=j=0;
  while (i < *nin) {
    while ( i < *nin && (ISNA(xin[i]) || xin[i] < *xmin || xin[i] > *xmax) ) i++;
    if (i == *nin) break;
    if (i>0 && !ISNA(xin[i-1])) {
      xout[j] = (xin[i-1] < *xmin) ? *xmin : *xmax;
      yout[j] = yin[i-1] + (yin[i]-yin[i-1])/(xin[i]-xin[i-1])*(xout[j]-xin[i-1]);
      j++;
      if (j >= *nout) Rf_error("Output buffer too small.");
    }
    while (i < *nin && !ISNA(xin[i]) && xin[i] >= *xmin && xin[i] <= *xmax) {
      xout[j] = xin[i];
      yout[j] = yin[i];
      j++;
      if (j >= *nout) Rf_error("Output buffer too small.");
      i++;
    }
    if (i == *nin) break;
    if (!ISNA(xin[i])) {
      xout[j] = (xin[i] < *xmin) ? *xmin : *xmax;
      yout[j] = yin[i-1] + (yin[i]-yin[i-1])/(xin[i]-xin[i-1])*(xout[j]-xin[i-1]);
      j++;
      if (j >= *nout) Rf_error("Output buffer too small.");
    }
    xout[j] = yout[j] = NA_REAL;
    j++;
    if (j >= *nout) Rf_error("Output buffer too small.");
  }
  if (ISNA(xout[j-1])) j--;
  *nout = j;
}

// call 4 times.
// only consider one boundary at a time
// 'inside' is +/- 1
// inside = -1 signifies that  x < xlim is "inside"
//                             x > xlim is clipped

void map_restrict_poly (double* xin, double *yin, int *nin,
                        double* xout, double *yout, int *nout,
                        double *xlim, int *inside, int *poly, int *npoly) {
  int i, j, count_line, count_segments, position, ppos, merge;
  int segment_start_list[MAX_SEGMENTS], segment_finish_list[MAX_SEGMENTS];
  double ymid;

  count_segments=0;  // count how many internal line segments we get for a polyline
  count_line=0;  // keep track of which polyline we're treating
  i = j = 0;
  while (i < *nin) {
    if (!ISNA(xin[i])) {
      // is it the first point of a new polyline?
      if (i==0 || ISNA(xin[i-1])) {
        position = ((xin[i] > *xlim) - (xin[i] < *xlim)) * *inside;
        // +1 for "inside", -1 for "outside, 0 if xin[i]=xlim
        count_segments = 0;
        npoly[count_line++] = 0;
        ppos = -2; // just a label...
        Rprintf("New polyline %i at index=%i/%i position=%i ppos=%i\n", count_line, i, j, position, ppos);
      }

      // position=0 at this point can only happen if a polyline _starts_ at exactly xlim
      // In that case we do nothing, just continue
      if (position == 0) {
        ppos = 0;
        while (i < *nin && !ISNA(xin[i]) && (position = ((xin[i] > *xlim) - (xin[i] < *xlim)) * *inside)==0) i++;
      }

      // an internal point
      else if (position ==  1) {
        if (j>0 && !ISNA(xout[j-1])) {
          xout[j] = yout[j] = NA_REAL; j++;
          if (j >= *nout) Rf_error("Output vector too short!\n");
        }
        if (*poly) segment_start_list[count_segments++] = j ;
        Rprintf("  count_segments=%i, i=%i, j=%i, pos=%i\n",count_segments, i, j, position);
        if (ppos==0) { // previous point was on boundary -> include it
          xout[j] = xin[i-1]; yout[j] = yin[i-1]; j++;
          if (j >= *nout) Rf_error("Output vector too short!\n");
        }
        else if (ppos== -1){
          Rprintf(".....crossing at %i/%i?\n", i, j);
          ymid = yin[i] + (yin[i-1]-yin[i]) / (xin[i-1] - xin[i]) * (*xlim - xin[i]);
          xout[j] = *xlim; yout[j] = ymid; j++;
          if (j >= *nout) Rf_error("Output vector too short!\n");
        }
        ppos = position;
        xout[j] = xin[i]; yout[j] = yin[i]; j++;
        if (j >= *nout) Rf_error("Output vector too short!\n");
        i++;
        while ( i < *nin && !ISNA(xin[i]) && 
               (position = ((xin[i] > *xlim) - (xin[i] < *xlim)) * *inside) >= 0) {
          xout[j] = xin[i]; yout[j] = yin[i]; j++; i++;
          ppos = position;
          if (j >= *nout) Rf_error("Output vector too short!\n");
        }
        if (position < 0 && ppos > 0) {
          Rprintf(".....crossing at %i/%i?\n", i, j);
          ymid = yin[i] + (yin[i-1]-yin[i]) / (xin[i-1] - xin[i]) * (*xlim - xin[i]);
          xout[j] = *xlim; yout[j] = ymid; j++;
        }
        if (*poly) segment_finish_list[count_segments-1] = j - 1;
      }

      // external point
      else if (position == -1) {
        ppos = position;
        i++;
        while ( i < *nin && !ISNA(xin[i]) && 
               (position = ((xin[i] > *xlim) - (xin[i] < *xlim)) * *inside) <= 0) {
          ppos = position;
          i++;
        }
      }
    }

    if (ISNA(xin[i]) || i == *nin) { // a polyline is finished

      if ( *poly && count_segments > 0) {
        // if count_segments==1 && position==1 -> nothing left to do, the complete polygon is 'inside'
        Rprintf("finished polyline %i, nseg=%i\n      i=%i j=%i, position=%i\n", count_line, count_segments, i, j, position);
        if (position > 0) { //we finished (and thus also started) inside the region,
                        //so the first and last segment are to be merged
// check for polygon closure, if applicable
          if ( (yout[segment_start_list[0]] != yout[segment_finish_list[count_segments-1]]) ||
               (xout[segment_start_list[0]] != xout[segment_finish_list[count_segments-1]]) ){
            Rprintf("p1: %i : %lf %lf\n", segment_start_list[0], xout[segment_start_list[0]], yout[segment_start_list[0]]);  
            Rprintf("p2: %i : %lf %lf\n", segment_finish_list[count_segments-1], 
                           xout[segment_finish_list[count_segments-1]], yout[segment_finish_list[count_segments-1]]);  

            Rf_error("Polygon not correctly closed.");
          }
          merge = 1 ; // if there is only 1 segment, no need to merge...
        }
        else merge = 0;
        Rprintf("position=%i, merge=%i\n",position, merge);
        // (over-)estimate extra output space needed
        if (count_segments - merge > 0) {
          if (j >= *nout - 3*count_segments) Rf_error("Output vector too short! 2\n");
          construct_poly(xout, yout, segment_start_list, 
                          segment_finish_list, count_segments, merge, &j);
        }
      }
      if (ISNA(xin[i]) && j>0 && !ISNA(xout[j-1])) {
        xout[j] = yout[j] = NA_REAL; j++;
        if (j >= *nout) Rf_error("Output vector too short! 1\n");
      }
      while (i < *nin && ISNA(xin[i])) i++;
    }
  }
  *nout = j;
}


void construct_poly(double *xout, double *yout, int *segment_start_list, int *segment_finish_list, 
                    int count_segments, int merge, int *line_end) {

  int i,j,k,m,n,pe, pstart;
  int remaining, llen, closed, line_start, end_point, poly_len;
  int sorted_start_list[MAX_SEGMENTS], ordered_finish_list[MAX_SEGMENTS];
  int is_used[MAX_SEGMENTS], poly[MAX_SEGMENTS];
  double *xbuf, *ybuf;

  llen = segment_finish_list[count_segments-1] - segment_start_list[0] +10*count_segments;
  xbuf = (double*) malloc( llen * sizeof(double));
  ybuf = (double*) malloc( llen * sizeof(double));
  Rprintf("llen=%i, buffer length=%i\n",llen, llen+count_segments*10);
  for (i=0; i<count_segments; i++) {
     Rprintf("   %i start=%i %lf finish=%i %lf\n",i, segment_start_list[i],xout[segment_start_list[i]]
         ,segment_finish_list[i],xout[segment_finish_list[i]]);
  }

  line_start = segment_start_list[0];
//  *line_end = segment_finish_list[count_segments-1];
  if (merge) {
    count_segments--;
    segment_start_list[0] = segment_start_list[count_segments];
  }

  Rprintf("construct_poly: count=%i, merge=%i\n",count_segments, merge);
  //  sorted_start_list[]   : element 0 contains the number of the segment with largest y value
  //  ordered_finish_list[] : element 0 contains the order of the end point of segment 0
  for (i=0; i < count_segments ; i++) {
    ordered_finish_list[i] = k = 0;
    for (j=0; j< count_segments; j++) {
      ordered_finish_list[i] += (yout[segment_finish_list[j]] > yout[segment_finish_list[i]]);
      k += (yout[segment_start_list[j]] > yout[segment_start_list[i]]);
    }
    sorted_start_list[k] = i ;
    is_used[i] = 0;
  }
  for (i=0; i<count_segments; i++) {
     Rprintf("   %i start=%i %lf %lf finish=%i %lf %lf \n",i, segment_start_list[i],xout[segment_start_list[i]],
                yout[segment_start_list[i]],segment_finish_list[i],xout[segment_finish_list[i]],yout[segment_finish_list[i]]);
  }
  for (i=0; i<count_segments; i++) {
    Rprintf("%i : start:%i finish:%i\n",i,sorted_start_list[i], ordered_finish_list[i] );}
  // reconstruct polygons, write to buffer
  remaining = count_segments;
  i = 0;
  n = 0;
  while (remaining > 0) {
    // add segments until it closes
    while (i < count_segments && is_used[i]) i++;
    if (i == count_segments) break; //shouldn't happen!
    Rprintf("Polygon start segment:%i -> %i\n",i, sorted_start_list[i]);
    end_point = i;
    closed = 0;
    poly_len=0;
    while (!closed) {
      poly[poly_len++] = i; //sorted_start_list[i];
      if (poly_len>=MAX_SEGMENTS) Rf_error("polygon explosion.");
      Rprintf("  %i adding segment %i -> %i\n",poly_len, i, sorted_start_list[i] );
      is_used[i] = 1;
      remaining--;
      pe = ordered_finish_list[sorted_start_list[i]];
      Rprintf("  ..... ends at : %i\n",pe);
      if (pe == end_point) closed = 1;
      else {
        i = pe;
        if (is_used[i]) Rf_error("Closure error.");
      }
    }
    // write polygon to buffer
    // TODO : add some extra interpolating points inbetween the segments
    //        they are "invisible" without projection, but you want
    //        a projection of the xlim boundary to look smooth
    pstart = n;
    for (j=0; j<poly_len ; j++) {
      Rprintf("  writing to buffer: j=%i : %i, n=%i\n",j, poly[j],sorted_start_list[poly[j]], n);
      m=sorted_start_list[poly[j]];
      if (m==0 && merge) {
        for (k = segment_start_list[m] ; k < *line_end ; k++) {
          xbuf[n] = xout[k];
          ybuf[n] = yout[k];
          n++;
          if (n >= llen) Rf_error("Buffer too short.");
        }
        for (k=line_start + 1 ; k <= segment_finish_list[0] ; k++) {
          xbuf[n] = xout[k];
          ybuf[n] = yout[k];
          n++;
          if (n >= llen) Rf_error("Buffer too short.");
        }
      }
      else {
        for (k = segment_start_list[m] ; k <= segment_finish_list[m] ; k++) {
          xbuf[n] = xout[k];
          ybuf[n] = yout[k];
          n++;
          if (n >= llen) Rf_error("Buffer too short.");
        }
      }
    }
    // close the polygon
    Rprintf("closure at n=%i\n",n);
    xbuf[n] = xbuf[pstart];
    ybuf[n] = ybuf[pstart];
    n++;
    if (n >= llen) Rf_error("Buffer too short.");
    xbuf[n] = NA_REAL;
    ybuf[n] = NA_REAL;
    n++;
  }

  Rprintf("total sub-polygon length: %i\n",n);
  // write buffer to xout
  for (i=0 ; i<n; i++) {
    xout[line_start + i] = xbuf[i];
    yout[line_start + i] = ybuf[i];
  }
  *line_end = line_start + n -1; //drop final NA
  free(xbuf);
  free(ybuf);
}



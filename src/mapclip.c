/* Line & Polygon clipping for maps */
/* Part of R-package 'maps' */
/* Alex Deckmyn, 2017 */
/* distributed under GPL-2 */

/*
Method:
  - boundary crossings are interpolated to the boundary value
  - all crossings are indexed and sorted by latitude value
  - new sub-polygons are constructed one by one
*/

#include "R.h"

#define MAX_SEGMENTS 50
#define MAX_INTERP 10

void map_wrap_poly(double *xin, double *yin, int *nin,
              double *xout, double *yout, int *nout,
              double *xmin, double *xmax,
              int *poly, int *npoly, double *antarctica) ;

void map_clip_poly (double* xin, double *yin, int *nin,
                    double* xout, double *yout, int *nout,
                    double *xlim, int *inside, int *poly, int *npoly);

void construct_poly(double *xout, double *yout,
                    int *segment_start_list, int *segment_finish_list,
                    int count_segments, int *line_end, int *pcount, int sides);

void close_antarctica(double *xout, double *yout,
                      int *segment_start_list, int *segment_finish_list,
                      int *count_segments, double minlat);

void merge_segments(double *xout, double *yout,
                    int *segment_start_list, int *segment_finish_list,
                    int *count_segments);

/* call 4 times (yeah, I know, very efficient...) */
/* only consider one boundary at a time */
/* 'inside' is +/- 1 */
/* inside = -1 signifies that  x < xlim is "inside" */
/*                             x > xlim is clipped */

void map_clip_poly (double* xin, double *yin, int *nin,
                    double* xout, double *yout, int *nout,
                    double *xlim, int *inside, int *poly, int *npoly) {
  int i, j, count_line, count_segments, position, ppos, pcount;
  int segment_start_list[MAX_SEGMENTS], segment_finish_list[MAX_SEGMENTS];
  double ymid;

  count_segments=0;  /* count how many internal line segments we get for a polyline */
  count_line=0;  /* keep track of which polyline we're treating */
  position=2; /* an "impossible" value, just to keep the compiler happy */
  i = j = 0;
  while (i < *nin) {
    if (!ISNA(xin[i])) {
      /* is it the first point of a new polyline? */
      if (i==0 || ISNA(xin[i-1])) {
        position = ((xin[i] > *xlim) - (xin[i] < *xlim)) * *inside;
        /* +1 for "inside", -1 for "outside, 0 if xin[i]=xlim */
        count_segments = 0;
        npoly[count_line++] = 0;
        ppos = -2; /* just a label... */
      }

      /* position=0 at this point can only happen if a polyline _starts_ at exactly xlim */
      /* In that case we do nothing, just continue */
      if (position == 0) {
        ppos = 0;
        while (i < *nin && !ISNA(xin[i]) &&
               (position = ((xin[i] > *xlim) - (xin[i] < *xlim)) * *inside)==0) i++;
      }

      /* an internal point */
      else if (position ==  1) {
        if (j>0 && !ISNA(xout[j-1])) {
          xout[j] = yout[j] = NA_REAL; j++;
          if (j >= *nout) error("Output vector too short!\n");
        }
        if (*poly) segment_start_list[count_segments++] = j ;
        if (ppos==0) { /* previous point was on boundary -> include it */
          xout[j] = xin[i-1]; yout[j] = yin[i-1]; j++;
          if (j >= *nout) error("Output vector too short!\n");
        }
        else if (ppos== -1){
          ymid = yin[i] + (yin[i-1]-yin[i]) / (xin[i-1] - xin[i]) * (*xlim - xin[i]);
          xout[j] = *xlim; yout[j] = ymid; j++;
          if (j >= *nout) error("Output vector too short!\n");
        }
        ppos = position;
        xout[j] = xin[i]; yout[j] = yin[i]; j++;
        if (j >= *nout) error("Output vector too short!\n");
        i++;
        while ( i < *nin && !ISNA(xin[i]) &&
               (position = ((xin[i] > *xlim) - (xin[i] < *xlim)) * *inside) >= 0) {
          xout[j] = xin[i]; yout[j] = yin[i]; j++; i++;
          ppos = position;
          if (j >= *nout) error("Output vector too short!\n");
        }
        if (position < 0 && ppos > 0) {
          ymid = yin[i] + (yin[i-1]-yin[i]) / (xin[i-1] - xin[i]) * (*xlim - xin[i]);
          xout[j] = *xlim; yout[j] = ymid; j++;
          if (j >= *nout) error("Output vector too short!\n");
        }
        if (*poly) segment_finish_list[count_segments-1] = j - 1;
      }

      /* external point */
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

    if (ISNA(xin[i]) || i == *nin) { /* a polyline is finished */

      if ( *poly && count_segments > 0) {
        /* if count_segments==1 && position==1 -> nothing left to do, the complete polygon is 'inside' */
        if (position > 0) { /*we finished (and thus also started) inside the region, */
                            /*so the first and last segment are to be merged */
          /* check polygon closure */
          if ( (yout[segment_start_list[0]] != yout[segment_finish_list[count_segments-1]]) ||
               (xout[segment_start_list[0]] != xout[segment_finish_list[count_segments-1]]) ){
            error("Polygon not correctly closed.");
          }
          if (count_segments > 1) {
            merge_segments(xout, yout, segment_start_list, segment_finish_list, &count_segments);
            j = segment_finish_list[count_segments-1] + 1;
          }
          else count_segments = 0;
        }
        /* (over-)estimate extra output space needed */
        if (count_segments > 0) { /* if there is only 1 segment, no need to do anything */
          if (j >= *nout - (3+MAX_INTERP)*count_segments) error("Output vector too short!\n");
          construct_poly(xout, yout, segment_start_list,
                          segment_finish_list, count_segments, &j, &pcount, 1);
          npoly[count_line-1] = pcount;
        }
        else npoly[count_line-1]=1;
      }
      if (ISNA(xin[i]) && j>0 && !ISNA(xout[j-1])) {
        xout[j] = yout[j] = NA_REAL; j++;
        if (j >= *nout) error("Output vector too short!\n");
      }
      while (i < *nin && ISNA(xin[i])) i++;
    }
  }
  if (ISNA(xout[j-1])) j--;
  *nout = j;
}


void construct_poly(double *xout, double *yout,
                    int *segment_start_list, int *segment_finish_list,
                    int count_segments, int *line_end, int *pcount, int sides) {

  int i,j,k,m,n,pe, pstart;
  int remaining, buflen, closed, line_start, end_point, poly_len;
  int sorted_start_list[MAX_SEGMENTS], ordered_finish_list[MAX_SEGMENTS];
  int is_used[MAX_SEGMENTS], poly[MAX_SEGMENTS];
  double *xbuf, *ybuf, x0, y0, dy;

  if (count_segments > MAX_SEGMENTS)
    error("Too many line segments. Increase MAX_SEGMENTS and re-compile.");

  buflen = segment_finish_list[count_segments-1] - segment_start_list[0] + (3+MAX_INTERP)*count_segments;
  xbuf = (double*) R_alloc( buflen , sizeof(double));
  ybuf = (double*) R_alloc( buflen , sizeof(double));

  line_start = segment_start_list[0];
  for (i=0; i < count_segments ; i++) sorted_start_list[i] = 0 ;  

  /*  sorted_start_list[]   : element 0 contains the number of the segment with largest y value */
  /*  ordered_finish_list[] : element 0 contains the order of the end point of segment 0 */
  for (i=0; i < count_segments ; i++) {
    ordered_finish_list[i] = k = 0;
    for (j=0; j< count_segments; j++) {
      ordered_finish_list[i] += (yout[segment_finish_list[j]] > yout[segment_finish_list[i]]);
      if (i > j && yout[segment_finish_list[j]] == yout[segment_finish_list[i]]) {
//        Rprintf("Coinciding or crossing borders detected.\n");
        ordered_finish_list[i]++;
      }
      k += (yout[segment_start_list[j]] > yout[segment_start_list[i]]);
      if (i < j && yout[segment_start_list[j]] == yout[segment_start_list[i]]) {
//        Rprintf("Coinciding or crossing borders detected.\n");
        k++;
      }
    }
  /* NOTE: in case of "wrong" polygons (e.g. strange 1d lines) 
   * sorted_start_list may not get set correctly */
    sorted_start_list[k] = i ;
    is_used[i] = 0;
  }
  /* reconstruct polygons, write to buffer */
  remaining = count_segments;
  n = 0;
  *pcount = 0;
  while (remaining > 0) {
    /* start from the segment with highest y starting value: sorted_start_list[0] */
    /* add segments until it "closes" */
    *pcount += 1;
    i = *pcount -1;
    while (i < count_segments && is_used[i]) i++;
    if (i == count_segments) error("Polygon closure error. No segments left.\n") ;
    /* do we have polygons on both "sides" (wrapping) or only one (clipping) ? */
    /* if sides=2, every end point of a segment is also the starting point */
    /* of segment at the other 'side', so we count differently. */
    if (sides == 1) end_point = i;
    else end_point = (i % 2) ? i-1 : i+1 ;

    closed = 0;
    poly_len=0;
    while (!closed) {
      poly[poly_len++] = i; /* NOTE: the actual line segment is sorted_start_list[i], not i ! */
      if (poly_len > count_segments) error("More polygons than line segments.");
      is_used[i] = 1;
      remaining--;
      if (sorted_start_list[i] < 0 || sorted_start_list[i] >= count_segments) 
        error("Polygon segment ordering error.");
      pe = ordered_finish_list[sorted_start_list[i]];
      if (pe == end_point) closed = 1;
      else {
        if (sides==1) i = pe;
        else i = (pe%2) ? pe - 1 : pe + 1;
        if (i >= count_segments || is_used[i]) error("Sub-polygon closure error.");
      }
    }
    /* write polygon to buffer */
    pstart = n;
    for (j=0; j < poly_len ; j++) {
      m = sorted_start_list[poly[j]];
      /* add some interpolated points along the boundary */
      if (j>0) {
        x0 = xbuf[n-1];
        y0 = ybuf[n-1];
        dy = (yout[segment_start_list[m]] - y0)/MAX_INTERP ;
        if (dy != 0) for (k=1; k < MAX_INTERP; k++) {
          xbuf[n] = x0;
          ybuf[n] = y0 + k*dy;
          n++;
          if (n >= buflen) error("Buffer too short.");
        }
      }
      /* now write the new segment */
      for (k = segment_start_list[m] ; k <= segment_finish_list[m] ; k++) {
        xbuf[n] = xout[k];
        ybuf[n] = yout[k];
        n++;
        if (n >= buflen) error("Buffer too short.");
      }
    }
    /* close the polygon (with some extra points) */
    x0 = xbuf[n-1];
    y0 = ybuf[n-1];
    dy = (ybuf[pstart] - y0)/MAX_INTERP ;
    if (dy != 0) {
      for (k=1; k < MAX_INTERP; k++) {
        xbuf[n] = x0;
        ybuf[n] = y0 + k*dy;
        n++;
        if (n >= buflen) error("Buffer too short.");
      }
      xbuf[n] = xbuf[pstart];
      ybuf[n] = ybuf[pstart];
      n++;
    }
    if (n >= buflen) error("Buffer too short.");
    xbuf[n] = NA_REAL;
    ybuf[n] = NA_REAL;
    n++;
    if (n >= buflen) error("Buffer too short.");
  }

  /* write completed buffer to xout */
  for (i=0 ; i<n; i++) {
    xout[line_start + i] = xbuf[i];
    yout[line_start + i] = ybuf[i];
  }
  *line_end = line_start + n -1; /*drop final NA */
/* we switched to R_alloc, so no need to free
  R_Free(xbuf);
  R_Free(ybuf);
*/
}


/* ============================================================= */

void map_wrap_poly(double *xin, double *yin, int *nin,
                   double *xout, double *yout, int *nout,
                   double *xmin, double *xmax,
                   int *poly, int *npoly, double *antarctica) {

  int i, j, count_segments, count_line, pcount;
  int segment_start_list[MAX_SEGMENTS], segment_finish_list[MAX_SEGMENTS];
  double period, xi, ymid;

  period = *xmax - *xmin;

  j=0;        /* position in output vector */
  count_line=0;  /* keep track of which polyline we're treating */

  for (i=0 ; i <= *nin ; i++) {
    xi = (i< *nin) ? xin[i] : NA_REAL;
    if (!ISNA(xi)) {
      /* bring inside [ xmin, xmax ] */
      while (xi < *xmin) xi += period;
      while (xi > *xmax) xi -= period;
      /* is it the first point of a new line? */
      if (i==0 || ISNA(xin[i-1]) ) {
        if (xi != *xmin && xi != *xmax) {
          count_segments = 1;
          segment_start_list[0] = j;
        }
        else {
          count_segments = 0;
        }
        npoly[count_line] = 1;
      }
      else if (count_segments == 0) { /* the polyline started ON the boundary, so nothing was written yet */
      /* PROBLEM: you can NEVER look forward, because xin[i+1] may need to be shifted */
        if (xi != *xmin && xi != *xmax) {
          count_segments = 1 ;
          segment_start_list[0] = j;
          xout[j] = (xi - *xmin < period/2.) ? *xmin : *xmax;
          yout[j] = yin[i-1];
          j++;
          if (j >= *nout) error("Output vector too short.");
        }
      }
      /* have we just crossed/hit the boundary? */
      /* we compare with xout[j-1], because xin[i-1] may be shifted by 'period' */
      else if (fabs(xi - xout[j-1]) > period/2.) {
        /* If we are exactly on the boundary, we adapt 'side' to the previous point */
        if (xi == *xmin) xi= *xmax;
        else if (xi == *xmax) xi = *xmin;
        else {
          /* if we were exactly on the boundary: no need to interpolate */
          if (xout[j-1] == *xmin || xout[j-1]== *xmax) {
            if (j+2 > *nout) error("Output vector too short.");
            xout[j] = yout[j] = NA_REAL;
            xout[j+1]= (xout[j-1]==*xmin) ? *xmax : *xmin;
            yout[j+1]= yout[j-1];
            if (*poly) {
              segment_finish_list[count_segments-1] = j - 1;
              segment_start_list[count_segments++] = j + 1;
            }
            j += 2;
          }
          else { /* normal 'crossing' case */
            if (j+3 >= *nout ) error("Output vector too short!\n");
            /* create interpolated points at boundaries */
            if (xi < xout[j-1]) {
              xout[j]= *xmax; xout[j+1]=NA_REAL; xout[j+2]= *xmin;
              ymid = yin[i] + (yin[i]-yout[j-1]) / (xi + period - xout[j-1])
                                                     * (*xmax - xi - period);
            }
            else {
              xout[j]= *xmin; xout[j+1]=NA_REAL; xout[j+2]= *xmax;
              ymid = yin[i] + (yin[i]-yout[j-1]) / (xi - period - xout[j-1])
                                                     * (*xmin - xi + period);
            }
            yout[j]=ymid; yout[j+1]=NA_REAL; yout[j+2]=ymid;
            /* store the start location of this new segment */
            if (*poly) {
              segment_finish_list[count_segments-1] = j ;
              segment_start_list[count_segments++] = j + 2;
              if (count_segments > MAX_SEGMENTS) error("Too many crossings in line %i.\n",count_line);
            }
            j += 3;
          }
        }
      }
      /* just an internal point BUT if it is xmin/xmax at the start of a polyline: don't write it out (yet) */
      if (count_segments > 0) {
        xout[j] = xi; yout[j] = yin[i]; j++;
        if (j >= *nout) error("Output vector too short!\n");
      }
    }
    else { /* it is a NA entry that separates 2 polylines */
      if (*poly) {
        segment_finish_list[count_segments-1] = j-1;
        if ( xout[j-1] != *xmin && xout[j-1] != *xmax) {
          /* check for polygon closure */
          if ( (yout[segment_start_list[0]] != yout[segment_finish_list[count_segments-1]]) ||
               (xout[segment_start_list[0]] != xout[segment_finish_list[count_segments-1]]) ){
            error("Polygon not correctly closed.");
          }
          if (count_segments > 1) {
            /* make sure all line segments start/finish on the borders: possibly move first segment to end. */
            merge_segments(xout, yout, segment_start_list, segment_finish_list, &count_segments);
            j = segment_finish_list[count_segments-1] + 1;
          } else count_segments = 0; /* island polygon: do nothing. */
        }
          /* BUGFIX: it could be an artificial wrapping boundary starting at xmin/xmax -> do nothing */
          /* -> CHECK FOR CLOSURE */
        else if (count_segments == 1 && xout[j-1] == xout[segment_start_list[0]] &&
                 yout[j-1] == yout[segment_start_list[0]] ) count_segments = 0;

        /* we don't check closure in yout : it /may/ be wrong due to starting at xmin/xmax */
        /* if the polygon doesn't close, that usually counts as a crossing */
        if ( count_segments % 2) { /* 1 (or odd #) crossing: must be Antarctica */
          if ( !ISNA(*antarctica)) {
            /* (over-)estimate extra output space needed */
            if (j >= *nout - MAX_INTERP - 5) error("Output vector too short!\n");
            if (count_segments > MAX_SEGMENTS - 2)  error("Can't add segment for Antarctic closing\n");
            close_antarctica(xout, yout, segment_start_list, segment_finish_list, &count_segments, *antarctica);
            j = segment_finish_list[count_segments-1] + 1;
          }
          else { /* drop the polygon completely: don't draw Antarctica */
            npoly[count_line] = 0;
            count_segments = 0;
            j = segment_start_list[0];
          }
        }
        if (count_segments > 1) {
          /* (over-)estimate extra output space needed */
          if (j >= *nout - (3 + MAX_INTERP)*count_segments) error("Output vector too short!\n");
          construct_poly(xout, yout, segment_start_list, segment_finish_list,
                         count_segments, &j, &pcount, 2);
          npoly[count_line] = pcount;
        }
      }
      if (!ISNA(xout[j-1])) {
        xout[j] = yout[j] = NA_REAL; j++;
        if (j >= *nout) error("Output vector too short!\n");
      }
      count_line++;
    }
  }
  if (ISNA(xout[j-1])) j--;
  *nout = j; /* drop the last NA */
}

void close_antarctica(double *xout, double *yout,
                      int *segment_start_list, int *segment_finish_list,
                      int *count_segments, double minlat) {
  int i, j, k;
  double x0, x1, dx;
/* 1. find the line segment that crosses the domain, to know it's direction */
/* 2. add a closing segment in other direction */
  if ( (minlat < -90.) || (minlat > -86)) warning("Closing Antarctica polygon at abnormal latitude: %lf.\n", minlat);

  i=0;
  while (i < *count_segments && xout[segment_start_list[i]] == xout[segment_finish_list[i]]) i++;

  if (i == *count_segments) error("Antarctica closure error.");
  x1 = xout[segment_start_list[i]];
  x0 = xout[segment_finish_list[i]];
  dx = (x1 - x0)/MAX_INTERP;

  j = segment_finish_list[*count_segments - 1] + 1;
  xout[j] = yout[j] = NA_REAL; j++;
  segment_start_list[*count_segments] = j ;
  xout[j] = x0; yout[j] = minlat; j++;
  for (k=1 ; k < MAX_INTERP ; k++){
    yout[j] = minlat;
    xout[j] = x0 + k*dx;
    j++;
  }
  xout[j] = x1; yout[j] = minlat;
  segment_finish_list[*count_segments] = j ;

  *count_segments += 1;
}

/* attach the first line segment to the end, so all segments start/stop on teh border */
void merge_segments(double *xout, double *yout,
                    int *segment_start_list, int *segment_finish_list,
                    int *count_segments) {
  int i, buflen;
  double *xbuf, *ybuf, *xo, *yo;

  buflen = segment_finish_list[0] - segment_start_list[0] + 1 ;
  xbuf = (double*) R_alloc(buflen , sizeof(double));
  ybuf = (double*) R_alloc(buflen , sizeof(double));

  xo = xout + segment_start_list[0];
  yo = yout + segment_start_list[0];
  for (i=0 ; i < buflen; i++) {
    xbuf[i] = *(xo++);
    ybuf[i] = *(yo++);
  }

  xo = xout + segment_start_list[0];
  yo = yout + segment_start_list[0];
  for (i=segment_start_list[1]; i <= segment_finish_list[*count_segments-1]; i++) {
    *(xo++) = xout[i];
    *(yo++) = yout[i];
  }

  /* write buffer to xout */
  /* skip the (repeated) first point! */
  for (i=1; i < buflen  ; i++) {
    *(xo++) = xbuf[i];
    *(yo++) = ybuf[i];
  }
  /* fix the segment lists */
  for(i=1; i< *count_segments - 1; i++) {
    segment_start_list[i]  = segment_start_list[i+1]  - buflen - 1;
  }
  for(i=0; i< *count_segments - 2; i++) {
    segment_finish_list[i] = segment_finish_list[i+1] - buflen - 1;
  }
  segment_finish_list[*count_segments - 2] =  segment_finish_list[*count_segments - 1] - 2;

  *count_segments -= 1;
/*
  R_Free(xbuf);
  R_Free(ybuf);
*/
}


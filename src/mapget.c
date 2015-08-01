/* 
#include "S.h" 
#include <R_ext/Arith.h>
*/
#include "R.h"
#include "map.h"

#undef Realloc

#define Seek(f,n)	fseek(f, (int)(n), 0)
#define Read(f,s,n)	fread((char *)(s), sizeof(*(s)), (int)(n), f)
#define Write(f,s,n)	fwrite((char *)(s), sizeof(*(s)), (int)(n), f)
#define Alloc(s,n,t)	s = (t *)calloc((unsigned)(n), sizeof(t))
#define Realloc(s,n,t)	s = (t *)realloc((char *)s, (unsigned)(n) * sizeof(t))

#define Roffset(r)	(sizeof(Region)+((r)-1)*sizeof(struct region_h))
#define Loffset(l)	(sizeof(int)+sizeof(Polyline)+((l)-1)*sizeof(struct line_h))

#define RBAD(s,a)	{ \
				if(rf) fclose(rf); \
				*retlines = -1; \
				error(s,a); \
				return; \
			}
#define LBAD(s,a)	{ \
				if(lf) fclose(lf); \
				if(maxsize) free(xy); \
				*nwhich = -1; \
				error(s,a); \
				return; \
			}
#define TBAD(s,a)	{ \
				if(lf) fclose(lf); \
				*type = -1; \
				error(s,a); \
				return; \
			}

#define swap(a,b) t=a;a=b;b=t
static void *SwapBuffer(void *p, unsigned num_items, unsigned item_size)
{
  char *buffer = (char*)p, t, i;
  if(item_size > 1) {
    for(;num_items;num_items--) {
      for(i=0;i<item_size/2;i++) {
	swap(*(buffer+i), *(buffer+item_size-i-1));
      }
      buffer += item_size;
    }
  }
  return p;
}
/* global variable for swapping */
static int swap_override = 0;
/* If the current machine is big-endian, swap the buffer.
 * Otherwise do nothing.
 * BTW: big-endian is "network byte order"
 */
static void *AdjustBuffer(void *p, unsigned num_items, unsigned item_size)
{
  /* int z=1; */
  /* unsigned char *c=(unsigned char *)&z; */

  /* ^ is xor */
  /* if((c[0]==0) ^ swap_override) */
  if(swap_override)
    return SwapBuffer(p, num_items, item_size);
  return p;
}
static void AdjustRegionH(struct region_h *rh, unsigned n)
{
  for(;n;n--,rh++) {
    AdjustBuffer(&rh->offset,1,sizeof(rh->offset));
    AdjustBuffer(&rh->nline,1,sizeof(rh->nline));
    AdjustBuffer(&rh->sw,4,sizeof(rh->sw.x));
  }
}
static void AdjustLineH(struct line_h *lh, unsigned n)
{
  for(;n;n--,lh++) {
    AdjustBuffer(&lh->offset,1,sizeof(lh->offset));
    AdjustBuffer(&lh->npair,1,sizeof(lh->npair));
    AdjustBuffer(&lh->left,2,sizeof(lh->left));
    AdjustBuffer(&lh->sw,4,sizeof(lh->sw.x));
  }
}

static void setrange(r, xy)
double *r;
struct pair xy;
{
	if(xy.x < r[XMIN]) r[XMIN] = xy.x;
	if(xy.x > r[XMAX]) r[XMAX] = xy.x;
	if(xy.y < r[YMIN]) r[YMIN] = xy.y;
	if(xy.y > r[YMAX]) r[YMAX] = xy.y;
}

/* this should already be in R !! */
void
char_to_ascii(int *n, char **s, int *result)
{
  int i;
  for(i=0; i < *n; i++) {
    result[i] = s[i][0];
  }
}

void
map_match(int *ntable, char **table, int *nx, char **x, int *result, 
	  int *exact)
{
  /* requires: table and x are sorted.  result is all zeros. */
  int ix,it=0;
  for(ix=0; ix < *nx; ix++) {
    /* compare x[ix] and table[it] */
    char *px,*pt;
    int done = 0;
    while(!done) {
      /* printf("comparing %s and %s\n",x[ix],table[it]); */
      for(px=x[ix],pt=table[it];*px;px++,pt++) {
	if(*px > *pt) {
	  /* match may be further in the table */
	  /* this also handles *pt == 0 */
	  it++; break;
	}
	if(*px < *pt) { done = 1; break; }
      }
      if(*px == '\0') {
	if(!*exact || *pt == '\0') result[it++] = ix+1;
	else break;
      }
      if(it == *ntable) return; 
    }
  }
}

static void name(s, data, suffix)
char *s, *data, *suffix;
{
  if(data == 0)
    PROBLEM "No R_MAP_DATA_DIR variable!!" RECOVER(NULL_ENTRY);
  strcpy(s, data);
  strcat(s, suffix);
}

/*
 * maptype:
 * Discover whether the coordinate data is spherical
 * or planar and return as type.
 * type=-1 on error.
 */
void maptype(database, type)
char **database;
int *type;
{
  char Lname[512];
  int Coordtype;
  FILE *lf;

  name(Lname, *database, ".L");
  if((lf = fopen(Lname, "rb")) == NULL)
    TBAD("Cannot open %s", Lname);
  if(Read(lf, &Coordtype, 1) != 1)
    TBAD("Cannot read coordtype in %s", Lname);
  AdjustBuffer(&Coordtype,1,sizeof(Coordtype));
  if(Coordtype < 0 || Coordtype > 10000) {
    /* the file is probably byte-swapped the wrong way. */
    /* exactly one of these adjustbuffer calls will do something */
    AdjustBuffer(&Coordtype,1,sizeof(Coordtype));
    swap_override = !swap_override;
    AdjustBuffer(&Coordtype,1,sizeof(Coordtype));
  }
  *type = Coordtype;
  fclose(lf);
}

static double maptype_factor(int type)
{
  if((type == SPHERE) || (type == SPHERE0)) return DEG2RAD(1.0);
  else return(1.0);
}

/*
 * mapgetg:
 * Retrieve polygon information from the named database.
 * Which is a vector of nwhich polygon numbers.  The retrieved
 * information is returned in sl.  If retlines is 0, the
 * number of polylines in each polygon is returned in sl.
 * Otherwise, the actual polyline numbers are strung together
 * in sl.  Thus, the usual usage is to call mapgetg once with
 * retlines=0 and sl of length nwhich, and then again with
 * retlines=1 and sl of length the sum of the values returned
 * in sl in the first call.  Range and fill determine which
 * polygons are ignored:
 *	polygons lying entirely outside range when fill is FALSE
 *	[but no longer ...]
 *	polygons lying partially outside range when fill is TRUE
 * If an error is encountered, retlines will be set to -1 on return.
 */
void mapgetg(database, which, nwhich, sl, retlines, range, fill)
char **database;
int *which, *nwhich, *sl, *retlines, *fill;
double *range;
{
	Region region, total;
	char Gname[512];
	int i, maxsize = 0, k;
	int type;
	double factor, xmin, xmax, ymin, ymax;
	Polyline *line = NULL;
	struct region_h rh;
	FILE *rf;

	maptype(database, &type);
	if(type < 0) {
		*retlines = -1;
		return;
	}
	factor = maptype_factor(type);
	xmin = range[XMIN] * factor;
	xmax = range[XMAX] * factor;
	ymin = range[YMIN] * factor;
	ymax = range[YMAX] * factor;
	name(Gname, *database, ".G");
	if((rf = fopen(Gname, "rb")) == NULL)
    RBAD("Cannot open %s", Gname);
	if(Read(rf, &total, 1) != 1)
    RBAD("Cannot read size in %s", Gname);
  /* swap bytes if necessary */
  AdjustBuffer(&total,1,sizeof(total));
	for(i = 0; i < *nwhich; i++, which++) {
		region = *which;
		if(region <= 0 || region > total)
			continue;
		if(Seek(rf, Roffset(region)) == -1)
      RBAD("Cannot seek to header in %s", Gname);
		if(Read(rf, &rh, 1) != 1)
      RBAD("Cannot read header in %s", Gname);
    AdjustRegionH(&rh,1);
		if(!*retlines) {
			*sl = rh.nline;
	/* Don't ignore partial polygon if fill=TRUE
			if(*fill &&
				(rh.sw.x < xmin ||
				 rh.sw.y < ymin ||
				 rh.ne.x > xmax ||
				 rh.ne.y > ymax))
					*which = *sl = 0;
			if(!*fill &&
				(rh.sw.x > xmax ||
	*/
			if(rh.sw.x > xmax ||
				 rh.sw.y > ymax ||
				 rh.ne.x < xmin ||
	/*			 rh.ne.y < ymin))	*/
				 rh.ne.y < ymin)
					*which = *sl = 0;
			sl++;
		} else {
			if(maxsize < (int)rh.nline) {
				if(maxsize == 0)
					Alloc(line, rh.nline, Polyline);
				else
					Realloc(line, rh.nline, Polyline);
				if(line == NULL)
	  RBAD("No memory for polyline numbers", 0);
				maxsize = rh.nline;
			}
			if(Seek(rf, rh.offset) == -1)
	RBAD("Cannot seek to data in %s", Gname);
			if(Read(rf, line, rh.nline) != rh.nline)
	RBAD("Cannot read data in %s", Gname);
      AdjustBuffer(line, rh.nline, sizeof(*line));
			for(k = 0; k < (int)rh.nline; k++)
				*sl++ = line[k];
		}
	}
	if(line)
		free((char *)line);
	fclose(rf);
}


/*
 * mapgetl:
 * Retrieve polyline information from the named database.
 * Which is a vector of nwhich polyline numbers.  Getcoords
 * controls which of two actions mapgetl takes.
 *	getcoords = 0:
 * 		- replace each polyline number with the number
 * 		  of coordinate pairs in that polyline
 * 		- set this number to 0 if the polyline falls
 *		  entirely outside the bounds given in range and
 *		  fill is not TRUE
 * 	getcoords = 1:
 * 		- fill the x and y arrays with the coordinates
 * 		  of the points in the polylines, reversing the
 * 		  order if the polyline number is negative
 * 		- compute the actual ranges of the polylines
 * 		  and return these in range
 * If an error is encountered, nwhich will be set to -1 on return.
 */
void mapgetl(database, which, nwhich, getcoords, x, y, range, fill)
char **database;
int *which, *nwhich, *getcoords, *fill;
double *x, *y, *range;
{
  Polyline line, total;
	char Lname[512];
	int i, Coordtype, k, maxsize = 0, start, end, incr;
	int type;
	double factor, xmin, xmax, ymin, ymax, ox, dx, wind;
	struct line_h lh;
	struct pair *xy = NULL, XY;
	FILE *lf;

	maptype(database, &type);
	if(type < 0) {
		*nwhich = -1;
		return;
	}
	factor = maptype_factor(type);
	xmin = range[XMIN] * factor;
	xmax = range[XMAX] * factor;
	ymin = range[YMIN] * factor;
	ymax = range[YMAX] * factor;
	name(Lname, *database, ".L");
	if((lf = fopen(Lname, "rb")) == NULL)
    LBAD("Cannot open %s", Lname);
	if(Seek(lf, sizeof(Coordtype)) < 0)
    LBAD("Cannot seek in %s", Lname);
	if(Read(lf, &total, 1) != 1)
    LBAD("Cannot read size in %s", Lname);
  AdjustBuffer(&total,1,sizeof(total));
	if(*getcoords) {
		range[XMAX] = range[YMAX] = -1e30;
		range[XMIN] = range[YMIN] = 1e30;
	}
	for(i = 0; i < *nwhich; i++) {
		line = ABS(which[i]);
		if(line <= 0)
      LBAD("Polyline number must be positive", 0);
		if(line > total)
      LBAD("Polyline number must be <= %d", (int)total);
		if(Seek(lf, Loffset(line)) == -1)
      LBAD("Cannot seek to header in %s", Lname);
		if(Read(lf, &lh, 1) != 1)
      LBAD("Cannot read header in %s", Lname);
    AdjustLineH(&lh,1);

		/* range only */
		if(!*getcoords) {
			which[i] = lh.npair;
			/* Only eliminate lines if not filling. */
			if(!*fill &&
			  (lh.sw.x > xmax ||
			   lh.sw.y > ymax ||
			   lh.ne.x < xmin ||
			   lh.ne.y < ymin))
				which[i] = 0;
			continue;
		}

		/* get the coordinate pairs: get room for one polyline */
		if(maxsize < (int)lh.npair) {
			if(maxsize == 0)
				Alloc(xy, lh.npair, struct pair);
			else
				Realloc(xy, lh.npair, struct pair);
			if(xy == NULL)
	LBAD("No memory for coordinate pairs", 0);
			maxsize = lh.npair;
		}
		if(Seek(lf, lh.offset) == -1)
      LBAD("Cannot seek to data in %s", Lname);
		if(Read(lf, xy, lh.npair) != lh.npair)
      LBAD("Cannot read coords in %s", Lname);
    AdjustBuffer(xy,2*lh.npair,sizeof(xy->x));
		wind = ox = 0;
		if(which[i] > 0) {
			start = 0;
			end = lh.npair;
			incr = 1;
		} else {
			start = lh.npair - 1;
			end = -1;
			incr = -1;
		}
		for(k = start; k != end; k += incr) {
			XY.x = xy[k].x / factor;
			XY.y = xy[k].y / factor;
			dx = k != start ? XY.x - ox : 0;
			ox = XY.x;
			if(dx < -100)
				wind += 360;
			else if(dx > 100)
				wind -= 360;
			/*
			 * KLUDGE: we can't pull this trick with
			 * Antarctica, since it has a nonzero
			 * winding number with respect to the pole
			 */
			if(XY.y > -75)
				XY.x += wind;
			*x++ = XY.x;
			*y++ = XY.y;
			setrange(range, XY);
		}
		if(i < *nwhich-1) {
			*x++ = NA_REAL;
			*y++ = NA_REAL;
		}
	}
	if(xy)
		free((char *)xy);
	fclose(lf);
}

/* Point in polygon code ***************************************************/

/*
 * Return in *x and *y pointers to the coordinate pairs composing
 * polygon number poly in the given database, and return the lengths
 * in *n.  
 * Caller must free *x and *y.
 * The last point returned is the same as the first one.
 */
static void
getpoly(char **database, int poly, double **x, double **y, int *n)
{
  int i, j, status, zero = 0, one = 1, nline, npair;
  double range[4];
  static int *lines, *lengths;
  static double *X, *Y;

  /* no restriction on range */
  range[XMIN] = range[YMIN] = -1e30;
  range[XMAX] = range[YMAX] = 1e30;

  /* in nline get number of polylines in this polygon */
  status = 0;
  mapgetg(database, &poly, &one, &nline, &status, range, &one);

  /* success? */
  if(status < 0) error("mapgetg failure from getpoly");

  /* get the polyline numbers in lines */
  lines = Calloc(nline, int);
  status = 1;
  mapgetg(database, &poly, &one, lines, &status, range, &one);

  /* success? */
  if(status < 0)
    error("mapgetg failure from getpoly");

  /* in lengths get the number of pairs in each polyline */
  lengths = Calloc(nline, int);
  for(i = 0; i < nline; i++)
    lengths[i] = lines[i];
  status = nline;
  mapgetl(database, lengths, &status, &zero, 0, 0, range, &one);

  /* success? */
  if(status < 0)
    error("mapgetl failure from getpoly");

  /* allocate space for the actual coordinates */
  npair = nline-1;
  for(i = 0; i < nline; i++)
    npair += lengths[i];
  X = Calloc(npair, double);
  Y = Calloc(npair, double);

  /* get the coordinate pairs */
  status = nline;
  mapgetl(database, lines, &status, &one, X, Y, range, &one);

  /* success? */
  if(status < 0)
    error("mapgetl failure from getpoly");

  /* elide NAs */
  j = 0;
  for(i = 0; i < npair; i++)
    if(ISNA(X[i]))
      i++;
    else {
      X[j] = X[i];
      Y[j] = Y[i];
      j++;
    }

  /* clean up */
  Free(lines);
  Free(lengths);

  /* return the data */
  *x = X;
  *y = Y;
  *n = npair - 2*(nline-1);
}

/*
 * Detailed point-in-polygon.  The candidate point is (x,y) and the X and
 * Y vectors contain the n vertices of the polygon.  Return value is 1 if
 * (x,y) is in the polygon, 0 if it is not and -1 if it is too hard to
 * decide.  Draw a horizontal line through (x,y) and record the
 * x-positions of all the places it cuts the polygon.  If (x,y) is near the
 * cut, return -1.  There should be an even number of cuts; if not,
 * return -1.  Now sort the cuts and count how many are to the left of x.
 * If any cut lies exactly on x, return -1.  Otherwise, (x,y) is in the
 * polygon if and only if the number of cuts to the left is even.
 */
/* The polygon must be closed. */

#define EPS		(10*DBL_EPSILON)
#define CLOSE(a,b)	(b)?(fabs(((a)-(b))/(b))<EPS):(fabs((a)-(b))<EPS)
#define BETWEEN(a,y,b)	(((a)<=(y) && (y)<(b)) || ((b)<=(y) && (y)<(a)))

static int
pip(double x, double y, double *X, double *Y, int n)
{
  int i, nleft = 0, ni = 0;
  double xp;

  for(i = 1; i < n; i++) {
    if(!BETWEEN(Y[i-1], y, Y[i]))
      continue;
    if(Y[i] == Y[i-1]) xp = X[i-1];
    else xp = X[i-1] + (X[i]-X[i-1]) * (y-Y[i-1]) / (Y[i]-Y[i-1]);
    if(CLOSE(xp,x)) return(-1);
    nleft += (xp < x);
    ni++;
  }
  if(ni % 2) return(-1);
  return(nleft % 2);
}

/*
 * point-in-polygon:
 * Given a database and a collection of points, find for each point the
 * number of the database polygon that contains it; the point is first
 * checked against the bounding box of a candidate polygon and if that
 * test succeeds, a full test is made.
 *
 * Output is in poly.
*/
void
map_where(char **database, double *x, double *y, int *n, int *poly)
{
  Region npoly;
  char Gname[512];
  int i, j;
  int nv;
  struct region_h *rh;
  double *X, *Y;
  FILE *rf;

  /* find total # of polygons in database and read headers for bboxes */
  name(Gname, *database, ".G");
  if((rf = fopen(Gname, "rb")) == NULL)
    error("pip: cannot open %s", Gname);
  if(Read(rf, &npoly, 1) != 1) {
    fclose(rf);
    error("pip: cannot read size in %s", Gname);
  }
  AdjustBuffer(&npoly,1,sizeof(npoly));
  rh = Calloc(npoly, struct region_h);
  if(Read(rf, rh, npoly) != npoly) {
    fclose(rf);
    error("pip: cannot read headers in %s", Gname);
  }
  AdjustRegionH(rh,npoly);
  fclose(rf);

  /* check bounding box and possibly full polygon for each input point */
  memset(poly,0,sizeof(int)*(*n));
  for(j = 0; j < npoly; j++) {
    getpoly(database, j+1, &X, &Y, &nv);
    for(i = 0; i < *n; i++) {
      if(poly[i]) continue;
      if(rh[j].sw.x > DEG2RAD(x[i]) || rh[j].sw.y > DEG2RAD(y[i]) ||
	 rh[j].ne.x < DEG2RAD(x[i]) || rh[j].ne.y < DEG2RAD(y[i]))
	continue;
      if(pip(x[i], y[i], X, Y, nv) == 1)
	poly[i] = j+1;
    }
    Free(X);
    Free(Y);
  }
}

/* result is assumed initialized to zero */
void
map_in_one_polygon(double *px, double *py, int *np,
		   double *x, double *y, int *n, int *result, int *hit)
{
  int i;
  /* compute limits of polygon */
  double xlow=px[0], xhigh=px[0], ylow=py[0], yhigh=py[0];
  for(i = 0; i < *np; i++) {
    if(px[i] < xlow) xlow = px[i];
    else if(px[i] > xhigh) xhigh = px[i];
    if(py[i] < ylow) ylow = py[i];
    else if(py[i] > yhigh) yhigh = py[i];
  }
  /* loop points */
  for(i = 0; i < *n; i++) {
    if(x[i] < xlow || x[i] > xhigh ||
       y[i] < ylow || y[i] > yhigh)
      continue;
    if(pip(x[i], y[i], px, py, *np) == 1) result[i] = *hit;
  }
}

/* result[i] is the index of the polygon containing (x[i],y[i]), 
 * indexing from 1, or unchanged if not in any. 
 */
void
map_in_polygon(double *px, double *py, int *np,
	       double *x, double *y, int *n, int *result)
{
  int i,start=0,this_np,index=1;
  for(i = 1; i < *np; i++) {
    if(ISNA(px[i])) {
      this_np = i-start;
      map_in_one_polygon(px+start,py+start,&this_np,x,y,n,result,&index);
      start = i+1;
      index++;
    }
  }
  this_np = i-start;
  map_in_one_polygon(px+start,py+start,&this_np,x,y,n,result,&index);
}

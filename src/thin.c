#include "R.h"
#include "map.h"

#define INIT(a)		a.begin = -PI; a.end = PI
#define INSTALL_UP(j)	x[upfence]=x[j]; y[upfence++]=y[j]; INIT(wedge)
#define INSTALL_DOWN(j)	x[--downfence]=x[j]; y[downfence]=y[j]; INIT(wedge)
#define Max(a,b)	((a)>(b)?(a):(b))
#define Min(a,b)	((a)<(b)?(a):(b))

/*
 * An arc structure has a beginning angle and
 * an ending angle.  It should be in a canonical
 * form such that the beginning angle is
 * in [-PI,PI).  This implies that the end
 * angle is always greater than -PI, a fact
 * which is assumed in the routines below which
 * manipulate arcs.
 */
struct arc {
	double begin;
	double end;
};

/*
 * Test whether theta lies in the arc a.
 * Theta is expected to be in [-PI,PI).
 */
static int inarc(a, theta)
     struct arc a;
     double theta;
{
  if(a.begin <= theta && theta <= a.end)
    return(1);
  theta += PI2;
  if(a.begin <= theta && theta <= a.end)
    return(1);
  return(0);
}

/*
 * Intersect the arc a with the arc defined by begin
 * and end.  The arc is assumed to be in canonical form
 * but the arc defined by (begin,end) can be located
 * anywhere.
 */
static void intersect(a, begin, end)
     struct arc *a;
     double begin, end;
{
  /* move (begin,end) until it has a chance to intersect a */
  while(end < a->begin) {
    begin += PI2;
    end += PI2;
  }
  while(begin > a->end) {
    begin -= PI2;
    end -= PI2;
  }	

  /* if no intersection ... */
  if(end < a->begin || begin > a->end)
    a->begin = a->end = 0;

	/* do the intersection ... */
  else {
    a->begin = Max(begin, a->begin);
    a->end = Min(end, a->end);
  }

  /* canonicalize the intersection if necessary */
  while(a->begin >= PI) {
    a->begin -= PI;
    a->end -= PI;
  }
}

/*
 * Given an ordered list of n points in the plane, and
 * a tolerance delta, find a subset of the points with
 * the same ordering such that the linear spline passing
 * through the new points passes within delta of each of
 * the original points.
 */
static
int thin(x, y, n, delta, symmetric)
     double x[], y[], delta;
     int n, symmetric;
{
	int cur, next, mid, uptotal, downtotal, upfence, downfence, m;
	double dist, theta, alpha, dx, dy;
	struct arc wedge;

	if((symmetric && n <= 4) || (!symmetric && n <= 2) || delta <= 0)
		return(n);

	/* non-symmetric thin goes from one end to the other */
	if(!symmetric) {
		upfence = 0;
		INSTALL_UP(0);
		for(cur = 0, next = 1; next < n; next++) {
			dx = x[next] - x[cur];
			dy = y[next] - y[cur];
			if(dx == 0 && dy == 0)
				continue;
			theta = atan2(dy, dx);
			if(!inarc(wedge, theta)) {
				cur = --next;
				INSTALL_UP(cur);
				continue;
			}
			dist = hypot(dx, dy);
			if(dist > delta) {
				alpha = asin(delta / dist);
				intersect(&wedge, theta - alpha, theta + alpha);
			}
		}
		INSTALL_UP(n-1);
		return(upfence);
	}

	/* symmetric version: first thin from the beginning to the middle */
	upfence = 0; mid = (n + 1) / 2;
	INSTALL_UP(0);
	for(cur = 0, next = 1; next < mid; next++) {
		dx = x[next] - x[cur];
		dy = y[next] - y[cur];
		if(dx == 0 && dy == 0)
			continue;
		theta = atan2(dy, dx);
		if(!inarc(wedge, theta)) {
			cur = --next;
			INSTALL_UP(cur);
			continue;
		}
		dist = hypot(dx, dy);
		if(dist > delta) {
			alpha = asin(delta / dist);
			intersect(&wedge, theta - alpha, theta + alpha);
		}
	}
	INSTALL_UP(mid-1);
	uptotal = upfence;

	/* now work from the end to the middle */
	downfence = n; mid = n - mid;
	INSTALL_DOWN(n-1);
	for(cur = n-1, next = n-2; next >= mid; next--) {
		dx = x[next] - x[cur];
		dy = y[next] - y[cur];
		if(dx == 0 && dy == 0)
			continue;
		theta = atan2(dy, dx);
		if(!inarc(wedge, theta)) {
			cur = ++next;
			INSTALL_DOWN(cur);
			continue;
		}
		dist = hypot(dx, dy);
		if(dist > delta) {
			alpha = asin(delta / dist);
			intersect(&wedge, theta - alpha, theta + alpha);
		}
	}
	INSTALL_DOWN(mid);
	downtotal = n - downfence;

	/* for n odd, middle point is recorded twice, so fix that */
	if(n % 2) {
		downfence++;
		downtotal--;
	}

	/* now copy back upper half so that arrays are contiguous */
	for(m = 0; m < downtotal; m++) {
		x[upfence] = x[downfence];
		y[upfence++] = y[downfence++];
	}

	return(uptotal + downtotal);
}

void mapthin(x, y, n, delta, symmetric)
     double *x, *y, *delta;
     int *n, *symmetric;
{
	int start, end, m, from, to, wasna, isna, i;

	end = 0;
	while(end < *n) {
		start = end;
		while(end < *n && !ISNA(x[end]))
			end++;
		m = thin(x+start, y+start, end-start, *delta, (int)*symmetric);
		for(i = start+m; i < end; i++) {
			x[i] = NA_REAL;
			y[i] = NA_REAL;
		}
		while(end < *n && ISNA(x[end]))
			end++;
	}
	wasna = 0;
	for(from = to = 0; from < *n; from++) {
		isna = ISNA(x[from]);
		if(!isna) {
			x[to] = x[from];
			y[to] = y[from];
			to++;
		} else if(!wasna) {
			x[to] = NA_REAL;
			y[to] = NA_REAL;
			to++;
		}
		wasna = isna;
	}
	*n = wasna ? to-1 : to;
}

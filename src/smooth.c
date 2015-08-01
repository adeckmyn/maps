#include "R.h"
#include "map.h"
#include <math.h>
void
kernel_smooth(int *n, int *d, double *x, double *zr,
	      int *region, 
	      int *no, double *xo, double *zo,
	      double *lambda, int *normalize) 
{
  int i,j;
  /* region[i] is in 1..nr */
  /* zr[r] must be pre-divided by region_size[r] */
  /* loop output points */
  for(i=0;i<*no;i++) {
    double kern_sum = 0;
    double z = 0;
    double *xp = x;
    /* this could run faster with blocking */
    for(j=0;j<*n;j++) {
      int r = region[j]-1;
      int dim;
      double dist = 0;
      double kern;
      for(dim=0;dim<*d;dim++) {
	double dx = *xp++ - xo[dim];
	dist += dx*dx;
      }
      if(*lambda == 0) {
	double rd = sqrt(dist);
	kern = 1 + dist + rd*rd*rd;
      } else {
	kern = exp(-(*lambda)*dist);
      }
      z += zr[r]*kern;
      kern_sum += kern;
    }
    if(*normalize) zo[i] = z/kern_sum;
    else zo[i] = z;
    xo += *d;
  }
}
void
kernel_region_region(int *n, int *d, double *x, int *region, 
		     double *lambda, int *nr, double *krr) 
{
  int i,j;
  double *xp1 = x;
  /* krr must be zero on entry */
  /* loop all pairs of points */
  for(i=0;i<*n;i++) {
    int r1 = region[i]-1;
    double *xp2 = x;
    for(j=0;j<*n;j++) {
      int r2 = region[j]-1;
      int dim;
      double dist = 0;
      double kern;
      for(dim=0;dim<*d;dim++) {
	double dx = *xp2++ - xp1[dim];
	dist += dx*dx;
      }
      if(*lambda == 0) {
	/* this is not posdef */
	double rd = sqrt(dist);
	kern = 1 + dist + rd*rd*rd;
      } else {
	kern = exp(-(*lambda)*dist);
      }
      krr[r1 + r2*(*nr)] += kern;
    }
    xp1 += *d;
  }
}
void
kernel_region_x(int *n, int *d, double *x, int *region,
		int *no, double *xo, double *lambda, int *nr, double *krx)
{
  int i,j;
  double *xp = x;
  /* krx must be zero on entry */
  /* loop all pairs of points */
  for(i=0;i<*n;i++) {
    int r = region[i]-1;
    double *xop = xo;
    for(j=0;j<*no;j++) {
      int dim;
      double dist = 0;
      double kern;
      for(dim=0;dim<*d;dim++) {
	double dx = *xop++ - xp[dim];
	dist += dx*dx;
      }
      if(*lambda == 0) {
	double rd = sqrt(dist);
	kern = 1 + dist + rd*rd*rd;
      } else {
	kern = exp(-(*lambda)*dist);
      }
      krx[r + j*(*nr)] += kern;
    }
    xp += *d;
  }
}

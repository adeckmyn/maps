#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include "map.h"

#define Seek(f,n)	fseek(f, (int)(n), 0)
#define Read(f,s,n)	fread((char *)(s), sizeof(*(s)), (int)(n), f)
#define Write(f,s,n)	fwrite((char *)(s), sizeof(*(s)), (int)(n), f)
#define Alloc(s,n,t)	s = (t *)calloc((unsigned)(n), sizeof(t))
#define Max(a,b)	((a) > (b) ? (a) : (b))
#define R2FMT		"%hd%hd"	/* format for reading two Region's */

#define WORDSIZE 100

char Usage[] = "Usage: %s precision {spherical|planar} {ascii|binary} in-file in-file-stats-file out-file";
int Precision, Coordtype;
char *Me, *getword(), *Infile;
Polyline n;
int nl, maxp;

extern int
isspace(int c);

int
fatal(s, a, b)
char *s, *a;
int b;
{
	fprintf(stderr, s, a, b);
	fprintf(stderr, "\n");
	exit(1);
}

int
fatal2(s, a, b)
char *s;
int a, b;
{
	fprintf(stderr, s, a, b);
	fprintf(stderr, "\n");
	exit(1);
}

/*
 * Read one pair of coordinates.  The return value should be
 * 1 if a pair was read, 0 if the end-of-record indicator was
 * read and -1 if there was a read fatal.
 */
int
getpair(f, xy)
FILE *f;
struct pair *xy;
{
	char *w;

	if((w = getword(f)) == 0)
		return(-1);
	if(strcmp(w, EOR) == 0)
		return(0);
	xy->x = atof(w);

	if((w = getword(f)) == 0)
		return(-1);
	if(strcmp(w, EOR) == 0)
		return(0);
	xy->y = atof(w);

	return(1);
}

char *
getword(f)
FILE *f;
{
	static char word[WORDSIZE];
	char *s = word;
	int c;

	do
		if((c = fgetc(f)) < 0)
			return(0);

	while(isspace(c));
	do {
		if(s - word >= WORDSIZE-1)
			return(0);
		*s++ = c;
		c = fgetc(f);
	} while(c >= 0 && !isspace(c));
	*s++ = 0;
	return(word);
}

void
set_range(plh, xy)
struct line_h *plh;
struct pair *xy;
{
	int n = plh->npair;
	float xmin = FLT_MAX, ymin = FLT_MAX;
	float xmax = -FLT_MAX, ymax = -FLT_MAX;

	while(n--) {
		xmin = MIN(xmin, xy->x);
		xmax = MAX(xmax, xy->x);
		ymin = MIN(ymin, xy->y);
		ymax = MAX(ymax, xy->y);
		xy++;
	}
	plh->sw.x = xmin;
	plh->sw.y = ymin;
	plh->ne.x = xmax;
	plh->ne.y = ymax;
/*	printf("%f12 %f12 %f12 %f12\n", xmin, ymin, xmax, ymax); */
}

void
to_ascii(in, out)
FILE *in, *out;
{
	Polyline n, i;
	Pair m, j, N = 0;
	struct line_h *lh;
	struct pair *xy;
	char buf[128];
	int column, k;

	if(Seek(in, sizeof(Coordtype)))
		fatal2("Cannot seek past coordtype", 0, 0);
	if(Read(in, &n, 1) != 1)
		fatal2("Cannot read size", 0, 0);
	Alloc(lh, n, struct line_h);
	if(lh == NULL)
		fatal2("No memory for headers", 0, 0);
	if(Read(in, lh, n) != n)
		fatal2("Cannot read headers", 0, 0);
	for(i = 0; i < n; i++)
		N = Max(N, lh[i].npair);
	Alloc(xy, N, struct pair);
	if(xy == NULL)
		fatal2("No memory for data", 0, 0);
	for(i = 0; i < n; i++) {
		if((m = lh[i].npair) <= 0)
			fatal2("Negative pair count at header %d", (int)i,
			0);
		if(Seek(in, lh[i].offset) < 0)
			fatal2("Cannot seek to record %d", (int)i, 0);
		/*
		 * Simple read; change this to use other means of
		 * storing the polyline data.
		 */
		if(Read(in, xy, m) != m)
			fatal2("Cannot read record %d", (int)i, 0);
		fprintf(out, "%d %d\n", (int)lh[i].left, (int)lh[i].right);
		column = 0;
		for(j = 0; j < m; j++) {
			sprintf(buf, " %.*f %.*f", Precision, xy[j].x, Precision, xy[j].y);
			k = strlen(buf);
			if(column + k >= 80) {
				fputc('\n', out);
				column = 0;
			}
			fprintf(out, "%s", buf);
			column += k;
		}
		fprintf(out, "\n%s\n", EOR);
	}
}

void
to_binary(in, out)
FILE *in, *out;
{
	Polyline i;
	Pair m; 
	int t;
	int l, r;
	struct line_h *lh;
	struct pair *xy;

	if(Seek(out, sizeof(Coordtype) + sizeof(Polyline) + nl*sizeof(struct line_h)) < 0)
		fatal2("Cannot seek in input file", 0, 0);
	Alloc(lh, nl, struct line_h);
	Alloc(xy, maxp+1, struct pair);
	if(lh == NULL || xy == NULL)
		fatal2("No memory", 0, 0);
	for(i = 0; i < nl; i++) {
		if(fscanf(in, "%d%d", &l, &r) != 2)
			fatal2("Cannot read left and right at line %d", (int)i+1,
			0);
		lh[i].left = l;
		lh[i].right = r;
		m = 0;
		while((t = getpair(in, &xy[m])) > 0) {
			if(m == 0 ||
			   xy[m].x != xy[m-1].x ||
			   xy[m].y != xy[m-1].y)
				m++;
		}
		if(t < 0)
			fatal2("Read, line=%d word=%d", (int)i+1, (int)2+m*2);
		lh[i].offset = ftell(out);
		lh[i].npair = m;
		set_range(lh+i, xy);
		/*
		 * Simple write; change this to use other means of
		 * storing the polyline data.
		 */
		if(Write(out, xy, m) != m)
			fatal2("Cannot write record %d", (int)i, 0);
	}
	if(Seek(out, 0) < 0)
		fatal2("Cannot seek to beginning of output file", 0, 0);
	if(Write(out, &Coordtype, 1) != 1)
		fatal2("Cannot write coordtype to output file", 0, 0);
	if(Write(out, &n, 1) != 1)
		fatal2("Cannot write size to output file", 0, 0);
	if(Write(out, lh, nl) != nl)
		fatal2("Cannot write headers to output file", 0, 0);
}

int
main(ac, av)
int ac;
char *av[];
{

	FILE *in, *in2, *out;

	Me = av[0];
	if(ac != 7)
		fatal(Usage, Me, 0);
	Precision = atoi(av[1]);
	Coordtype = av[2][0] == 's' ? SPHERE : PLANE;
	Infile = av[4];
	if((in = fopen(av[4], "rb")) == NULL)
                fatal("Cannot open %s for reading", av[4], 0);
	if((in2 = fopen(av[5], "rb")) == NULL)
                fatal("Cannot open %s for reading", av[5], 0);
	if(fscanf(in2, "%d%d", &nl, &maxp) != 2)
		fatal("Cannot read stats data file %s", av[3], 0);
	n = nl;
        if((out = fopen(av[6], "wb")) == NULL)
                fatal("Cannot open %s for writing", av[6], 0);
        av[3][0] == 'a' ? (void) to_ascii(in, out) : (void) to_binary(in, out);
	exit(0);
}

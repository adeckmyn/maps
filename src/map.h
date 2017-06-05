#ifndef PI
#define PI		3.1415926535897932384626433832795028841971693993751
#endif
#define PI2		(2*PI)

#define SPHERE0         0       /* backwards compatibility */
#define SPHERE		2	/* line data is on the sphere */
#define PLANE		1	/* line data is on the plane */
#define EOR		"EOR"	/* end of record indicator */
#define MIN(a,b)	(a)<(b)?(a):(b)
#define MAX(a,b)	(a)>(b)?(a):(b)
#define ABS(x)		((x)<0?-(x):(x))
#define RAD2DEG(x)	((x)*180/PI)
#define DEG2RAD(x)	((x)*PI/180)
#define XMIN		0
#define XMAX		1
#define YMIN		2
#define YMAX		3

typedef unsigned Offset;	/* offset in a disk file */
typedef unsigned short Pair;	/* points in a polyline */
typedef unsigned char Line;	/* polylines in a region */
typedef unsigned short Region;	/* all regions */
typedef int Polyline;		/* all polylines (signed) */

/* stucture for hash tables */
typedef struct s_x_h {
	char *name;
	int index;
	struct s_x_h *next;
} x_h;

struct pair {
	float x;
	float y;
};

struct line_h {
	Offset offset;
	Pair npair;
	Region left, right;
	struct pair sw, ne;
};

struct region_h {
	Offset offset;
	Line nline;
	struct pair sw, ne;
};

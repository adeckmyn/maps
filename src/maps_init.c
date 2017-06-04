#include <stdlib.h>
#include <R_ext/Rdynload.h>

/* .C calls */
extern void char_to_ascii(void *, void *, void *);
extern void kernel_region_region(void *, void *, void *, void *, void *, void *, void *);
extern void kernel_region_x(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void kernel_smooth(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void map_clip_poly(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void map_getg(void *, void *, void *, void *, void *, void *, void *);
extern void map_getl(void *, void *, void *, void *, void *, void *, void *, void *);
extern void map_in_one_polygon(void *, void *, void *, void *, void *, void *, void *, void *);
extern void map_in_polygon(void *, void *, void *, void *, void *, void *, void *);
extern void map_match(void *, void *, void *, void *, void *, void *);
extern void map_thin(void *, void *, void *, void *, void *);
extern void map_type(void *, void *);
extern void map_where(void *, void *, void *, void *, void *);
extern void map_wrap_poly(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"char_to_ascii",        (DL_FUNC) &char_to_ascii,         3},
    {"kernel_region_region", (DL_FUNC) &kernel_region_region,  7},
    {"kernel_region_x",      (DL_FUNC) &kernel_region_x,       9},
    {"kernel_smooth",        (DL_FUNC) &kernel_smooth,        10},
    {"map_clip_poly",        (DL_FUNC) &map_clip_poly,        10},
    {"map_getg",              (DL_FUNC) &map_getg,             7},
    {"map_getl",              (DL_FUNC) &map_getl,             8},
    {"map_in_one_polygon",   (DL_FUNC) &map_in_one_polygon,    8},
    {"map_in_polygon",       (DL_FUNC) &map_in_polygon,        7},
    {"map_match",            (DL_FUNC) &map_match,             6},
    {"map_thin",              (DL_FUNC) &map_thin,             5},
    {"map_type",              (DL_FUNC) &map_type,             2},
    {"map_where",            (DL_FUNC) &map_where,             5},
    {"map_wrap_poly",        (DL_FUNC) &map_wrap_poly,        11},
    {NULL, NULL, 0}
};

void R_init_maps(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


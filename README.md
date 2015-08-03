# maps
###R package for geographical maps

This is a beta release of the forthcoming updated maps package. It is meant for testing prior to CRAN upload.

##CHANGES:

- The 'world' data base has been replaced by a much more recent data base, imported from the Natural Earth data project (the 1:50m world map, version 3.1).
This may have several implications for code that calls map().
  * Country names have changed. This is inevitable, since e.g. post-1990 Europe is dramatically different. As a consequence, any call to map() that involves the argument "region=...", may be affected. For instance, map('world','USSR') is now obsolete, while map('world','Russia') works fine.
  * The new data base contains more small islands.
  * Some remote islands that are officially part of other countries may cause an unexpected change in the scale of the map. This is not a new phenomenon (map('world','france') had this in the old set too), but some new cases may occur now. 
  * The new 'world' has a somewhat higher resolution than before, and looks much smoother. In fact, for some applications it may now suffice rather than needing to import mapHires.

- map.text(...,exact=TRUE) now behaves as documented. Previously, the "exact=TRUE" was not passed to the map drawing if add=FALSE. To get the old behaviour (plot map with exact=FALSE, write text for exact=TRUE) you should now use 2 commands: 
map(...,exact=F)
map.text(...,exact=T,add=T)

- a simple function iso3166() is added, which returns the country names for a list of two letter codes, and its inverse. In the future, I'll try to find a clean way to allow these ISO codes as map labels.

##FALL BACK TO LEGACY WORLD DATABASE
There are a few mechanisms to use the old 'world' database rather than the updated one, should that be necessary:
- Using 'database=legacy_world'
- Setting R_MAP_DATA_LEGACY=TRUE in the environment prior to loading maps
- Calling world.legacy(TRUE) for switching to the old database and world.legacy(FALSE) to switch back on the fly.

The last two options should only be used as a temporary last resource, to run code that requires the old database without having to edit it.

##TO DO/DISCUSS:

- Many islands remain nameless.  
- Some inconsistencies in the naming procedure remain. For instance, while most countries are named by their full name, "UK" and "USA" are shortened in the same way as in the old data base.
- I'd like to find a way to get Antarctica show up a bit nicer, but whithout adding imaginary points that would ruin any projection.


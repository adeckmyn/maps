# maps
###R package for geographical maps

This is the a beta release of the forthcoming updated maps package.

##CHANGES:

- The 'world' data base has been replaced by a much more recent data base, imported from the Natural Earth data project (the 1:50m world map, version 3.1).

This may have several implications for code that call map().

- Country names have changed. This is inevitable, since e.g. post-1990 Europe is dramatically different. As a consequence, any call to map() that involves region=..., may be affected. For instance, map('world','USSR') is now obsolete, while map('world','Russia') works fine.

- The new 'world' has a somewhat higher resolution than before. In fact, for some applications it may now suffice rather than needing to import mapHires.

##ADDITIONS:

- a simple function iso3166() is added, which returns the country names for a list of two letter codes.


##TO DO/DISCUSS:

- Many islands remain nameless.  
- Some inconsistencies in the naming procedure remain. For instance, while most countries are named by their full name, "UK" and "USA" are shortened in the same way as in the old data base.
- I'd like to find a way to get Antarctica show up a bit nicer, but whithout adding imaginary points that would ruin any projection.


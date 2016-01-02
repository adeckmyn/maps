\name{NEWS}
\title{News for package 'maps'}
\section{Changes in maps version 3.0.2 (2016-01-??)}{
  \itemize{
    \item World database rebuilt using mapMaker. Some corrections to polylines.
    \item Added the lakes on country boundaries as new polygons. That way, the dissappear when plotting with map(..., fill=TRUE) and map(..., interior=FALSE).
    \item Minor changes to man pages.
  }
}


\section{Changes in maps version 3.0.1 (2015-11-30)}{
  \itemize{
    \item Fix match.map() for world map
    \item Add some more island names to the world database
    \item Updated a few web references in man pages.
  }
}

\section{Changes in maps version 3.0.0 (2015-09-24)}{
  \itemize{
    \item MAJOR:
    \itemize{
      \item New 'world' database, taken from the public domain "Natural Earth" project. Many new countries have appeared since 1990. E.g. \code{map(region="USSR")} will no longer work. 
      \item The old map is still available as 'legacy_world'. It is also possible (during a transition period) to switch completely to the old database by giving the command \code{world.legacy(TRUE)} or even by setting the environment variable 'R_MAP_DATA_LEGACY=TRUE' prior to loading the package.
    }
    \item ADDITIONS:
    \itemize{
      \item data set \code{iso3166} containing ISO-3166 country codes.
      \item New functions \code{iso.expand()}, \code{sov.expand()} and \code{iso.alpha()} that use this table for converting country names to ISO codes or back. Also expansion by sovereignty. Could be used e.g. for country labels.
    }
    \item SMALL CHANGES & FIXES
    \itemize{
      \item Internally now uses perl-style regular expressions. Useful for e.g. 'negative' regular expressions: 
      \itemize{
         \item \code{"Norway(?!:Svalbard)"} means Norway and all islands except Svalbard.
         \item \code{map(reg="(?!Belgium)",fill=TRUE,xlim=c(-10,20),ylim=c(40,60))}
      }
      \item map.text(...,exact=TRUE) now behaves as documented. To get the old (non-documented) behaviour (plot map with exact=FALSE, write text for exact=TRUE) you could now use 2 commands: 
      \itemize{
        \item \code{map(...,exact=FALSE)}
        \item \code{map.text(...,exact=TRUE,add=TRUE)}
      }
      \item match.map had a potential bug with locale-dependent behaviour of \code{order()}.
    }
  }
}


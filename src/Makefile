#-*- Makefile -*-
.SUFFIXES: .G .L .N .gon .line .name

PKGNAME = maps
CP = cp
ECHO = echo
RM = rm -f
MKDIR = mkdir

OBJS = mapget.o		\
	smooth.o	\
	thin.o

GDATA = county.G state.G usa.G nz.G world.G world2.G italy.G france.G state.vbm.G state.carto.G
LDATA = county.L state.L usa.L nz.L world.L world2.L italy.L france.L state.vbm.L state.carto.L
NDATA = county.N state.N usa.N nz.N world.N world2.N italy.N france.N state.vbm.N state.carto.N

.line.L:
	@$(MKDIR) -p ../inst/mapdata
	./Lmake 0 s b ${*}.line ${*}.linestats ../inst/mapdata/${*}.L

.gon.G:
	./Gmake b ${*}.gon ${*}.gonstats ../inst/mapdata/${*}.G ../inst/mapdata/${*}.L

.name.N:
	@$(MKDIR) -p ../inst/mapdata
	@$(CP) ${*}.name ../inst/mapdata/${*}.N

.PHONY: all gdata ldata ndata

all: gdata ndata $(PKGNAME)$(SHLIB_EXT) legacy

state.carto.L: state.carto.line state.carto.linestats Lmake
	@$(MKDIR) -p ../inst/mapdata
	./Lmake 0 p b state.carto.line state.carto.linestats ../inst/mapdata/state.carto.L

state.vbm.L: state.vbm.line state.vbm.linestats Lmake
	@$(MKDIR) -p ../inst/mapdata
	./Lmake 0 p b state.vbm.line state.vbm.linestats ../inst/mapdata/state.vbm.L

gdata: Gmake ldata
	$(MAKE) $(GDATA)

ldata: Lmake world2.line
	$(MAKE) $(LDATA)

ndata: world2.line
	$(MAKE) $(NDATA)

$(PKGNAME)$(SHLIB_EXT): $(OBJS)
	$(R_HOME)/bin/R CMD SHLIB -o $(PKGNAME)$(SHLIB_EXT) $(OBJS)

world2.line: world.line
	@$(ECHO) "Converting world to world2"
	$(AWK) -f convert.awk < world.line > world2.line
	@$(CP) world.linestats world2.linestats
	@$(CP) world.gon world2.gon
	@$(CP) world.gonstats world2.gonstats
	@$(CP) world.name world2.name

# temporarily, we include the old (1990) world map 
legacy: legacy_world.line Lmake Gmake
	@$(ECHO) "Creating legacy world databases"
	$(AWK) -f convert.awk < legacy_world.line > legacy_world2.line
	./Lmake 0 s b legacy_world.line legacy_world.linestats ../inst/mapdata/legacy_world.L
	./Lmake 0 s b legacy_world2.line legacy_world.linestats ../inst/mapdata/legacy_world2.L
	./Gmake b legacy_world.gon legacy_world.gonstats ../inst/mapdata/legacy_world.G ../inst/mapdata/legacy_world.L
	./Gmake b legacy_world.gon legacy_world.gonstats ../inst/mapdata/legacy_world2.G ../inst/mapdata/legacy_world2.L
	@$(CP) legacy_world.name ../inst/mapdata/legacy_world.N
	@$(CP) legacy_world.name ../inst/mapdata/legacy_world2.N
	$(RM)  legacy_world2.line 


clean:
	$(RM) $(OBJS) Gmake Lmake world2.* $(PKGNAME).s[lo] $(PKGNAME).dylib *.exe maps.dll symbols.rds
	$(RM) -r ../inst/mapdata ../libs

EXTENSION  = f
COMPILER   = f77
OPTIONS    = -C -g -i4 -u
OBJECTS    = adj_fnh.o \
             cgm_util.o \
             environ.o \
             f2ursi.o \
             gen.o \
             getdat.o \
             hlim.o \
             init.o \
             io_util.o \
             looklib.o \
             low_para.o \
             math_uti.o \
             mid_para.o \
             output.o \
             param.o \
             pim.o \
             precip.o \
             read_dba.o \
             strnglib.o \
             teccalc.o \
             timelib.o \
             usumodel.o
INCLUDES   = aindex.inc \
             cgdb.inc \
             dpath.inc \
             f6parm.inc \
             grid.inc \
             gwc1.inc \
             indirect.inc \
             io.inc \
             logicuni.inc \
             lower.inc \
             low_e.inc \
             math4_co.inc \
             midlat.inc \
             precip.inc \
             region_b.inc \
             ssnstuff.inc \
             usuarr.inc
pim        : $(OBJECTS)
	$(COMPILER) $(OPTIONS) $(OBJECTS) -o pim
$(OBJECTS) : $(INCLUDES)
	$(COMPILER) $(OPTIONS) -c $*.$(EXTENSION)

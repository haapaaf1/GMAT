# $Id: Makefile 9490 2011-04-27 16:28:40Z tdnguye2 $
# 
# GMAT: General Mission Analysis Tool.
# 
#  Copyright (c) 2002-2011 United States Government as represented by the
#  Administrator of The National Aeronautics and Space Administration.
#  All Other Rights Reserved.
#
# makefile for GMAT's MarsGRAM2005 Plugin
# This is the base level makefile 
#
# This file builds the plug-in base code

include ../MarsGRAMEnv.mk


TARGET = ../../lib/libMarsGRAM$(SHARED_EXTENSION)


all: prep $(TARGET)

prep:
	rm -rf $(TARGET)

HEADERS = -Iinclude \
          -Iplugin \
          -Ifactory \
          -Iatmosphere \
          -I$(GMAT_CODE_LOCATION)/base/include \
          -I$(GMAT_CODE_LOCATION)/base/attitude \
          -I$(GMAT_CODE_LOCATION)/base/coordsystem \
          -I$(GMAT_CODE_LOCATION)/base/util \
          -I$(GMAT_CODE_LOCATION)/base/solarsys \
          -I$(GMAT_CODE_LOCATION)/base/hardware \
          -I$(GMAT_CODE_LOCATION)/base/spacecraft \
          -I$(GMAT_CODE_LOCATION)/base/foundation \
          -I$(GMAT_CODE_LOCATION)/base/factory \
          -I$(GMAT_CODE_LOCATION)/base/function \
          -I$(GMAT_CODE_LOCATION)/base/parameter \
          -I$(GMAT_CODE_LOCATION)/base/solver \
          -I$(GMAT_CODE_LOCATION)/base/subscriber \
          -I$(GMAT_CODE_LOCATION)/base/interface \
          -I$(GMAT_CODE_LOCATION)/base/command \
          -I$(GMAT_CODE_LOCATION)/base/executive \
          -I$(GMAT_CODE_LOCATION)/base/interpreter \
          -I$(GMAT_CODE_LOCATION)/base/forcemodel \
          -I$(GMAT_CODE_LOCATION)/gui/include \
          -I$(GMAT_CODE_LOCATION)/gui/app \
          -I$(MATLAB_IF_CODE_LOCATION)/base/include \
          -I$(MATLAB_IF_CODE_LOCATION)/base/interface


clean : archclean

archclean :
	rm -rf */*.o
	rm -rf $(TARGET)
	rm -rf */*~
	rm -rf core
	rm -rf */core

CPP_OBJECTS = \
    plugin/GmatPluginFunctions.o \
    factory/MarsGRAMFactory.o \
    atmosphere/MarsGRAM2005.o

C_OBJECTS = \
    atmosphere/julday_conversion.o \
	atmosphere/readalb.o \
    atmosphere/readtopo.o \
    atmosphere/makebin.o \
    atmosphere/setup_MGRAM05.o \
    atmosphere/marssubs_M05.o \
    atmosphere/TESsubs_M05.o \
    atmosphere/mgram_M05.o


ifeq ($(MAC_SPECIFIC), 1)
$(TARGET) : $(C_OBJECTS) $(CPP_OBJECTS)
	mkdir -p ../../lib
	$(CPP) $(C_OBJECTS) $(CPP_OBJECTS) -o $(TARGET) $(SHARED_LIB_FLAGS)
	install_name_tool -change lib/lib$(BASE_LIBRARY).dylib $(BASE_LIB_LOCATION)lib$(BASE_LIBRARY).dylib $(TARGET)
	cp $(TARGET) $(GMAT_Bin_Location)
else
$(TARGET) : $(C_OBJECTS) $(CPP_OBJECTS)
	mkdir -p ../../lib
	$(CPP) $(C_OBJECTS) $(CPP_OBJECTS) -o $(TARGET) $(SHARED_LIB_FLAGS)
	cp $(TARGET) $(GMAT_BIN_LOCATION)
endif


$(CPP_OBJECTS): %.o: %.cpp %.hpp
	$(CPP) $(CPPFLAGS) $(HEADERS) -c -o $@ $<
    
$(C_OBJECTS): %.o: %.c %.h
	$(C) $(CFLAGS) -c -o $@ $<
    
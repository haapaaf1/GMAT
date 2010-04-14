# $Id$
# Environment settings for the MatlabInterface plugin
# location of GMAT base headers and libraries
# Mac OS X (Leopard) version 2010.04.13 - Wendy Shoan
GMAT_CODE_LOCATION = ../../../../src
GMAT_BIN_LOCATION = ../../../../bin

WX_INSTALLED = /usr/local/bin

# location of MATLAB headers and libraries
# **** edit this line to point to your MATLAB (2009) installation ****
MATLAB_DIR = /Applications/MATLAB_R2009a/MATLAB_R2009a.app

MATLAB_CPP_FLAGS = -D__USE_MATLAB__=1 -I$(MATLAB_DIR)/extern/include -Igmat_mex/src
MATLAB_LIB_DIR = -L$(MATLAB_DIR)/bin/maci -L$(MATLAB_DIR)/bin -L$(MATLAB_DIR)/sys/os/maci
MATLAB_HDF = -lhdf5.5
MATLAB_ICU_LIB = $(MATLAB_DIR)/bin/maci
MATLAB_ICUDATA = $(MATLAB_ICU_LIB)/libicudata.dylib.38 $(MATLAB_ICU_LIB)/libicui18n.dylib.38 \
	$(MATLAB_ICU_LIB)/libicuio.dylib.38 $(MATLAB_ICU_LIB)/libicuuc.dylib.38
MATLAB_LIBRARIES = $(MATLAB_LIB_DIR) -leng -lmx -lmat -lut -lz -lstdc++ -lc $(MATLAB_ICUDATA) \
	-lz.1 -lxerces-c.27 $(MATLAB_HDF)

MEX_OBJECTS =	../gmat_mex/src/MatlabClient.o \
				../gmat_mex/src/MatlabConnection.o
				
# Set to 0 for Windows, 1 for Linux or Mac
LINUX_MAC = 1   

# Set to 0 for Linux, 1 for Mac 
MAC_SPECIFIC = 1

# Set to 1 to build against the MATLAB version, 0 for no MATLAB
USE_MATLAB = 1

DEBUG_BUILD = 0

# Select the base library - base library now built without MATLAB
BASE_LIBRARY = GmatBase
BASE_LIB_LOCATION = GMAT.app/Contents/Frameworks/

# Compiler options
CPP = g++
C = gcc

ifeq ($(PROFILE_BUILD), 1)
PROFILE_FLAGS = -pg
else
PROFILE_FLAGS = 
endif

# wxWidgets flags
WXCPPFLAGS = `$(WX_INSTALLED)/wx-config --cppflags`
WXLINKFLAGS = `$(WX_INSTALLED)/wx-config --libs --universal=no --static=no`


SHARED_EXTENSION = .dylib
SHARED_LIB_FLAGS = -bundle -two_levelnamespace -undefined dynamic_lookup \
                   -L$(GMAT_CODE_LOCATION)/base/lib -l$(BASE_LIBRARY) \
                    $(MAC_CPP_FLAGS) $(WXLINKFLAGS) $(MATLAB_LIBRARIES)

# where are these used????
DESIRED_OPTIMIZATIONS =  -DSTRICT -Wall -fno-pcc-struct-return -O3 \
                 -finline-functions -funroll-loops -fno-rtti -DNO_GCC_PRAGMA \
                 -march=pentium -malign-double -fexceptions \
                 -fexpensive-optimizations

ifeq ($(DEBUG_BUILD), 1)
OPTIMIZATIONS = -fno-strict-aliasing $(WX_28_DEFINES)
else
OPTIMIZATIONS = -O3 -fno-strict-aliasing $(WX_28_DEFINES)
endif

# Do not edit below this line -- here we build up longer compile/link strings
CPP_BASE = $(OPTIMIZATIONS) -Wall \
           $(MATLAB_CPP_FLAGS) $(WXCPPFLAGS) $(PROFILE_FLAGS) $(DEBUG_FLAGS)

CPPFLAGS = $(CPP_BASE)

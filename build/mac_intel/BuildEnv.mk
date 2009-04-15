# $Id$
# Build environment file for Linux
# Modifed for Mac OS 10.3  W. Shoan - 2005.11.10
# Modifed for Mac OS 10.4  W. Shoan - 2006.01.19
# Modified a whole bunch after that  W. Shoan  - 2006 - 2007
# NOTES to user - changes are necessary where you see the "*** EDIT THIS ***" notes

# Flags used to control the build
# Make this 1 if you want MATLAB 
USE_MATLAB = 1
USE_SPICE = 1
USE_DEVIL = 0
CONSOLE_APP = 0
DEBUG_BUILD = 0
PROFILE_BUILD = 0
WX_28_SYNTAX = 1
USE_STC_EDITOR = 0

# Not currently used 
USE_SHARED = 1

# flag indicating whether or not to build as a shared library
SHARED_BASE = 0

# *** EDIT THIS *** - put the top of the GMAT project directory structure here ....
TOP_DIR = < put your top level directory here >
# *** EDIT THIS *** - this is where you installed the version of wxMac that you're using ...
WX_HOME = /Applications/wxmac-2.8.8/osx-build
#WX_HOME = /Applications/wxmac-2.8.7/osx-build
# *** EDIT THIS *** - 'sudo make install' of wxMac will put things here ......
WX_INSTALLED = /usr/local/bin
# *** EDIT THIS *** - this is where you installed MATLAB ......
MATLAB = /Applications/MATLAB_R2007b
# *** EDIT THIS *** - this should match the version of wxMac you are using
#INSTALL_WX_LIBS = install_libs_into_bundle_2_8_7
INSTALL_WX_LIBS = install_libs_into_bundle_2_8_8
#INSTALL_WX_LIBS = install_libs_into_bundle_2_8_10

BUILD = release

ifeq ($(USE_SPICE), 1)
# location of CSPICE headers and libraries
# *** EDIT THIS *** -this is where you installed the version of CSPICE that you're using ...
SPICE_DIR = /Applications/CSPICE
SPICE_INCLUDE = -I$(SPICE_DIR)/cspice/include
SPICE_LIB_DIR = $(SPICE_DIR)/cspice/lib
SPICE_LIBRARIES = $(SPICE_LIB_DIR)/cspice.a
SPICE_DIRECTIVE = -D__cplusplus -D__USE_SPICE__
SPICE_STACKSIZE = ulimit -s 61440
else
SPICE_INCLUDE =
SPICE_LIB_DIR =
SPICE_LIBRARIES =
SPICE_DIRECTIVE = 
SPICE_STACKSIZE = echo 'SPICE not included in this build ...'
endif

ifeq ($(PROFILE_BUILD), 1)
USE_PROFILING = 1
else
USE_PROFILING = 0
endif

# currently cannot use MATLAB or shared base library with console version 
ifeq ($(CONSOLE_APP), 1)
USE_MATLAB = 0
SHARED_BASE = 0
endif

# MATLAB specific data
MATLAB_INCLUDE = -I${MATLAB}/extern/include \
				-I$(TOP_DIR)/src/matlab/gmat_mex/src
MATLAB_LIB = -L${MATLAB}/bin/maci \
			-L$(MATLAB)/bin \
             -L${MATLAB}/sys/os/maci
MATLAB_LIBRARIES = -leng -lmx -lut -lmat \
                   -lz -lstdc++ -lc \
                   -licudata -licuuc -licui18n -licuio -lz.1 -lxerces-c.27 -lhdf5.0

# DevIL data
ifeq ($(USE_DEVIL), 1)
IL_HEADERS = -I../DevIL/il -I../DevIL 
IL_LIBRARIES = -L../devIL -lilu -lilut -lDevIL
else
IL_HEADERS = -DSKIP_DEVIL 
IL_LIBRARIES = 
endif

# wxWidgets settings
ifeq ($(WX_28_SYNTAX), 1)
WX_28_DEFINES = -D__USE_WX280__ -D__USE_WX280_GL
else
WX_28_DEFINES = 
endif

WXCPPFLAGS = `$(WX_INSTALLED)/wx-config --cppflags`
WXLINKFLAGS = `$(WX_INSTALLED)/wx-config --libs --universal=no --static=no`

#
# Compiler options
CPP = g++
C = gcc
# *** EDIT THIS *** - You need a fortran compiler here ............
#FORTRAN = /sw/bin/g77
FORTRAN = /usr/local/bin/gfortran

PROFILE_FLAGS = -pg

SOME_OPTIMIZATIONS = -O3 -funroll-loops -fno-rtti

ifeq ($(USE_PROFILING), 1)
OPTIMIZATIONS = $(SOME_OPTIMIZATIONS) $(PROFILE_FLAGS)
else
OPTIMIZATIONS = $(SOME_OPTIMIZATIONS) 
endif

LINUX_MAC = 1
MAC_SPECIFIC = 1

# For MacOS application
ifeq ($(MAC_SPECIFIC),1)
ifeq ($(USE_MATLAB),1)
EXECUTABLE 	= $(TOP_DIR)/bin/GMAT
else
EXECUTABLE  = $(TOP_DIR)/bin/GMATNoMatlab
endif
# *** EDIT THIS *** - put the version number of the wxMac that you're using here ...
#WX_VERSION   = 2.8.10
WX_VERSION   = 2.8.8
GMAT_INFO    = $(TOP_DIR)/src/gui/Info_GMAT.plist
CONTENTS_DIR = $(EXECUTABLE).app/Contents
MACOS_DIR    = $(CONTENTS_DIR)/MacOS
RES_DIR      = $(CONTENTS_DIR)/Resources
LIBS_DIR     = $(CONTENTS_DIR)/Frameworks
MAC_APP      = $(MACOS_DIR)/GMAT
MAC_SCRIPT_DIR   = $(MACOS_DIR)/
MAC_PKG      = $(CONTENTS_DIR)/Info.plist
MAC_PKGINFO  = $(CONTENTS_DIR)/PkgInfo
GMAT_ICONS   = $(TOP_DIR)/bin/files/icons/GMATIcon.icns
MAC_ICONS    = $(RES_DIR)/gmat.icns

#REZ =
# Define macros for linking the Carbon and wx resource files
REZ = /Developer/Tools/Rez -d __DARWIN__ -t APPL -d __WXMAC__ Carbon.r
# *** EDIT THIS *** - Point to where the FORTRAN libraries are
#FORTRAN_LIB = -L/sw/lib  
#FORTRAN_LIB = -L/usr/local/lib -lgcc_s.1 -lgfortran 
#FORTRAN_LIB = -L/usr/local/lib -lgfortran 
FORTRAN_LIB = -L/usr/local/gfortran/lib -lgfortran 

else 
REZ = 
FORTRAN_LIB =                
endif 

# Build specific flags
MATLAB_FLAGS = -D__USE_MATLAB__=1

ifeq ($(CONSOLE_APP),1)
CONSOLE_FLAGS = -D__CONSOLE_APP__
else
CONSOLE_FLAGS = 
endif

# Set options for debugging and profiling
DEBUG_FLAGS = 

# Build the complete list of flags for the compilers
ifeq ($(USE_MATLAB),1)
CPPFLAGS = $(OPTIMIZATIONS) $(CONSOLE_FLAGS) -Wall $(MATLAB_FLAGS) \
           $(WXCPPFLAGS) $(SPICE_INCLUDE) $(SPICE_DIRECTIVE)\
           $(MATLAB_INCLUDE) $(IL_HEADERS) \
           -fpascal-strings -I/Developer/Headers/FlatCarbon  \
           -D__WXMAC__ $(WX_28_DEFINES) -fno-strict-aliasing -fno-common
F77_FLAGS = $(SOME_OPTIMIZATIONS) $(CONSOLE_FLAGS) -Wall $(MATLAB_FLAGS) \
           $(WXCPPFLAGS) \
           $(MATLAB_INCLUDE) $(IL_HEADERS) -D__WXMAC__ $(WX_28_DEFINES)
TCPIP_OBJECTS =	$(TOP_DIR)/src/matlab/gmat_mex/src/MatlabClient.o \
				$(TOP_DIR)/src/matlab/gmat_mex/src/MatlabConnection.o
else
CPPFLAGS = $(OPTIMIZATIONS) $(CONSOLE_FLAGS) -Wall \
           $(WXCPPFLAGS) $(SPICE_INCLUDE) $(SPICE_DIRECTIVE) $(IL_HEADERS) -D__WXMAC__ $(WX_28_DEFINES)
F77_FLAGS = $(OPTIMIZATIONS) $(CONSOLE_FLAGS) -Wall \
            $(WXCPPFLAGS) $(IL_HEADERS) -D__WXMAC__ $(WX_28_DEFINES)
TCPIP_OBJECTS =	
endif

# Link specific flags
# *** EDIT THIS *** - put the correcct wx lib here (based on the version you're using, 
#                     i.e. 2.8, 2.6, etc.)
ifeq ($(USE_MATLAB),1)
LINK_FLAGS = $(WXLINKFLAGS)\
             $(MATLAB_LIB) $(MATLAB_LIBRARIES) \
             $(SPICE_LIBRARIES) -lm\
             $(FORTRAN_LIB) -framework OpenGL -framework AGL -headerpad_max_install_names\
             -lwx_mac_gl-2.8 $(IL_LIBRARIES) $(DEBUG_FLAGS)
#             -lwx_mac_gl-2.8 -lg2c $(IL_LIBRARIES) $(DEBUG_FLAGS)
else
LINK_FLAGS = $(WXLINKFLAGS)\
               $(FORTRAN_LIB) -framework OpenGL -framework AGL  -headerpad_max_install_names \
               $(SPICE_LIBRARIES) -lm\
             -lwx_mac_gl-2.8 $(DEBUG_FLAGS) $(IL_LIBRARIES) 
#             -lwx_mac_gl-2.8 -lg2c $(DEBUG_FLAGS) $(IL_LIBRARIES) 
endif

# currently cannot use MATLAB with console version
# ifeq ($(USE_MATLAB),1)
# CONSOLE_LINK_FLAGS = $(MATLAB_LIB) $(MATLAB_LIBRARIES) -L../../base/lib \
#            			$(FORTRAN_LIB) \
#                     -lg2c -ldl $(DEBUG_FLAGS) 
# else
#CONSOLE_LINK_FLAGS = -L../../base/lib $(FORTRAN_LIB) $(SPICE_LIBRARIES) -lm -lg2c -ldl $(DEBUG_FLAGS) 
CONSOLE_LINK_FLAGS = -L../../base/lib $(FORTRAN_LIB) $(SPICE_LIBRARIES) -lm -ldl $(DEBUG_FLAGS) 
# endif

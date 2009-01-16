# $Id$
# Build environment file for Windows

# Flags used to control the build
USE_MATLAB = 1
USE_DEVIL = 0
CONSOLE_APP = 0
DEBUG_BUILD = 0
PROFILE_BUILD = 0
WX_28_SYNTAX = 1
WX_SHARED = 1
SHARED_BASE = 1
USE_STC_EDITOR = 1

# GMAT application icon for Windows only
GMAT_ICON_RC = D:/Projects/GmatDev/src/gui/resource/GmatIcon.rc
GMAT_ICON_O  = D:/Projects/GmatDev/src/gui/resource/GmatIcon.o

# The Console app does not support MATLAB linkage or shared base libraries for now
ifeq ($(CONSOLE_APP), 1)
USE_MATLAB = 0
CONSOLE_FLAGS = -D__CONSOLE_APP__
else
CONSOLE_FLAGS =
endif

# MATLAB specific data
ifeq ($(USE_MATLAB), 1)
MATLAB_CPP_FLAGS = -D__USE_MATLAB__=1 -IC:/Program\ Files/MATLAB/R2007B/extern/include
MATLAB_LIB = -LC:/Program\ Files/MATLAB/R2007B/bin/win32
MATLAB_LIBRARIES = -leng -lmx -lmat
else
MATLAB_CPP_FLAGS =
MATLAB_LIB =
MATLAB_LIBRARIES =
endif

# DevIL data
ifeq ($(USE_DEVIL), 1)
IL_CPP_FLAGS = -ID:/DevIL/include/il -ID:/DevIL/include
IL_LIBRARIES = -LD:/devIL/dlls -lilu -lilut -lDevIL
else
IL_CPP_FLAGS = -DSKIP_DEVIL
IL_LIBRARIES =
endif

# STC editor (wxStyledTextCtrl) data
ifeq ($(USE_STC_EDITOR), 1)
STC_CPP_FLAGS = -D__USE_STC_EDITOR__
STC_LIBRARIES = -LD:/wxWidgets-2.8.9/lib -lwx_msw_stc-2.8
else
STC_CPP_FLAGS =
STC_LIBRARIES =
endif

GMAT_CPP_FLAGS = $(MATLAB_CPP_FLAGS) $(IL_CPP_FLAGS) $(STC_CPP_FLAGS)
GMAT_LINK_FLAGS = $(MATLAB_LIB) $(MATLAB_LIBRARIES) $(IL_LIBRARIES) $(STC_LIBRARIES)

# wxWidgets settings
ifeq ($(WX_28_SYNTAX), 1)
WX_28_DEFINES = -D__USE_WX280__ -D__USE_WX280_GL__ -DwxUSE_GLCANVAS
else
WX_28_DEFINES = 
endif

# Compiler options
CPP = g++
C = gcc
FORTRAN = g77
FORTRAN_LIB = -LC:/MinGW/lib -lg2c

ifeq ($(PROFILE_BUILD), 1)
PROFILE_FLAGS = -pg
else
PROFILE_FLAGS = 
endif

ifeq ($(SHARED_BASE), 1)
SHARED_LIB_FLAGS = $(FORTRAN_LIB) $(GMAT_LINK_FLAGS) -shared -Wl --out-implib
else
SHARED_LIB_FLAGS = 
endif

OPTIMIZATIONS =  -DwxUSE_UNIX=0 -D_X86_=1 -DWIN32 -DWINVER=0x0400 -D__WIN95__ \
                 -D__GNUWIN32__ -D__WIN32__ -mthreads -DSTRICT  -D__WXMSW__ \
                 -D__WINDOWS__ -Wall -fno-pcc-struct-return -O3\
                 -finline-functions -funroll-loops -fno-rtti -DNO_GCC_PRAGMA \
                 -malign-double -fexceptions -D__USE_WX280_GL__\
                 -fexpensive-optimizations -march=pentium4
                 
#  -march=pentium4 -mfpmath=sse -fomit-frame-pointer -DNDEBUG

# Do not edit below this line -- here we build up longer compile/link strings
LINUX_MAC = 0

WXCPPFLAGS = `/usr/local/bin/wx-config --cppflags`
WXLINKFLAGS = `/usr/local/bin/wx-config --libs --gl-libs --static=no` \
               -lopengl32 -lglu32

# Set options for debugging and profiling
ifeq ($(DEBUG_BUILD), 1)
DEBUG_FLAGS = -g
else
DEBUG_FLAGS = 
endif

# Build the complete list of flags for the compilers
CPP_BASE = $(OPTIMIZATIONS) $(CONSOLE_FLAGS) $(GMAT_CPP_FLAGS) -Wall \
           $(WXCPPFLAGS) $(PROFILE_FLAGS) $(DEBUG_FLAGS)

CPPFLAGS = $(CPP_BASE) $(PROFILE_FLAGS) $(DEBUG_FLAGS)

F77_FLAGS = $(CPPFLAGS)

LINK_FLAGS = $(GMAT_LINK_FLAGS) $(WXLINKFLAGS) \
             $(FORTRAN_LIB) $(DEBUG_FLAGS)

ifeq ($(USE_MATLAB),1)
CONSOLE_LINK_FLAGS = $(MATLAB_LIB) $(MATLAB_LIBRARIES) -L../base/lib \
                    -lgfortran $(DEBUG_FLAGS) $(PROFILE_FLAGS)
else
CONSOLE_LINK_FLAGS = -L../base/lib $(FORTRAN_LIB) $(DEBUG_FLAGS) 
endif

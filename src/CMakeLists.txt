# $Id$
# 
# GMAT: General Mission Analysis Tool.
# 
# CMAKE script file for GMAT base library
# This file must be installed in the build directory of GMAT.
# That is, we should have the directory structure
#   ./build
#   ./src/base 
#   ./src/console
#   ./src/gui
#
# Author: Joris T. Olympio
#  

# if you want to force a compiler, do it here
#if(UNIX)
#   SET (CMAKE_C_COMPILER "gcc")
#   SET (CMAKE_CXX_COMPILER "g++")
#endif()

PROJECT(GMAT)

cmake_minimum_required(VERSION 2.6)

# uncomment line below only if you want to use a
#  specific fortran compiler: g77, gfortran,...
#SET (CMAKE_Fortran_COMPILER "g77")

SET (USE_SPICE  ON)
SET (USE_DEVIL  OFF) 
SET (DEBUG_BUILD  OFF) 
SET (PROFILE_BUILD  OFF)
SET (WX_28_SYNTAX  ON)
SET (WX_SHARED  ON)
SET (SHARED_BASE  ON)    
SET (USE_64_BIT_LONGS  OFF)
SET (USE_STC_EDITOR  OFF)  
SET (USE_WX_EMAIL  OFF)
SET (SHOW_HELP_BUTTON  ON)
SET (SMART_APPLY_BUTTON  ON)

# F2C directory
# header and library must be in F2C_PATH/include and F2C_PATH/lib respectively
# F2C is available at: http://www.netlib.org/f2c/
SET (USE_F2C ON) 
SET(F2C_PATH "D:/gmat-buildbranch/depends/f2c/")

# (dev) to print out what Make is doing.
SET (VERBOSE_MAKE OFF)  

# specify the version of the build
SET(GMAT_VERSION "2012")


# The script will automatically locate  
# - wxWidgets library, if installed
# - DevIL library, if installed
# FindDevIL.cmake and FindwxWidgets.cmake are usually present with last versions
# of CMake.
# If the script is unable to locate the libraries, or if you want to
# specify it, you should manually set the variables DevIL_DIR or CSPICE_DIR 
# for DevIL and Spice paths respectively.
# In case a library cannot be found, the script will disable the 
# appropriate USE_**** options and continue with the compilation, but for
# wxWidgets when the script will actually terminate.
#
# The script is not able to automatically locate the CSPICE library 
# (it is still experimental), as it does not follow and standardized 
# installation/configuration procedure. You will have to specify CSPICE_DIR.
# In case of error, USE_SPICE will be turned OFF.

# Set the DevIL directory. Set is CMake is unable to find its location.
#SET(DevIL_DIR "/usr/local/include/IL")

# Set the SPICE directory. 
# CSPICE is available at: http://naif.jpl.nasa.gov/naif/toolkit.html
#SET (CSPICE_DIR "/home/joris/cspice")
SET (CSPICE_DIR "D:/gmat-buildbranch/depends/cspice/cspice32/")

SET (WXWIDGET_DIR "D:/gmat-buildbranch/depends/wxWidgets/wxWidgets-2.8.12/")

# DO NOT EDIT BELOW THIS LINE
# ====================================================================

#


# build configurate
if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE Release)
endif()

if (DEBUG_BUILD)
    SET(CMAKE_BUILD_TYPE Debug)
endif (DEBUG_BUILD)

# ====================================================================
# specific win32 ressources
if(WIN32)
    SET(GMAT_ICON_DIR "resource")  # relative to src/gui/
    if(WIN32)
        SET(GMAT_ICON_RC  "GmatIcon.rc")
    elseif(APPLE) 
        SET(GMAT_ICON_RC  "GMATMac.icns")       
    endif()
    SET(GMAT_ICON_O  "GmatIcon.o")
endif()

# ====================================================================
#need to include Find****.cmake
SET( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# ====================================================================
# Common build options
INCLUDE(CheckCXXCompilerFlag)

SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -W2")
# for some reasons, the following optimizations do not work 
# for me on Ubuntu Karmic, but fine on Windows
#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-pcc-struct-return")
#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -finline-functions -funroll-loops -fno-rtti")
#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=pentium")
#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -malign-double -fexceptions -fexpensive-optimizations")
SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing")

# check ffriend-injection flag is allowed
CHECK_CXX_COMPILER_FLAG("-ffriend-injection" GCC_FFRIEND_INJECTION)
if( GCC_FFRIEND_INJECTION )
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffriend-injection")
endif()

# check fPIC flag is allowed
CHECK_CXX_COMPILER_FLAG("-fPIC" GCC_PIC)
if( GCC_PIC )
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
endif()


# flags to append to CMAKE_CXX_FLAGS according to CMAKE_BUILD_TYPE 
SET(CMAKE_CXX_FLAGS_RELEASE "-O3")
SET(CMAKE_CXX_FLAGS_DEBUG  "-O0 -g")
SET(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
SET(CMAKE_CXX_FLAGS_MINSIZEREL "-O3")

# some definitions
#ADD_DEFINITIONS("-DSTRICT")
ADD_DEFINITIONS("-DNO_GCC_PRAGMA")


#if(SHARED_BASE)
    set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--no-undefined --out-implib")
    #LINK_LIBRARIES("dl")
#endif()

# CYGWIN specifics
if(CYGWIN)
    CHECK_CXX_COMPILER_FLAG("-mno-cygwin" GCC_NOCYGWIN)

    if( GCC_NOCYGWIN)
        SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mno-cygwin")
    endif()
endif()


#
if (PROFILE_BUILD)
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg")
endif()

#
if (USE_64_BIT_LONGS)
    MESSAGE("64-bit compilation")
    ADD_DEFINITIONS("-DUSE_64_BIT_LONGS")
endif()

#
if (NOT USE_DEVIL OR NOT UNIX)
    ADD_DEFINITIONS("-DSKIP_DEVIL")
endif()

#
if (NOT CONSOLE_APP)
    if (WX_28_SYNTAX)
        ADD_DEFINITIONS("-D__USE_WX280__")
        ADD_DEFINITIONS("-D__USE_WX280_GL__")
        ADD_DEFINITIONS("-DwxUSE_GLCANVAS")
    endif()
endif()

# to print out what Make is doing
if (VERBOSE_MAKE)
    SET(CMAKE_VERBOSE_MAKEFILE ON)
endif()

# ====================================================================
# Mac specifics (not tested)
if(APPLE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} current_version 0.5 -compatibility_version 0.5 -fvisibility=default")
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -framework OpenGL -framework AGL  -headerpad_max_install_names")
    LINK_DIRECTORIES("stdc++.6")
endif()

ADD_DEFINITIONS("-DLINUX_MAC")

# ====================================================================
# dynamic base lib
SUBDIRS(../src/base)

# ====================================================================
# GUI binary
SUBDIRS(../src/gui)

# ====================================================================
# Console binary
SUBDIRS(../src/console)


# ====================================================================
# plugins...


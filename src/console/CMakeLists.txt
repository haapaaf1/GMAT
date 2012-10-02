# $Id$
# 
# GMAT: General Mission Analysis Tool.
# 
# CMAKE script file for GMAT GUI
# This file must be installed in the src/console directory
#
# Author: Joris T. Olympio
#  
# DO NOT MODIFY THIS FILE UNLESS YOU KNOW WHAT YOU ARE DOING!
#
PROJECT(GMAT_console)

MESSAGE("==============================")
MESSAGE("Console building setup " ${VERSION})

if (PROFILE_BUILD)
	SET(CONSOLETARGET "gmatConsoleP")
else()
	SET(CONSOLETARGET "gmatConsole")
endif()

# ====================================================================
# SPICE specifics

if (USE_SPICE)
    MESSAGE("-- Building with SPICE")

    # check if provided directory exists
    if( (IS_DIRECTORY ${CSPICE_DIR}) AND (EXISTS ${CSPICE_DIR}) )
        #MESSAGE("SPICE FOUND")
        SET(USE_SPICE TRUE)
    else()
        #MESSAGE(FATAL_ERROR "SPICE NOT FOUND")
        MESSAGE("** SPICE NOT FOUND in directory " ${CSPICE_DIR})
        MESSAGE("** Disabling USE_CSPICE")
        SET(USE_SPICE FALSE)
    endif()


    if (USE_SPICE)
        MESSAGE(STATUS "SPICE FOUND")
        MESSAGE("-- CSPICE_DIR=" ${CSPICE_DIR})

        INCLUDE_DIRECTORIES("${CSPICE_DIR}/include")
        SET(SPICE_LIB_DIR  "${CSPICE_DIR}/lib")
        LINK_DIRECTORIES(${SPICE_LIB_DIR})
        LINK_LIBRARIES("${SPICE_LIB_DIR}/cspice.lib")
        ADD_DEFINITIONS("-D__USE_SPICE__")
        #SPICE_STACKSIZE = ulimit -s 61440

        SET(SPICE_SRCS
         	 "attitude/SpiceAttitude.cpp"
             "util/SpiceAttitudeKernelReader.cpp"
    	     "util/SpiceInterface.cpp"
             "util/SpiceOrbitKernelReader.cpp"
             "util/SpiceKernelReader.cpp"
             "util/SpiceKernelWriter.cpp"
    	     "util/SpiceOrbitKernelWriter.cpp")
    endif()

else()
    MESSAGE("Building without SPICE")
endif()


# ====================================================================
# source files
SET(SRCS 
    driver.cpp 
    ConsoleAppException.cpp
    PrintUtility.cpp
    ConsoleMessageReceiver.cpp
)

# ====================================================================
# include directories for header files
INCLUDE_DIRECTORIES("../base/include")
INCLUDE_DIRECTORIES("../base/foundation")
INCLUDE_DIRECTORIES("../base/executive")
INCLUDE_DIRECTORIES("../base/factory")
INCLUDE_DIRECTORIES("../base/forcemodel") 
INCLUDE_DIRECTORIES("../base/subscriber")
INCLUDE_DIRECTORIES("../base/command")
INCLUDE_DIRECTORIES("../base/spacecraft")
INCLUDE_DIRECTORIES("../base/solarsys")
INCLUDE_DIRECTORIES("../base/propagator")
INCLUDE_DIRECTORIES("../base/interpreter")
INCLUDE_DIRECTORIES("../base/parameter")
INCLUDE_DIRECTORIES("../base/interpolator")
INCLUDE_DIRECTORIES("../base/util")
INCLUDE_DIRECTORIES("../base/stopcond") 
INCLUDE_DIRECTORIES("../base/hardware")
INCLUDE_DIRECTORIES("../base/configs")
INCLUDE_DIRECTORIES("../base/burn")
INCLUDE_DIRECTORIES("../base/solver")
INCLUDE_DIRECTORIES("../base/function")
INCLUDE_DIRECTORIES("../base/coordsystem")
INCLUDE_DIRECTORIES("../base/attitude")
INCLUDE_DIRECTORIES("../base/math")
INCLUDE_DIRECTORIES("../base/plugin")
INCLUDE_DIRECTORIES("../base/event")
	
# ====================================================================
# libraries
LINK_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}/../base/lib")
LINK_LIBRARIES(libGmatBase)


# ====================================================================
# compilation

# add the install targets
SET(EXECUTABLE_OUTPUT_PATH "../../bin")

ADD_EXECUTABLE(${CONSOLETARGET} ${SRCS})

SET_TARGET_PROPERTIES(${CONSOLETARGET} PROPERTIES TARGET_VERSION_MAJOR "${VERSION}")

# ====================================================================
# installation (make install)
SET(CMAKE_INSTALL_PREFIX "./")  # default is usr/local

if  (SHARED_BASE)
	INSTALL(TARGETS ${TargetName} DESTINATION "${CMAKE_CURRENT_SOURCE_DIR}/../../../../application/bin")
endif()
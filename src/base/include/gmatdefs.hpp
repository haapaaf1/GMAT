//$Id$
// *** File Name : gmatdefs.h
// *** Created   : May 20, 2003
// **************************************************************************
// ***  Developed By  :  Thinking Systems, Inc. (www.thinksysinc.com)     ***
// ***  For:  Flight Dynamics Analysis Branch (Code 595)                  ***
// ***  Under Contract:  P.O.  GSFC S-67573-G                             ***
// ***                                                                    ***
// ***  Header Version: July 12, 2002                                     ***
// **************************************************************************
// Module Type               : ANSI C++ Source
// Development Environment   : Visual C++ 7.0
// Modification History      : 5/20/2003 - D. Conway, Thinking Systems, Inc.
//                             Original delivery
//
// **Legal**
//
//                           : 2003/09/16 - W. Shoan/GSFC/583
//                             added ObjectTypes enum type in namespace Gmat
//                           : 11/9/2003 - D. Conway, Thinking Systems, Inc.
//                             Added OBJECT_TYPE for member objects
// **************************************************************************

#ifndef GMATDEFS_HPP
#define GMATDEFS_HPP


#include <string>               // For std::string
#include <cstring>              // Resolves issues for GCC 4.3
#include <vector>               // For std::vector
#include <map>                  // For std::map
#include <stack>                // for std::stack

#ifdef _WIN32  // Windows

   #ifdef _MSC_VER  // Microsoft Visual C++

      #define WIN32_LEAN_AND_MEAN  // Exclude rarely-used stuff from Windows headers
      // Windows Header File entry point:
      #include <windows.h>

      #define  _USE_MATH_DEFINES  // makes Msoft define pi

   #endif  // End Microsoft C++ specific block

   #ifdef _DYNAMICLINK  // Only used for Windows DLLs
      #ifdef GMAT_EXPORTS
         #define GMAT_API __declspec(dllexport)
      #else
         #define GMAT_API __declspec(dllimport)
      #endif
   #endif

#endif //  End of OS nits

#ifndef GMAT_API
   #define GMAT_API
#endif


typedef double          Real;              // 8 byte float
typedef int             Integer;           // 4 byte signed integer
typedef unsigned char   Byte;              // 1 byte
typedef unsigned int    UnsignedInt;       // 4 byte unsigned integer

typedef std::vector<Real>        RealArray;
typedef std::vector<Integer>     IntegerArray;
typedef std::vector<UnsignedInt> UnsignedIntArray;
typedef std::vector<std::string> StringArray;

class GmatBase;                            // Forward reference for ObjectArray
class ElementWrapper;
typedef std::vector<GmatBase*> ObjectArray;
typedef std::vector<ElementWrapper*> WrapperArray;
typedef std::map<std::string, GmatBase*> ObjectMap;
typedef std::map<std::string, ElementWrapper*> WrapperMap;
typedef std::stack<ObjectMap*> ObjectMapStack;

typedef struct geoparms
{
   Real xtemp;  /// minimum global exospheric temperature (degrees K)
   Real tkp;    /// geomagnetic index

} GEOPARMS;


/// GMAT's epoch representation; eventually a struct holding MJ day & sec of day
typedef double GmatEpoch;
typedef std::vector<Integer> IntegerArray;


namespace Gmat
{
   /**
    * The list of object types
    *
    * This list needs to be synchronized with the GmatBase::OBJECT_TYPE_STRING
    * list found in base/Foundation/GmatBase.cpp
    */
   enum ObjectType
   {
      SPACECRAFT= 1001,
      FORMATION,
      SPACEOBJECT,
      GROUND_STATION,
      BURN,
      IMPULSIVE_BURN,
      FINITE_BURN,
      COMMAND,
      PROPAGATOR,
      ODE_MODEL,
//      FORCE_MODEL = ODE_MODEL,
      PHYSICAL_MODEL,
      TRANSIENT_FORCE,
      INTERPOLATOR,
      SOLAR_SYSTEM,
      SPACE_POINT,
      CELESTIAL_BODY,
      CALCULATED_POINT,
      LIBRATION_POINT,
      BARYCENTER,
      ATMOSPHERE,
      PARAMETER,
      VARIABLE,
      ARRAY,
      STRING,
      STOP_CONDITION,
      SOLVER,
      SUBSCRIBER,
      REPORT_FILE,
      XY_PLOT,
      OPENGL_PLOT,
      PROP_SETUP,
      FUNCTION,
      FUEL_TANK,
      THRUSTER,
      HARDWARE,            // Tanks, Thrusters, Antennae, Sensors, etc.
      COORDINATE_SYSTEM,
      AXIS_SYSTEM,
      ATTITUDE,
      MATH_NODE,
      MATH_TREE,
      BODY_FIXED_POINT,
      // Estimation types
      MEASUREMENT_MODEL,
      UNKNOWN_OBJECT
   };


   /**
    * The list of data types
    *
    * This list needs to be synchronized with the GmatBase::PARAM_TYPE_STRING
    * list found in base/Foundation/GmatBase.cpp
    */
   enum ParameterType
   {
      INTEGER_TYPE,
      UNSIGNED_INT_TYPE,
      UNSIGNED_INTARRAY_TYPE,
      REAL_TYPE,
      REAL_ELEMENT_TYPE,
      STRING_TYPE,
      STRINGARRAY_TYPE,
      BOOLEAN_TYPE,
      RVECTOR_TYPE,
      RMATRIX_TYPE,
      TIME_TYPE,
      OBJECT_TYPE,
      OBJECTARRAY_TYPE,
      ON_OFF_TYPE,
      ENUMERATION_TYPE,
      TypeCount,
      UNKNOWN_PARAMETER_TYPE = -1
   };

   enum WrapperDataType
   {
      NUMBER_WT,          // Real, Integer
      MATRIX_WT,          // Rmatix
      STRING_WT,          // a raw text string
      STRING_OBJECT_WT,   // name of a String Object
      OBJECT_PROPERTY_WT,
      VARIABLE_WT,
      ARRAY_WT,
      ARRAY_ELEMENT_WT,
      PARAMETER_WT,
      OBJECT_WT,
      BOOLEAN_WT,
      INTEGER_WT,
      ON_OFF_WT,
      UNKNOWN_WRAPPER_TYPE = -2
   };


   enum RunState
   {
      IDLE = 10000,
      RUNNING,
      PAUSED,
      TARGETING,
      OPTIMIZING,
      SOLVING,
      SOLVEDPASS,
      WAITING
   };

   enum WriteMode
   {
      SCRIPTING,
      SHOW_SCRIPT,
      OWNED_OBJECT,
      MATLAB_STRUCT,
      EPHEM_HEADER,
      NO_COMMENTS,
      GUI_EDITOR,
   };
   
   enum StateElementId 
   {
      UNKNOWN_STATE = -1,
      CARTESIAN_STATE = 3700,          // Integrable state representations
      EQUINOCTIAL_STATE,
      ORBIT_STATE_TRANSITION_MATRIX,   // 6x6 STM for the orbit
      MASS_FLOW,                       // m dot
      PREDEFINED_STATE_MAX,
      USER_DEFINED_BEGIN = 3800,
      USER_DEFINED_END = 3999          // Allow up to 200 dynamic entries
   };
}

typedef std::vector<Gmat::ObjectType> ObjectTypeArray;
typedef std::vector<Gmat::WrapperDataType> WrapperTypeArray;

#endif //GMATDEFS_HPP

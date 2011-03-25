//$Header$
//------------------------------------------------------------------------------
//                             Antenna
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/11/30
//
// Based in part upon elements of the Thruster class.
//
/**
 *
 * Implements the Antenna base class to provide for modeling of antenna
 * performance. An antenna is owned by a parent object and other hardware
 * on the parent object are assigned to use it.
 *
 */
//------------------------------------------------------------------------------


#include "Antenna.hpp"
#include "ObjectReferencedAxes.hpp"
#include "MessageInterface.hpp"
#include <sstream>

//#define DEBUG_ANTENNA
//#define DEBUG_ANTENNA_SET
//#define DEBUG_ANTENNA_INIT
//#define DEBUG_ANTENNA_CONVERT

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif

#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif

//---------------------------------
// static data
//---------------------------------

/// Available local axes labels
StringArray Antenna::localAxesLabels;

/// Labels used for the antenna element parameters.
const std::string
Antenna::PARAMETER_TEXT[AntennaParamCount - HardwareParamCount] =
{
   "IsActive",
   "CoordinateSystem",
   "Origin",
   "Axes",
   "AntennaID",
   "MaxRange",
   "MinRange",
   "MaxRangeRate",
   "MinRangeRate",
   "BoreSightVector_1",
   "BoreSightVector_2",
   "BoreSightVector_3",
   "MaxCoElevation",
   "MinCoElevation"
};

/// Types of the parameters used by antennas.
const Gmat::ParameterType
Antenna::PARAMETER_TYPE[AntennaParamCount - HardwareParamCount] =
{
   Gmat::BOOLEAN_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::ENUMERATION_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE
};


//------------------------------------------------------------------------------
//  Antenna(std::string nomme)
//------------------------------------------------------------------------------
/**
 * Antenna constructor.
 *
 * @param nomme Name of the antenna.
 *
 * @Note coordSystem and spacecraft are set through SetRefObject() during
 *       Sandbox initialization. localOrigin and j2000Body are set when
 *       solarSystem is set. localCoordSystem is created during initialization
 *       or when new spacecraft is set
 */
//------------------------------------------------------------------------------
Antenna::Antenna(std::string nomme) :
    Hardware             (Gmat::HARDWARE, "Antenna", nomme),
    solarSystem          (NULL),
    localCoordSystem     (NULL),
    coordSystem          (NULL),
    localOrigin          (NULL),
    j2000Body            (NULL),
    spacecraft           (NULL),
    groundStation        (NULL),
    coordSystemName      ("EarthMJ2000Eq"),
    localOriginName      ("Earth"),
    localAxesName        ("VNB"),
    j2000BodyName        ("Earth"),
    satName              (GmatBase::STRING_PARAMETER_UNDEFINED),
    stationName          (GmatBase::STRING_PARAMETER_UNDEFINED),
    antennaActive        (false),
    usingLocalCoordSys   (true),
    antennaID            (GmatBase::INTEGER_PARAMETER_UNDEFINED),
    maxRange             (GmatBase::REAL_PARAMETER_UNDEFINED),
    minRange             (GmatBase::REAL_PARAMETER_UNDEFINED),
    maxRangeRate         (GmatBase::REAL_PARAMETER_UNDEFINED),
    minRangeRate         (GmatBase::REAL_PARAMETER_UNDEFINED),
    maxCoElevation       (GmatBase::REAL_PARAMETER_UNDEFINED),
    minCoElevation       (GmatBase::REAL_PARAMETER_UNDEFINED)
{
    objectTypes.push_back(Gmat::ANTENNA);
    objectTypeNames.push_back("Antenna");

    boreSightVector[0] = 1.0;
    boreSightVector[1] = 0.0;
    boreSightVector[2] = 0.0;
    
    inertialDirection[0] = 1.0;
    inertialDirection[1] = 0.0;
    inertialDirection[2] = 0.0;

    // Local axes labels
    localAxesLabels.push_back("VNB");
    localAxesLabels.push_back("LVLH");
    localAxesLabels.push_back("MJ2000Eq");
    localAxesLabels.push_back("SpacecraftBody");

}


//------------------------------------------------------------------------------
//  ~Antenna()
//------------------------------------------------------------------------------
/**
 * Antenna destructor.
 */
//------------------------------------------------------------------------------
Antenna::~Antenna()
{
   if (usingLocalCoordSys && localCoordSystem)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (localCoordSystem, "localCS", "Antenna::~Antenna()",
          "deleting localCoordSystem");
      #endif
      delete localCoordSystem;
      localCoordSystem = NULL;
   }
}


//------------------------------------------------------------------------------
//  Antenna(const Antenna &ant)
//------------------------------------------------------------------------------
/**
 * Antenna copy constructor.
 *
 * @param th The object being copied.
 *
 * @Note coordSystem and spacecraft are set through SetRefObject() during
 *       Sandbox initialization. localOrigin and j2000Body are set when
 *       solarSystem is set. localCoordSystem is created during initialization
 *       or when new spacecraft is set
 */
//------------------------------------------------------------------------------
Antenna::Antenna(const Antenna &ant) :
    Hardware             (ant),
    solarSystem          (NULL),
    localCoordSystem     (NULL),
    coordSystem          (NULL),
    localOrigin          (NULL),
    j2000Body            (NULL),
    spacecraft           (NULL),
    groundStation        (NULL),
    coordSystemName      (ant.coordSystemName),
    localOriginName      (ant.localOriginName),
    localAxesName        (ant.localAxesName),
    j2000BodyName        (ant.j2000BodyName),
    satName              (ant.satName),
    stationName          (ant.stationName),
    antennaActive        (ant.antennaActive),
    usingLocalCoordSys   (ant.usingLocalCoordSys),
    antennaID            (ant.antennaID),
    maxRange             (ant.maxRange),
    minRange             (ant.minRange),
    maxRangeRate         (ant.maxRangeRate),
    minRangeRate         (ant.minRangeRate),
    maxCoElevation       (ant.maxCoElevation),
    minCoElevation       (ant.minCoElevation)
{
    localAxesLabels = ant.localAxesLabels;

    inertialDirection[0] = ant.inertialDirection[0];
    inertialDirection[1] = ant.inertialDirection[1];
    inertialDirection[2] = ant.inertialDirection[2];

    boreSightVector[0] = ant.boreSightVector[0];
    boreSightVector[1] = ant.boreSightVector[1];
    boreSightVector[2] = ant.boreSightVector[2];
}


//------------------------------------------------------------------------------
//  Antenna& operator=(const Antenna &ant)
//------------------------------------------------------------------------------
/**
 * Antenna assignment operator.
 *
 * @param th The object being copied.
 *
 * @return this object, with parameters set to the input object's parameters.
 *
 * @Note coordSystem and spacecraft are set through SetRefObject() during
 *       Sandbox initialization. localOrigin and j2000Body are set when
 *       solarSystem is set. localCoordSystem is created during initialization
 *       or when new spacecraft is set
 */
//------------------------------------------------------------------------------
Antenna& Antenna::operator=(const Antenna &ant)
{
    if (&ant == this)
        return *this;

    Hardware::operator=(ant);

    solarSystem         = NULL;
    localCoordSystem    = NULL;
    coordSystem         = NULL;
    localOrigin         = NULL;
    j2000Body           = NULL;
    spacecraft          = NULL;
    groundStation       = NULL;

    coordSystemName     = ant.coordSystemName;
    localOriginName     = ant.localOriginName;
    localAxesName       = ant.localAxesName;
    j2000BodyName       = ant.j2000BodyName;
    satName             = ant.satName;
    stationName         = ant.stationName;

    antennaID           = ant.antennaID;
    maxRange            = ant.maxRange;
    minRange            = ant.minRange;
    maxRangeRate        = ant.maxRangeRate;
    minRangeRate        = ant.minRangeRate;
    maxCoElevation      = ant.maxCoElevation;
    minCoElevation      = ant.minCoElevation;

    antennaActive       = ant.antennaActive;
    usingLocalCoordSys  = ant.usingLocalCoordSys;

    inertialDirection[0] = ant.inertialDirection[0];
    inertialDirection[1] = ant.inertialDirection[1];
    inertialDirection[2] = ant.inertialDirection[2];

    boreSightVector[0] = ant.boreSightVector[0];
    boreSightVector[1] = ant.boreSightVector[1];
    boreSightVector[2] = ant.boreSightVector[2];

    localAxesLabels      = ant.localAxesLabels;

    return *this;
}


//---------------------------------------------------------------------------
//  GmatBase* Clone() const
//---------------------------------------------------------------------------
/**
 * Provides a clone of this object by calling the copy constructor.
 *
 * @return A GmatBase pointer to the cloned antenna.
 */
//---------------------------------------------------------------------------
GmatBase* Antenna::Clone() const
{
   return new Antenna(*this);
}


//---------------------------------------------------------------------------
//  void Copy(GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 *
 * @param orig The original that is being copied.
 *
 * @return A GmatBase pointer to the cloned antenna.
 */
//---------------------------------------------------------------------------
void Antenna::Copy(const GmatBase* orig)
{
   operator=(*((Antenna *)(orig)));
}


//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string Antenna::GetParameterText(const Integer id) const
{
   if (id >= HardwareParamCount && id < AntennaParamCount)
      return PARAMETER_TEXT[id - HardwareParamCount];

   if (id == DIRECTION_X)
      return "Element1";
   if (id == DIRECTION_Y)
      return "Element2";
   if (id == DIRECTION_Z)
      return "Element3";

   return Hardware::GetParameterText(id);
}

//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Integer Antenna::GetParameterID(const std::string &str) const
{
   for (Integer i = HardwareParamCount; i < AntennaParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - HardwareParamCount])
         return i;
   }

   if (str == "Element1")
      return DIRECTION_X;
   if (str == "Element2")
      return DIRECTION_Y;
   if (str == "Element3")
      return DIRECTION_Z;

   return Hardware::GetParameterID(str);
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType Antenna::GetParameterType(const Integer id) const
{
   if (id >= HardwareParamCount && id < AntennaParamCount)
      return PARAMETER_TYPE[id - HardwareParamCount];

   return Hardware::GetParameterType(id);
}


//------------------------------------------------------------------------------
//  std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type string, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 */
//------------------------------------------------------------------------------
std::string Antenna::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool Antenna::IsParameterReadOnly(const Integer id) const
{
   if ((id == ANTENNA_ACTIVE))
      return true;

   if ((id == ORIGIN || id == AXES))
      if (coordSystemName != "Local")
         return true;

   return Hardware::IsParameterReadOnly(id);
}


//------------------------------------------------------------------------------
//  Real GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve the value for a Real parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The parameter's value.
 */
//------------------------------------------------------------------------------
Real Antenna::GetRealParameter(const Integer id) const
{
   switch (id)
   {
        case MAX_RANGE:
            return maxRange;
        case MIN_RANGE:
            return minRange;
        case MAX_RANGERATE:
            return maxRangeRate;
        case MIN_RANGERATE:
            return minRangeRate;
        case BORESITEVECTOR_1:
            return boreSightVector[0];
        case BORESITEVECTOR_2:
            return boreSightVector[1];
        case BORESITEVECTOR_3:
            return boreSightVector[2];
        case MIN_COELEVATION:
            return minCoElevation;
        case MAX_COELEVATION:
            return maxCoElevation;
       default:
            break;   // Default just drops through
   }

   return Hardware::GetRealParameter(id);
}


//------------------------------------------------------------------------------
//  Real SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * Set the value for a Real parameter.
 *
 * @param id The integer ID for the parameter.
 * @param value The new parameter value.
 *
 * @return the parameter value at the end of this call, or throw an exception
 *         if the parameter id is invalid or the parameter type is not Real.
 */
//------------------------------------------------------------------------------
Real Antenna::SetRealParameter(const Integer id, const Real value)
{
    #ifdef DEBUG_ANTENNA_SET
    MessageInterface::ShowMessage
        ("Antenna::SetRealParameter() '%s' entered, id=%d, value=%f\n",
        GetName().c_str(), id, value);
    #endif

    switch (id)
    {
        case MAX_RANGE:
            maxRange = value;
            break;
        case MIN_RANGE:
            minRange = value;
            break;
        case MAX_RANGERATE:
            maxRangeRate = value;
            break;
        case MIN_RANGERATE:
            minRangeRate = value;
            break;
        case BORESITEVECTOR_1:
            boreSightVector[0] = value;
            break;
        case BORESITEVECTOR_2:
            boreSightVector[1] = value;
            break;
        case BORESITEVECTOR_3:
            boreSightVector[2] = value;
            break;
        case MIN_COELEVATION:
            minCoElevation = value;
            break;
        case MAX_COELEVATION:
            maxCoElevation = value;
            break;
        default:
            break;   // Default just drops through
   }

   return Hardware::SetRealParameter(id, value);
}


//---------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param id The integer ID for the parameter.
 *
 * @return The string stored for this parameter, or throw ab=n exception if
 *         there is no string association.
 */
//---------------------------------------------------------------------------
std::string Antenna::GetStringParameter(const Integer id) const
{
   if (id == COORDINATE_SYSTEM)
      return coordSystemName;
   if (id == ORIGIN)
      return localOriginName;
   if (id == AXES)
      return localAxesName;

   return Hardware::GetStringParameter(id);
}


//---------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string &value)
//---------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param id The integer ID for the parameter.
 * @param value The new string for this parameter.
 *
 * @return true if the string is stored, throw if the string is not stored.
 */
//---------------------------------------------------------------------------
bool Antenna::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_ANTENNA_SET
   MessageInterface::ShowMessage
      ("Antenna::SetStringParameter() '%s' entered, id=%d, value='%s'\n",
       GetName().c_str(), id, value.c_str());
   #endif

   switch (id)
   {
   case COORDINATE_SYSTEM:
      coordSystemName = value;
      return true;
   case ORIGIN:
      localOriginName = value;
      return true;
   case AXES:
      localAxesName = value;
      return true;
   default:
      return Hardware::SetStringParameter(id, value);
   }
}


//---------------------------------------------------------------------------
//  const StringArray& GetStringArrayParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Access an array of string data.
 *
 * @param id The integer ID for the parameter.
 *
 * @return The requested StringArray; throws if the parameter is not a
 *         StringArray.
 */
//---------------------------------------------------------------------------
const StringArray& Antenna::GetStringArrayParameter(const Integer id) const
{
   if (id == AXES)
      return localAxesLabels;

   return Hardware::GetStringArrayParameter(id);
}


//---------------------------------------------------------------------------
//  bool GetBooleanParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve a boolean parameter.
 *
 * @param id The integer ID for the parameter.
 *
 * @return the boolean value for this parameter, or throw an exception if the
 *         parameter access in invalid.
 */
//---------------------------------------------------------------------------
bool Antenna::GetBooleanParameter(const Integer id) const
{
   if (id == ANTENNA_ACTIVE)
      return antennaActive;

   return Hardware::GetBooleanParameter(id);
}


//---------------------------------------------------------------------------
//  bool SetBooleanParameter(const Integer id, const bool value)
//---------------------------------------------------------------------------
/**
 * Sets the value for a boolean parameter.
 *
 * @param id The integer ID for the parameter.
 * @param value The new value.
 *
 * @return the boolean value for this parameter, or throw an exception if the
 *         parameter is invalid or not boolean.
 */
//---------------------------------------------------------------------------
bool Antenna::SetBooleanParameter(const Integer id, const bool value)
{
   if (id == ANTENNA_ACTIVE) {
      #ifdef DEBUG_ANTENNA
         MessageInterface::ShowMessage(
            "Setting antenna %s firing mode to %s\n", instanceName.c_str(),
            (value == true ? "true" : "false"));
      #endif
      antennaActive = value;
      return antennaActive;
   }

   return Hardware::SetBooleanParameter(id, value);
}

//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                   const std::string &name)
//------------------------------------------------------------------------------
/**
 * Sets referenced objects.
 *
 * @param obj The object.
 * @param type Type of the object.
 * @param name Name of the object.
 *
 * @return true if the ref object was set, false if not.
 */
//------------------------------------------------------------------------------
bool Antenna::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                            const std::string &name)
{
   if (obj == NULL)
      return false;

   if (type == Gmat::COORDINATE_SYSTEM && obj->GetType() == Gmat::COORDINATE_SYSTEM &&
       coordSystemName == name)
   {
      coordSystem = (CoordinateSystem*)obj;
      return true;
   }

   if (type == Gmat::CELESTIAL_BODY && obj->GetType() == Gmat::CELESTIAL_BODY &&
       localOriginName == name)
   {
      localOrigin = (CelestialBody*)obj;
      return true;
   }

   if (type == Gmat::CELESTIAL_BODY && obj->GetType() == Gmat::CELESTIAL_BODY &&
       j2000BodyName == name)
   {
      j2000Body = (CelestialBody*)obj;
      return true;
   }

   if (obj->GetType() == Gmat::SPACECRAFT)
   {
      return SetSpacecraft((Spacecraft*)obj);
   }

   if (obj->GetType() == Gmat::GROUND_STATION)
   {
      return SetGroundStation((GroundStation*)obj);
   }

   return Hardware::SetRefObject(obj, type, name);
}


//---------------------------------------------------------------------------
//  ObjectArray& GetRefObjectArray(const Gmat::ObjectType type)
//---------------------------------------------------------------------------
/**
 * Obtains an array of GmatBase pointers by type.
 *
 * @param type The type of objects requested
 *
 * @return Reference to the array.
 */
//---------------------------------------------------------------------------
ObjectArray& Antenna::GetRefObjectArray(const Gmat::ObjectType type)
{
//   if (type == Gmat::HARDWARE)
//      return tanks;

   return Hardware::GetRefObjectArray(type);
}


//---------------------------------------------------------------------------
//  ObjectArray& GetRefObjectArray(const std::string& typeString)
//---------------------------------------------------------------------------
/**
 * Obtains an array of GmatBase pointers based on a string (e.g. the typename).
 *
 * @param typeString The string used to find the objects requested.
 *
 * @return Reference to the array.  This default method returns an empty vector.
 */
//---------------------------------------------------------------------------
ObjectArray& Antenna::GetRefObjectArray(const std::string& typeString)
{
//   if ((typeString == "FuelTank") || (typeString == "Tanks"))
//      return tanks;

   return Hardware::GetRefObjectArray(typeString);
}


//------------------------------------------------------------------------------
//  void SetSolarSystem(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 * Sets the internal solar system pointer for objects that have one.
 *
 * @param ss   The solar system.
 */
//------------------------------------------------------------------------------
void Antenna::SetSolarSystem(SolarSystem *ss)
{
   #ifdef DEBUG_ANTENNA_SET
   MessageInterface::ShowMessage
      ("Antenna::SetSolarSystem() ss=<%p> '%s'\n", ss, ss->GetName().c_str());
   #endif

   if (solarSystem != ss)
   {
      solarSystem = ss;

      if (usingLocalCoordSys)
      {
         localOrigin = solarSystem->GetBody(localOriginName);
         j2000Body = solarSystem->GetBody(j2000BodyName);
      }
   }
}


//------------------------------------------------------------------------------
//  bool Initialize()
//------------------------------------------------------------------------------
/**
 * Sets up the bodies used in the thrust calculations.
 */
//------------------------------------------------------------------------------
bool Antenna::Initialize()
{
   #ifdef DEBUG_ANTENNA_INIT
   MessageInterface::ShowMessage
      ("Antenna::Initialize() <%p>'%s' entered, spacecraft=<%p>\n", this,
       GetName().c_str(), spacecraft);
   #endif

   bool retval = Hardware::Initialize();

   if (retval)
   {
      if (usingLocalCoordSys)
      {
         if (solarSystem == NULL)
            throw HardwareException
               ("Unable to initialize the Antenna object " +
                instanceName + " " + "\"SolarSystem\" was not set for the antenna.");

         if ((!localOrigin) || (!j2000Body))
            throw HardwareException
               ("Unable to initialize the antenna object " +
                instanceName + "; either \"" + j2000BodyName + "\" or \"" +
                localOriginName + "\" was not set for the antenna.");
      }
   }

   // delete old local coordinate system
   if (usingLocalCoordSys && localCoordSystem != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (localCoordSystem, "localCS", "Antenna::Initialize()",
          "deleting localCoordSystem");
      #endif
      delete localCoordSystem;
      localCoordSystem = NULL;
   }

   // If spacecraft is available, create new local coordinate system
   if (usingLocalCoordSys && spacecraft != NULL)
      localCoordSystem = CreateLocalCoordinateSystem();

   // If spacecraft is available, create new local coordinate system
   if (usingLocalCoordSys && groundStation != NULL)
      localCoordSystem = CreateLocalCoordinateSystem();

   if (localCoordSystem == NULL)
   {
      #ifdef DEBUG_ANTENNA_INIT
      MessageInterface::ShowMessage
         ("Antenna::Initialize() <%p>'%s' returning false, localCoordSystem is NULL\n",
          this, GetName().c_str());
      #endif
      return false;
   }

   // Convert direction to inertial coord system
    if (spacecraft != NULL)
    {
        Real epoch = spacecraft->GetRealParameter("A1Epoch");
        ConvertDirectionToInertial(direction, inertialDirection, epoch);
    }
    else if (groundStation != NULL)
    {
        Real epoch = groundStation->GetRealParameter("A1Epoch");
        ConvertDirectionToInertial(direction, inertialDirection, epoch);
    }

   #ifdef DEBUG_ANTENNA_INIT
   MessageInterface::ShowMessage
      ("   Inertial pointing direction:  %18le  %18le  %18le\n",
       inertialDirection[0], inertialDirection[1], inertialDirection[2]);
   MessageInterface::ShowMessage
      ("   %s tank mass computation\n", decrementMass ? "Continue with " : "Skipping");
   #endif

   #ifdef DEBUG_ANTENNA_INIT
   MessageInterface::ShowMessage
      ("Antenna::Initialize() <%p>'%s' returning false\n", this, GetName().c_str());
   #endif

   return retval;
}

//------------------------------------------------------------------------------
//  bool SetSpacecraft(Spacecraft *sat)
//------------------------------------------------------------------------------
/**
 * Accessor method to pass in the spacecraft pointer. This method is usually
 * called during the Sandbox initialization when building Spacecraft owned
 * objects such as tanks and antennas. The spacecraft passes itself to this
 * class instance using Antenna::SetRefObject() in Spacecraft::SetRefObject().
 *
 * This method creates new local coordinate system if current spacecraft is not
 * the same as old spacecraft.
 *
 * @param <sat> the Spacecraft
 */
//------------------------------------------------------------------------------
bool Antenna::SetSpacecraft(Spacecraft *sat)
{
   if (sat == NULL)
      return false;

   #ifdef DEBUG_ANTENNA_SET
   MessageInterface::ShowMessage
      ("Antenna::SetSpacecraft() sat=<%p>'%s', usingLocalCoordSys=%d, "
       "localCoordSystem=<%p>\n", sat, sat->GetName().c_str(), usingLocalCoordSys,
       localCoordSystem);
   #endif

   // If spcecraft is different, create new local coordinate system
   if (spacecraft != sat)
   {
      spacecraft = sat;
      satName = spacecraft->GetName();

      if (usingLocalCoordSys)
      {
         if (localCoordSystem)
         {
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (localCoordSystem, "localCS", "Antenna::SetSpacecraft()",
                "deleting localCoordSystem");
            #endif
            delete localCoordSystem;
            localCoordSystem = NULL;
         }
         localCoordSystem = CreateLocalCoordinateSystem();
      }
   }

   #ifdef DEBUG_ANTENNA_SET
   MessageInterface::ShowMessage("Antenna::SetSpacecraft() returning true\n");
   #endif

   return true;
}

//------------------------------------------------------------------------------
//  bool SetGroundStation(GroundStation *station)
//------------------------------------------------------------------------------
/**
 * Accessor method to pass in the ground station pointer. This method is usually
 * called during the Sandbox initialization when building Ground Stations owned
 * objects such as tanks and antennas. The ground station passes itself to this
 * class instance using Antenna::SetRefObject() in GroundStation::SetRefObject().
 *
 * This method creates new local coordinate system if current ground station
 * is not the same as the old ground station.
 *
 * @param <staion> the Ground Station
 */
//------------------------------------------------------------------------------
bool Antenna::SetGroundStation(GroundStation *station)
{
   if (station == NULL)
      return false;

   #ifdef DEBUG_ANTENNA_SET
   MessageInterface::ShowMessage
      ("Antenna::SetGroundStation() sat=<%p>'%s', usingLocalCoordSys=%d, "
       "localCoordSystem=<%p>\n", sat, sat->GetName().c_str(), usingLocalCoordSys,
       localCoordSystem);
   #endif

   // If ground station is different, create new local coordinate system
   if (groundStation != station)
   {
      groundStation = station;
      stationName = groundStation->GetName();

      if (usingLocalCoordSys)
      {
         if (localCoordSystem)
         {
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (localCoordSystem, "localCS", "Antenna::SetGroundStation()",
                "deleting localCoordSystem");
            #endif
            delete localCoordSystem;
            localCoordSystem = NULL;
         }
         localCoordSystem = CreateLocalCoordinateSystem();
      }
   }

   #ifdef DEBUG_ANTENNA_SET
   MessageInterface::ShowMessage("Antenna::SetGroundStation() returning true\n");
   #endif

   return true;
}

//------------------------------------------------------------------------------
// CoordinateSystem* CreateLocalCoordinateSystem()
//------------------------------------------------------------------------------
CoordinateSystem* Antenna::CreateLocalCoordinateSystem()
{
   #ifdef DEBUG_ANTENNA_INIT
   MessageInterface::ShowMessage
      ("Antenna::CreateLocalCoordinateSystem() <%p>'%s' entered, usingLocalCoordSys=%d, "
       "spacecraft=<%p>\n", this, GetName().c_str(), usingLocalCoordSys, spacecraft);
   #endif

   CoordinateSystem *localCS = NULL;

   // If coordinate system being used is local, then create
   if (usingLocalCoordSys)
   {
      if (solarSystem == NULL)
      {
         // Since SolarSystem may be set later, just return NULL for now
         #ifdef DEBUG_ANTENNA_INIT
         MessageInterface::ShowMessage
            ("Antenna::CreateLocalCoordinateSystem() SolarSystem is not set so, "
             "returning NULL\n");
         #endif
         return NULL;

         //throw HardwareException
         //   ("Unable to initialize the Antenna object " +
         //    instanceName + " " + "\"SolarSystem\" was not set for the antenna.");
      }

      if (spacecraft == NULL)
      {
         // Since spacecraft may be set later, just return NULL for now
         #ifdef DEBUG_ANTENNA_INIT
         MessageInterface::ShowMessage
            ("Antenna::CreateLocalCoordinateSystem() spacecraft is not set so, "
             "returning NULL\n");
         #endif
         return NULL;

         //throw HardwareException("Unable to initialize the Antenna object " +
         //   instanceName + " " + satName + " was not set for the antenna.");
      }

      if (spacecraft != NULL)
        satName = spacecraft->GetName();
      else if (groundStation != NULL)
        stationName = groundStation->GetName();

      localCS = new CoordinateSystem("Local");
      AxisSystem *orAxes = new ObjectReferencedAxes("Local");
      #ifdef DEBUG_MEMORY
        MemoryTracker::Instance()->Add
            (localCS, "localCS", "Antenna::CreateLocalCoordinateSystem()",
            "new CoordinateSystem()");
        MemoryTracker::Instance()->Add
            (orAxes, "localAxes", "Antenna::CreateLocalCoordinateSystem()",
            "new ObjectReferencedAxes()");
      #endif

      orAxes->SetStringParameter("Primary", localOriginName);

      if (spacecraft != NULL)
        orAxes->SetStringParameter("Secondary", satName);
      else if (groundStation != NULL)
        orAxes->SetStringParameter("Secondary", stationName);

      orAxes->SetRefObject(localOrigin, Gmat::SPACE_POINT, localOriginName);

      if (spacecraft != NULL)
        orAxes->SetRefObject(spacecraft, Gmat::SPACE_POINT, satName);
      else if (groundStation != NULL)
        orAxes->SetRefObject(groundStation, Gmat::SPACE_POINT, stationName);

      if (localAxesName == "VNB")
      {
         orAxes->SetStringParameter("XAxis", "V");
         orAxes->SetStringParameter("YAxis", "N");
      }
      else if (localAxesName == "LVLH")
      {
         orAxes->SetStringParameter("XAxis", "-R");
         orAxes->SetStringParameter("YAxis", "-N");
      }
      else if (localAxesName == "SpacecraftBody")
      {
         throw HardwareException("Antenna cannot use SpacecraftBody yet.");
      }
      else if (localAxesName == "GroundStationBody")
      {
         throw HardwareException("Antenna cannot use GroundStationBody yet.");
      }

      // If I set origin same as the primary body, it doesn't work correctly
      // for origin other than Earant. GMAT LOI.Origin = Luna in APT_Ex_LunarTransfer.script
      // so set Earth as origin
      //localCS->SetStringParameter("Origin", localOriginName);
      localCS->SetStringParameter("Origin", j2000BodyName);
      localCS->SetRefObject(orAxes, Gmat::AXIS_SYSTEM, orAxes->GetName());
      localCS->SetRefObject(j2000Body, Gmat::SPACE_POINT, j2000BodyName);
      localCS->SetSolarSystem(solarSystem);
      localCS->Initialize();

      #ifdef DEBUG_ANTENNA_INIT
      MessageInterface::ShowMessage
         ("   Local CS <%p> created with AxisSystem <%p>:\n"
          "      localAxesName = '%s'\n      Origin        = '%s'\n"
          "      Primary       = '%s'\n      Secondary     = '%s'\n"
          "      j2000Body     = '%s'\n", localCS, orAxes, localAxesName.c_str(),
          localCS->GetOriginName().c_str(),
          localCS->GetPrimaryObject() ? localCS->GetPrimaryObject()->GetName().c_str() : "NULL",
          localCS->GetSecondaryObject() ? localCS->GetSecondaryObject()->GetName().c_str() : "NULL",
          localCS->GetJ2000BodyName().c_str());
      #endif

      // Since CoordinateSystem clones AxisSystem, delete it from here
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (orAxes, "localAxes", "Antenna::CreateLocalCoordinateSystem()", "deleting localAxes");
      #endif
   }
   else
   {
      // If not using local cooordinate system, then it is using configured CS and
      // it should have been set by this time
      if (coordSystem)
      {
         throw HardwareException
            ("Unable to initialize the Antenna object " +
             instanceName + " " + coordSystemName + " was not set for the antenna.");
      }
      localCS = coordSystem;
   }

   #ifdef DEBUG_ANTENNA_INIT
   MessageInterface::ShowMessage
      ("Antenna::CreateLocalCoordinateSystem() <%p>'%s' returning <%p>\n",
       this, GetName().c_str(), localCS);
   #endif

   return localCS;
}


//------------------------------------------------------------------------------
// void ConvertDirectionToInertial(Real *dir, Real *dirInertial, Real epoch)
//------------------------------------------------------------------------------
void Antenna::ConvertDirectionToInertial(Real *dir, Real *dirInertial, Real epoch)
{
   if (localCoordSystem == NULL)
   {
      MessageInterface::ShowMessage
         ("Antenna::ConvertDirectionToInertial(), usingLocalCoordSys=%d, "
          "coordSystemName='%s', coordSystem=<%p>'%s'\n", usingLocalCoordSys,
          coordSystemName.c_str(), coordSystem,
          coordSystem ? coordSystem->GetName().c_str() : "NULL");

      throw HardwareException
         ("Unable to convert antenna elements to Inertial, the local Coordinate "
          "System has not been created");
   }

   #ifdef DEBUG_ANTENNA_CONVERT
   MessageInterface::ShowMessage
      ("Antenna::ConvertDirectionToInertial() input dir = %f %f %f\n   "
       "localCoordSystem=<%p>'%s'", dir[0], dir[1], dir[2], localCoordSystem,
       localCoordSystem->GetName().c_str());
   #endif

   Real inDir[6], outDir[6];

   for (Integer i=0; i<6; i++)
      inDir[i] = 0.0;

   inDir[0] = dir[0];
   inDir[1] = dir[1];
   inDir[2] = dir[2];

   localCoordSystem->ToMJ2000Eq(epoch, inDir, outDir);

   dirInertial[0] = outDir[0];
   dirInertial[1] = outDir[1];
   dirInertial[2] = outDir[2];

   #ifdef DEBUG_ANTENNA_CONVERT
   MessageInterface::ShowMessage
      ("Antenna::ConvertDirectionToInertial() returning\n"
       "           direction = %f %f %f\n   dirInertial = %f %f %f\n",
       direction[0], direction[1], direction[2],
       dirInertial[0], dirInertial[1], dirInertial[2]);
   #endif
}


//---------------------------------------------------------------------------
// void ComputeInertialDirection(Real epoch)
//---------------------------------------------------------------------------
void Antenna::ComputeInertialDirection(Real epoch)
{
   ConvertDirectionToInertial(direction, inertialDirection, epoch);
}




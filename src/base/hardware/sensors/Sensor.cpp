//$Header$
//------------------------------------------------------------------------------
//                             Sensor
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
/**
 *
 * Implements Sensor base class to provide for modeling of sensors
 *
 */
//------------------------------------------------------------------------------


#include "Sensor.hpp"
#include "ObjectReferencedAxes.hpp"
#include "MessageInterface.hpp"
#include <sstream>

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
StringArray Sensor::localAxesLabels;

/// Labels used for the sensor element parameters.
const std::string
Sensor::PARAMETER_TEXT[SensorParamCount - HardwareParamCount] =
{
   "IsFiring",
   "CoordinateSystem",
   "Origin",
   "Axes",
   "DutyCycle",
   "ThrustScaleFactor",
   "DecrementMass",
   "Tank",
   "GravitationalAccel",
};

/// Types of the parameters used by sensors.
const Gmat::ParameterType
Sensor::PARAMETER_TYPE[SensorParamCount - HardwareParamCount] =
{
   Gmat::BOOLEAN_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::ENUMERATION_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::REAL_TYPE,
};


//------------------------------------------------------------------------------
//  Sensor(std::string nomme)
//------------------------------------------------------------------------------
/**
 * Sensor constructor.
 *
 * @param nomme Name of the sensor.
 *
 * @Note coordSystem and spacecraft are set through SetRefObject() during
 *       Sandbox initialization. localOrigin and j2000Body are set when
 *       solarSystem is set. localCoordSystem is created during initialization
 *       or when new spacecraft is set
 */
//------------------------------------------------------------------------------
Sensor::Sensor(std::string nomme) :
   Hardware             (Gmat::HARDWARE, "Sensor", nomme),
   solarSystem          (NULL),
   localCoordSystem     (NULL),
   coordSystem          (NULL),
   localOrigin          (NULL),
   j2000Body            (NULL),
   spacecraft           (NULL),
   coordSystemName      ("EarthMJ2000Eq"),
   localOriginName      ("Earth"),
   localAxesName        ("VNB"),
   j2000BodyName        ("Earth"),
   satName              (""),
   usingLocalCoordSys   (true),
   initialized          (false)
{
   objectTypes.push_back(Gmat::SENSOR);
   objectTypeNames.push_back("Sensor");
   parameterCount = SensorParamCount;

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
//  ~Sensor()
//------------------------------------------------------------------------------
/**
 * Sensor destructor.
 */
//------------------------------------------------------------------------------
Sensor::~Sensor()
{
   if (usingLocalCoordSys && localCoordSystem)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (localCoordSystem, "localCS", "Sensor::~Sensor()",
          "deleting localCoordSystem");
      #endif
      delete localCoordSystem;
      localCoordSystem = NULL;
   }
}


//------------------------------------------------------------------------------
//  Sensor(const Sensor& mySensor)
//------------------------------------------------------------------------------
/**
 * Sensor copy constructor.
 *
 * @param mySensor The object being copied.
 *
 * @Note coordSystem and spacecraft are set through SetRefObject() during
 *       Sandbox initialization. localOrigin and j2000Body are set when
 *       solarSystem is set. localCoordSystem is created during initialization
 *       or when new spacecraft is set
 */
//------------------------------------------------------------------------------
Sensor::Sensor(const Sensor& mySensor) :
   Hardware             (mySensor),
   solarSystem          (NULL),
   localCoordSystem     (NULL),
   coordSystem          (NULL),
   localOrigin          (NULL),
   j2000Body            (NULL),
   spacecraft           (NULL),
   coordSystemName      (mySensor.coordSystemName),
   localOriginName      (mySensor.localOriginName),
   localAxesName        (mySensor.localAxesName),
   j2000BodyName        (mySensor.j2000BodyName),
   satName              (mySensor.satName),
   usingLocalCoordSys   (mySensor.usingLocalCoordSys),
   initialized          (false)
{
   parameterCount = mySensor.parameterCount;
   localAxesLabels = mySensor.localAxesLabels;

   inertialDirection[0] = mySensor.inertialDirection[0];
   inertialDirection[1] = mySensor.inertialDirection[1];
   inertialDirection[2] = mySensor.inertialDirection[2];

}


//------------------------------------------------------------------------------
//  Sensor& operator=(const Sensor& mySensor)
//------------------------------------------------------------------------------
/**
 * Sensor assignment operator.
 *
 * @param mySensor The object being copied.
 *
 * @return this object, with parameters set to the input object's parameters.
 *
 * @Note coordSystem and spacecraft are set through SetRefObject() during
 *       Sandbox initialization. localOrigin and j2000Body are set when
 *       solarSystem is set. localCoordSystem is created during initialization
 *       or when new spacecraft is set
 */
//------------------------------------------------------------------------------
Sensor& Sensor::operator=(const Sensor& mySensor)
{
   if (&mySensor == this)
      return *this;

   Hardware::operator=(mySensor);

   solarSystem         = NULL;
   localCoordSystem    = NULL;
   coordSystem         = NULL;
   localOrigin         = NULL;
   j2000Body           = NULL;
   spacecraft          = NULL;

   coordSystemName     = mySensor.coordSystemName;
   localOriginName     = mySensor.localOriginName;
   localAxesName       = mySensor.localAxesName;
   j2000BodyName       = mySensor.j2000BodyName;
   satName             = mySensor.satName;

   inertialDirection[0]  = mySensor.inertialDirection[0];
   inertialDirection[1]  = mySensor.inertialDirection[1];
   inertialDirection[2]  = mySensor.inertialDirection[2];

   usingLocalCoordSys  = mySensor.usingLocalCoordSys;
   initialized         = false;

   localAxesLabels     = mySensor.localAxesLabels;

   return *this;
}


//---------------------------------------------------------------------------
//  GmatBase* Clone() const
//---------------------------------------------------------------------------
/**
 * Provides a clone of this object by calling the copy constructor.
 *
 * @return A GmatBase pointer to the cloned sensor.
 */
//---------------------------------------------------------------------------
GmatBase* Sensor::Clone() const
{
   return new Sensor(*this);
}


//---------------------------------------------------------------------------
//  void Copy(GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 *
 * @param orig The original that is being copied.
 *
 * @return A GmatBase pointer to the cloned sensor.
 */
//---------------------------------------------------------------------------
void Sensor::Copy(const GmatBase* orig)
{
   operator=(*((Sensor *)(orig)));
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
std::string Sensor::GetParameterText(const Integer id) const
{
   if (id >= HardwareParamCount && id < SensorParamCount)
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
Integer Sensor::GetParameterID(const std::string &str) const
{
   for (Integer i = HardwareParamCount; i < SensorParamCount; i++)
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
Gmat::ParameterType Sensor::GetParameterType(const Integer id) const
{
   if (id >= HardwareParamCount && id < SensorParamCount)
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
std::string Sensor::GetParameterTypeString(const Integer id) const
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
bool Sensor::IsParameterReadOnly(const Integer id) const
{

   if ((id == ORIGIN || id == AXES))
      if (coordSystemName != "Local")
         return true;

   return Hardware::IsParameterReadOnly(id);
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
std::string Sensor::GetStringParameter(const Integer id) const
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
bool Sensor::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_SENSOR_SET
   MessageInterface::ShowMessage
      ("Sensor::SetStringParameter() '%s' entered, id=%d, value='%s'\n",
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
const StringArray& Sensor::GetStringArrayParameter(const Integer id) const
{
   if (id == AXES)
      return localAxesLabels;

   return Hardware::GetStringArrayParameter(id);
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
bool Sensor::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
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
      //spacecraft = (Spacecraft*)obj;
      //return true;
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
ObjectArray& Sensor::GetRefObjectArray(const Gmat::ObjectType type)
{
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
ObjectArray& Sensor::GetRefObjectArray(const std::string& typeString)
{

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
void Sensor::SetSolarSystem(SolarSystem *ss)
{
   #ifdef DEBUG_SENSOR_SET
   MessageInterface::ShowMessage
      ("Sensor::SetSolarSystem() ss=<%p> '%s'\n", ss, ss->GetName().c_str());
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
bool Sensor::Initialize()
{
   #ifdef DEBUG_SENSOR_INIT
   MessageInterface::ShowMessage
      ("Sensor::Initialize() <%p>'%s' entered, spacecraft=<%p>\n", this,
       GetName().c_str(), spacecraft);
   #endif

   bool retval = Hardware::Initialize();

   if (retval)
   {
      if (usingLocalCoordSys)
      {
         if (solarSystem == NULL)
            throw HardwareException
               ("Unable to initialize the Sensor object " +
                instanceName + " " + "\"SolarSystem\" was not set for the sensor.");

         if ((!localOrigin) || (!j2000Body))
            throw HardwareException
               ("Unable to initialize the sensor object " +
                instanceName + "; either \"" + j2000BodyName + "\" or \"" +
                localOriginName + "\" was not set for the sensor.");
      }
   }

   // delete old local coordinate system
   if (usingLocalCoordSys && localCoordSystem != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (localCoordSystem, "localCS", "Sensor::Initialize()",
          "deleting localCoordSystem");
      #endif
      delete localCoordSystem;
      localCoordSystem = NULL;
   }

   // If spacecraft is available, create new local coordinate system
   if (usingLocalCoordSys && spacecraft != NULL)
      localCoordSystem = CreateLocalCoordinateSystem();

   if (localCoordSystem == NULL)
   {
      #ifdef DEBUG_SENSOR_INIT
      MessageInterface::ShowMessage
         ("Sensor::Initialize() <%p>'%s' returning false, localCoordSystem is NULL\n",
          this, GetName().c_str());
      #endif
      return false;
   }

   // Convert direction to inertial coord system
   Real epoch = spacecraft->GetRealParameter("A1Epoch");
   ConvertDirectionToInertial(direction, inertialDirection, epoch);

   #ifdef DEBUG_SENSOR_INIT
   MessageInterface::ShowMessage
      ("   Inertial thrust direction:  %18le  %18le  %18le\n",
       inertialDirection[0], inertialDirection[1], inertialDirection[2]);
   MessageInterface::ShowMessage
      ("   %s tank mass computation\n", decrementMass ? "Continue with " : "Skipping");
   #endif

   #ifdef DEBUG_SENSOR_INIT
   MessageInterface::ShowMessage
      ("Sensor::Initialize() <%p>'%s' returning false\n", this, GetName().c_str());
   #endif

   return retval;
}

//------------------------------------------------------------------------------
//  bool SetSpacecraft(Spacecraft *sat)
//------------------------------------------------------------------------------
/**
 * Accessor method to pass in the spacecraft pointer. This method is usually
 * called during the Sandbox initialization when building Spacecraft owned
 * objects such as tanks and sensors. The spacecraft passes itself to this
 * class instance using Sensor::SetRefObject() in Spacecraft::SetRefObject().
 *
 * This method creates new local coordinate system if current spacecraft is not
 * the same as old spacecraft.
 *
 * @param <sat> the Spacecraft
 */
//------------------------------------------------------------------------------
bool Sensor::SetSpacecraft(Spacecraft *sat)
{
   if (sat == NULL)
      return false;

   #ifdef DEBUG_SENSOR_SET
   MessageInterface::ShowMessage
      ("Sensor::SetSpacecraft() sat=<%p>'%s', usingLocalCoordSys=%d, "
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
               (localCoordSystem, "localCS", "Sensor::SetSpacecraft()",
                "deleting localCoordSystem");
            #endif
            delete localCoordSystem;
            localCoordSystem = NULL;
         }
         localCoordSystem = CreateLocalCoordinateSystem();
      }
   }

   #ifdef DEBUG_SENSOR_SET
   MessageInterface::ShowMessage("Sensor::SetSpacecraft() returning true\n");
   #endif

   return true;
}


//------------------------------------------------------------------------------
// CoordinateSystem* CreateLocalCoordinateSystem()
//------------------------------------------------------------------------------
CoordinateSystem* Sensor::CreateLocalCoordinateSystem()
{
   #ifdef DEBUG_SENSOR_INIT
   MessageInterface::ShowMessage
      ("Sensor::CreateLocalCoordinateSystem() <%p>'%s' entered, usingLocalCoordSys=%d, "
       "spacecraft=<%p>\n", this, GetName().c_str(), usingLocalCoordSys, spacecraft);
   #endif

   CoordinateSystem *localCS = NULL;

   // If coordinate system being used is local, then create
   if (usingLocalCoordSys)
   {
      if (solarSystem == NULL)
      {
         // Since SolarSystem may be set later, just return NULL for now
         #ifdef DEBUG_SENSOR_INIT
         MessageInterface::ShowMessage
            ("Sensor::CreateLocalCoordinateSystem() SolarSystem is not set so, "
             "returning NULL\n");
         #endif
         return NULL;

         //throw HardwareException
         //   ("Unable to initialize the Sensor object " +
         //    instanceName + " " + "\"SolarSystem\" was not set for the sensor.");
      }

      if (spacecraft == NULL)
      {
         // Since spacecraft may be set later, just return NULL for now
         #ifdef DEBUG_SENSOR_INIT
         MessageInterface::ShowMessage
            ("Sensor::CreateLocalCoordinateSystem() spacecraft is not set so, "
             "returning NULL\n");
         #endif
         return NULL;

         //throw HardwareException("Unable to initialize the Sensor object " +
         //   instanceName + " " + satName + " was not set for the sensor.");
      }

      satName = spacecraft->GetName();

      localCS = new CoordinateSystem("Local");
      AxisSystem *orAxes = new ObjectReferencedAxes("Local");
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (localCS, "localCS", "Sensor::CreateLocalCoordinateSystem()",
          "new CoordinateSystem()");
      MemoryTracker::Instance()->Add
         (orAxes, "localAxes", "Sensor::CreateLocalCoordinateSystem()",
          "new ObjectReferencedAxes()");
      #endif
      orAxes->SetStringParameter("Primary", localOriginName);
      orAxes->SetStringParameter("Secondary", satName);
      orAxes->SetRefObject(localOrigin, Gmat::SPACE_POINT, localOriginName);
      orAxes->SetRefObject(spacecraft, Gmat::SPACE_POINT, satName);
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
         throw HardwareException("Sensor cannot use SpacecraftBody yet.");
      }

      // If I set origin same as the primary body, it doesn't work correctly
      // for origin other than EarmySensor. GMAT LOI.Origin = Luna in APT_Ex_LunarTransfer.script
      // so set Earth as origin
      //localCS->SetStringParameter("Origin", localOriginName);
      localCS->SetStringParameter("Origin", j2000BodyName);
      localCS->SetRefObject(orAxes, Gmat::AXIS_SYSTEM, orAxes->GetName());
      localCS->SetRefObject(j2000Body, Gmat::SPACE_POINT, j2000BodyName);
      localCS->SetSolarSystem(solarSystem);
      localCS->Initialize();

      #ifdef DEBUG_SENSOR_INIT
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
         (orAxes, "localAxes", "Sensor::CreateLocalCoordinateSystem()", "deleting localAxes");
      #endif
   }
   else
   {
      // If not using local cooordinate system, then it is using configured CS and
      // it should have been set by this time
      if (coordSystem)
      {
         throw HardwareException
            ("Unable to initialize the Sensor object " +
             instanceName + " " + coordSystemName + " was not set for the sensor.");
      }
      localCS = coordSystem;
   }

   #ifdef DEBUG_SENSOR_INIT
   MessageInterface::ShowMessage
      ("Sensor::CreateLocalCoordinateSystem() <%p>'%s' returning <%p>\n",
       this, GetName().c_str(), localCS);
   #endif

   return localCS;
}


//------------------------------------------------------------------------------
// void ConvertDirectionToInertial(Real *dir, Real *dirInertial, Real epoch)
//------------------------------------------------------------------------------
void Sensor::ConvertDirectionToInertial(Real *dir, Real *dirInertial, Real epoch)
{
   if (localCoordSystem == NULL)
   {
      MessageInterface::ShowMessage
         ("Sensor::ConvertDirectionToInertial(), usingLocalCoordSys=%d, "
          "coordSystemName='%s', coordSystem=<%p>'%s'\n", usingLocalCoordSys,
          coordSystemName.c_str(), coordSystem,
          coordSystem ? coordSystem->GetName().c_str() : "NULL");

      throw HardwareException
         ("Unable to convert sensor elements to Inertial, the local Coordinate "
          "System has not been created");
   }

   #ifdef DEBUG_SENSOR_CONVERT
   MessageInterface::ShowMessage
      ("Sensor::ConvertDirectionToInertial() input dir = %f %f %f\n   "
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

   #ifdef DEBUG_SENSOR_CONVERT
   MessageInterface::ShowMessage
      ("Sensor::ConvertDirectionToInertial() returning\n"
       "           direction = %f %f %f\n   dirInertial = %f %f %f\n",
       direction[0], direction[1], direction[2],
       dirInertial[0], dirInertial[1], dirInertial[2]);
   #endif
}


//---------------------------------------------------------------------------
// void ComputeInertialDirection(Real epoch)
//---------------------------------------------------------------------------
void Sensor::ComputeInertialDirection(Real epoch)
{
   ConvertDirectionToInertial(direction, inertialDirection, epoch);
}




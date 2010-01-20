//$Id$
//------------------------------------------------------------------------------
//                               Hardware
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2004/11/08
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under MOMS Task
// Order 124.
//
/**
 * Class implementation for the Hardware base class.
 */
//------------------------------------------------------------------------------


#include "Hardware.hpp"
#include "ObjectReferencedAxes.hpp"
#include "Spacecraft.hpp"
#include "HardwareException.hpp"
#include "MessageInterface.hpp"
#include <string.h>

//#define DEBUG_HARDWARE
//#define DEBUG_HARDWARE_SET
//#define DEBUG_HARDWARE_INIT
//#define DEBUG_HARDWARE_CONVERT

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
StringArray Hardware::localAxesLabels;

/// Labels used for the hardware element parameters.
const std::string
Hardware::PARAMETER_TEXT[HardwareParamCount - GmatBaseParamCount] =
{
   "X_Direction",
   "Y_Direction", 
   "Z_Direction",
   "CoordinateSystem",
   "Origin",
   "Axes"
};


/// Types of the parameters used by all hardware elements.
const Gmat::ParameterType
Hardware::PARAMETER_TYPE[HardwareParamCount - GmatBaseParamCount] =
{
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::ENUMERATION_TYPE
};



//------------------------------------------------------------------------------
//  Hardware(Gmat::ObjectType typeId, const std::string &typeStr, 
//           const std::string &nomme)
//------------------------------------------------------------------------------
/**
 * Hardware base class constructor.
 *
 * @param typeId Core object type for the component.
 * @param typeStr String label for the actual object type for the component.
 * @param nomme Name of the component.
 */
//------------------------------------------------------------------------------
Hardware::Hardware(Gmat::ObjectType typeId, const std::string &typeStr, 
                   const std::string &nomme) :
   GmatBase(typeId, typeStr, nomme),
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
   isMJ2000EqAxes       (false),
   isSpacecraftBodyAxes (false)
{
   objectTypes.push_back(Gmat::HARDWARE);
   objectTypeNames.push_back("Hardware");

   inertialDirection[0] = 1.0;
   inertialDirection[1] = 0.0;
   inertialDirection[2] = 0.0;

   direction[0] = 1.0;
   direction[1] = 0.0;
   direction[2] = 0.0;
   
   secondDirection[0] = 0.0;
   secondDirection[1] = 1.0;
   secondDirection[2] = 0.0;

   location[0] = 0.0;
   location[1] = 0.0;
   location[2] = 0.0;

   // Local axes labels
   localAxesLabels.push_back("VNB");
   localAxesLabels.push_back("LVLH");
   localAxesLabels.push_back("MJ2000Eq");
   localAxesLabels.push_back("SpacecraftBody");

   // set parameter write order
   for (Integer i=HardwareParamCount; i <= AXES; i++)
      parameterWriteOrder.push_back(i);

   parameterWriteOrder.push_back(DIRECTION_X);
   parameterWriteOrder.push_back(DIRECTION_Y);
   parameterWriteOrder.push_back(DIRECTION_Z);

}


//------------------------------------------------------------------------------
//  ~Hardware()
//------------------------------------------------------------------------------
/**
 * Hardware base class destructor.
 */
//------------------------------------------------------------------------------
Hardware::~Hardware()
{
   if (usingLocalCoordSys && localCoordSystem)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (localCoordSystem, "localCS", "Hardware::~Hardware()",
          "deleting localCoordSystem");
      #endif
      delete localCoordSystem;
      localCoordSystem = NULL;
   }
}


//------------------------------------------------------------------------------
//  Hardware(const Hardware& hw)
//------------------------------------------------------------------------------
/**
 * Hardware copy constructor.
 * 
 * @param hw The object being copied.
 */
//------------------------------------------------------------------------------
Hardware::Hardware(const Hardware& hw) :
   GmatBase(hw),
   solarSystem          (NULL),
   localCoordSystem     (NULL),
   coordSystem          (NULL),
   localOrigin          (NULL),
   j2000Body            (NULL),
   spacecraft           (NULL),
   coordSystemName      (hw.coordSystemName),
   localOriginName      (hw.localOriginName),
   localAxesName        (hw.localAxesName),
   j2000BodyName        (hw.j2000BodyName),
   satName              (hw.satName),
   usingLocalCoordSys   (hw.usingLocalCoordSys),
   isMJ2000EqAxes       (hw.isMJ2000EqAxes),
   isSpacecraftBodyAxes (hw.isSpacecraftBodyAxes)
{
   direction[0] = hw.direction[0];
   direction[1] = hw.direction[1];
   direction[2] = hw.direction[2];

   localAxesLabels = hw.localAxesLabels;

   inertialDirection[0] = hw.inertialDirection[0];
   inertialDirection[1] = hw.inertialDirection[1];
   inertialDirection[2] = hw.inertialDirection[2];
      
   secondDirection[0] = hw.secondDirection[0];
   secondDirection[1] = hw.secondDirection[1];
   secondDirection[2] = hw.secondDirection[2];

   location[0] = hw.location[0];
   location[1] = hw.location[1];
   location[2] = hw.location[2];
}


//------------------------------------------------------------------------------
//  Hardware& operator=(const Hardware& hw)
//------------------------------------------------------------------------------
/**
 * Hardware assignment operator.
 * 
 * @param hw The object being copied.
 * 
 * @return this object, with parameters set to the input object's parameters.
 */
//------------------------------------------------------------------------------
Hardware& Hardware::operator=(const Hardware& hw)
{
   if (&hw == this)
      return *this;
   
   GmatBase::operator=(hw);

   solarSystem         = NULL;
   localCoordSystem    = NULL;
   coordSystem         = NULL;
   localOrigin         = NULL;
   j2000Body           = NULL;
   spacecraft          = NULL;

   coordSystemName     = hw.coordSystemName;
   localOriginName     = hw.localOriginName;
   localAxesName       = hw.localAxesName;
   j2000BodyName       = hw.j2000BodyName;
   satName             = hw.satName;

   direction[0] = hw.direction[0];
   direction[1] = hw.direction[1];
   direction[2] = hw.direction[2];
   
   secondDirection[0] = hw.secondDirection[0];
   secondDirection[1] = hw.secondDirection[1];
   secondDirection[2] = hw.secondDirection[2];
   
   location[0] = hw.location[0];
   location[1] = hw.location[1];
   location[2] = hw.location[2];

   inertialDirection[0]  = hw.inertialDirection[0];
   inertialDirection[1]  = hw.inertialDirection[1];
   inertialDirection[2]  = hw.inertialDirection[2];

   localAxesLabels     = hw.localAxesLabels;
   
   return *this;
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
 *
 */
//------------------------------------------------------------------------------
std::string Hardware::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < HardwareParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   return GmatBase::GetParameterText(id);
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
Integer Hardware::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < HardwareParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }
   
   return GmatBase::GetParameterID(str);
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
 *
 */
//------------------------------------------------------------------------------
Gmat::ParameterType Hardware::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < HardwareParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];
      
   return GmatBase::GetParameterType(id);
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
 *
 */
//------------------------------------------------------------------------------
std::string Hardware::GetParameterTypeString(const Integer id) const
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
bool Hardware::IsParameterReadOnly(const Integer id) const
{
   if ((id == ORIGIN || id == AXES))
      if (coordSystemName != "Local")
         return true;

   return Hardware::IsParameterReadOnly(id);
}


//------------------------------------------------------------------------------
//  const StringArray& GetPropertyEnumStrings(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Access an array of enumerated string data.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The requested StringArray; throws if the parameter is not a
 *         StringArray.
 */
//------------------------------------------------------------------------------
const StringArray& Hardware::GetPropertyEnumStrings(const Integer id) const
{
   if (id == AXES)
      return localAxesLabels;

   return GmatBase::GetPropertyEnumStrings(id);
}


//------------------------------------------------------------------------------
//  const StringArray& GetPropertyEnumStrings(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Access an array of enumerated string data.
 *
 * @param <label> The parameter name.
 *
 * @return The requested StringArray
 */
//------------------------------------------------------------------------------
const StringArray& Hardware::GetPropertyEnumStrings(const std::string &label) const
{
   return GetPropertyEnumStrings(GetParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetRefObjectName(const Gmat::ObjectType type) const
//------------------------------------------------------------------------------
std::string Hardware::GetRefObjectName(const Gmat::ObjectType type) const
{
   #ifdef DEBUG_THRUSTER_REF_OBJ
   MessageInterface::ShowMessage
      ("Hardware::GetRefObjectName() <%p>'%s' entered, type=%d\n", this,
       GetName().c_str(), type);
   #endif

   std::string refObjName;
   if (type == Gmat::COORDINATE_SYSTEM)
   {
      std::string refObjName;
      if (!usingLocalCoordSys)
         refObjName = coordSystemName;

      #ifdef DEBUG_THRUSTER_REF_OBJ
      MessageInterface::ShowMessage
         ("Hardware::GetRefObjectName() <%p>'%s' returning '%s'\n", this,
          GetName().c_str(), refObjName.c_str());
      #endif

      return refObjName;
   }

   return Hardware::GetRefObjectName(type);
}


//------------------------------------------------------------------------------
// const ObjectTypeArray& GetRefObjectTypeArray()
//------------------------------------------------------------------------------
/**
 * Retrieves the list of ref object types used by this class.
 *
 * @return the list of object types.
 *
 */
//------------------------------------------------------------------------------
const ObjectTypeArray& Hardware::GetRefObjectTypeArray()
{
   refObjectTypes.clear();
   refObjectTypes.push_back(Gmat::COORDINATE_SYSTEM);
   refObjectTypes.push_back(Gmat::CELESTIAL_BODY);
   refObjectTypes.push_back(Gmat::SPACECRAFT);
   return refObjectTypes;
}


//------------------------------------------------------------------------------
// virtual const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
const StringArray& Hardware::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   #ifdef DEBUG_THRUSTER_REF_OBJ
   MessageInterface::ShowMessage
      ("Hardware::GetRefObjectNameArray() <%p>'%s' entered, type=%d\n",
       this, GetName().c_str(), type);
   #endif

   refObjectNames.clear();
   if (type == Gmat::UNKNOWN_OBJECT || type == Gmat::COORDINATE_SYSTEM)
   {
      if (!usingLocalCoordSys)
         refObjectNames.push_back(coordSystemName);
   }

   if (type == Gmat::UNKNOWN_OBJECT || type == Gmat::CELESTIAL_BODY)
   {
      if (usingLocalCoordSys)
      {
         refObjectNames.push_back(j2000BodyName);
         if (localOriginName != j2000BodyName)
            refObjectNames.push_back(localOriginName);
      }
   }

   if (type == Gmat::UNKNOWN_OBJECT || type == Gmat::SPACECRAFT)
   {
      if (satName != "")
         refObjectNames.push_back(satName);
   }

   #ifdef DEBUG_THRUSTER_REF_OBJ
   MessageInterface::ShowMessage
      ("Hardware::GetRefObjectNameArray(), refObjectNames.size()=%d\n",
       refObjectNames.size());
   for (UnsignedInt i=0; i<refObjectNames.size(); i++)
      MessageInterface::ShowMessage("   '%s'\n", refObjectNames[i].c_str());
   #endif

   return refObjectNames;
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
Real Hardware::GetRealParameter(const Integer id) const
{
   switch (id) {
      case DIRECTION_X:
         return direction[0];
         
      case DIRECTION_Y:
         return direction[1];
         
      case DIRECTION_Z:
         return direction[2];
         
      default:
         break;   // Default just drops through
   }
   return GmatBase::GetRealParameter(id);
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
Real Hardware::SetRealParameter(const Integer id, const Real value)
{
   switch (id) {
      case DIRECTION_X:
         return direction[0] = value;
         
      case DIRECTION_Y:
         return direction[1] = value;
         
      case DIRECTION_Z:
//         if (value < 0) {
//            throw HardwareException("Z_Direction must be >= 0"); 
//         }
         return direction[2] = value;
      default:
         break;   // Default just drops through
   }

   return GmatBase::SetRealParameter(id, value);
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
std::string Hardware::GetStringParameter(const Integer id) const
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
bool Hardware::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_HARDWARE_SET
   MessageInterface::ShowMessage
      ("Hardware::SetStringParameter() '%s' entered, id=%d, value='%s'\n",
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
const StringArray& Hardware::GetStringArrayParameter(const Integer id) const
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
bool Hardware::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
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

   return GmatBase::SetRefObject(obj, type, name);
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
ObjectArray& Hardware::GetRefObjectArray(const Gmat::ObjectType type)
{
   return GmatBase::GetRefObjectArray(type);
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
ObjectArray& Hardware::GetRefObjectArray(const std::string& typeString)
{
   return GmatBase::GetRefObjectArray(typeString);
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
void Hardware::SetSolarSystem(SolarSystem *ss)
{
   #ifdef DEBUG_HARDWARE_SET
   MessageInterface::ShowMessage
      ("Hardware::SetSolarSystem() ss=<%p> '%s'\n", ss, ss->GetName().c_str());
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
//  bool SetSpacecraft(Spacecraft *sat)
//------------------------------------------------------------------------------
/**
 * Accessor method to pass in the spacecraft pointer. This method is usually
 * called during the Sandbox initialization when building Spacecraft owned
 * objects such as tanks and thrusters. The spacecraft passes itself to this
 * class instance using Hardware::SetRefObject() in Spacecraft::SetRefObject().
 *
 * This method creates new local coordinate system if current spacecraft is not
 * the same as old spacecraft.
 *
 * @param <sat> the Spacecraft
 */
//------------------------------------------------------------------------------
bool Hardware::SetSpacecraft(Spacecraft *sat)
{
   if (sat == NULL)
      return false;

   #ifdef DEBUG_THRUSTER_REF_OBJ
   MessageInterface::ShowMessage
      ("Hardware::SetSpacecraft() sat=<%p>'%s', usingLocalCoordSys=%d, "
       "localCoordSystem=<%p>\n", sat, sat->GetName().c_str(), usingLocalCoordSys,
       localCoordSystem);
   #endif

   // If spcecraft is different
   if (spacecraft != sat)
   {
      spacecraft = sat;
      satName = spacecraft->GetName();

      // create new local coordinate system
      if (usingLocalCoordSys)
      {
         if (localCoordSystem)
         {
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (localCoordSystem, "localCS", "Hardware::SetSpacecraft()",
                "deleting localCoordSystem");
            #endif
            delete localCoordSystem;
            localCoordSystem = NULL;
         }
         localCoordSystem = CreateLocalCoordinateSystem();

         #ifdef DEBUG_THRUSTER_REF_OBJ
         MessageInterface::ShowMessage
            ("   new localCoordSystem <%p> created\n", localCoordSystem);
         #endif
      }
   }

   #ifdef DEBUG_THRUSTER_REF_OBJ
   MessageInterface::ShowMessage("Hardware::SetSpacecraft() returning true\n");
   #endif

   return true;
}


//------------------------------------------------------------------------------
// CoordinateSystem* CreateLocalCoordinateSystem()
//------------------------------------------------------------------------------
CoordinateSystem* Hardware::CreateLocalCoordinateSystem()
{
   #ifdef DEBUG_THRUSTER_INIT
   MessageInterface::ShowMessage
      ("Hardware::CreateLocalCoordinateSystem() <%p>'%s' entered, usingLocalCoordSys=%d, "
       "spacecraft=<%p>\n", this, GetName().c_str(), usingLocalCoordSys, spacecraft);
   #endif

   CoordinateSystem *localCS = NULL;

   // If coordinate system being used is local, then create
   if (usingLocalCoordSys)
   {
      if (solarSystem == NULL)
      {
         // Since SolarSystem may be set later, just return NULL for now
         #ifdef DEBUG_THRUSTER_INIT
         MessageInterface::ShowMessage
            ("Hardware::CreateLocalCoordinateSystem() SolarSystem is not set so, "
             "returning NULL\n");
         #endif
         return NULL;

         //throw HardwareException
         //   ("Unable to initialize the Thruster object " +
         //    instanceName + " " + "\"SolarSystem\" was not set for the thruster.");
      }

      if (spacecraft == NULL)
      {
         // Since spacecraft may be set later, just return NULL for now
         #ifdef DEBUG_THRUSTER_INIT
         MessageInterface::ShowMessage
            ("Hardware::CreateLocalCoordinateSystem() spacecraft is not set so, "
             "returning NULL\n");
         #endif
         return NULL;

         //throw HardwareException("Unable to initialize the Thruster object " +
         //   instanceName + " " + satName + " was not set for the thruster.");
      }

      // Call CoordinateSystem static method to create a local coordinate system
      localOrigin = solarSystem->GetBody(localOriginName);
      localCS = CoordinateSystem::CreateLocalCoordinateSystem
         ("Local", localAxesName, spacecraft, localOrigin, spacecraft,
          j2000Body, solarSystem);

      if (localCS == NULL)
         return NULL;

      if (localAxesName == "MJ2000Eq")
         isMJ2000EqAxes = true;
      else if (localAxesName == "SpacecraftBody")
         isSpacecraftBodyAxes = true;

   }
   else
   {
      // If not using local cooordinate system, then it is using configured CS and
      // it should have been set by this time
      if (coordSystem == NULL)
      {
         throw HardwareException
            ("Unable to initialize the Thruster object " +
             instanceName + " " + coordSystemName + " was not set for the thruster.");
      }
      localCS = coordSystem;
   }

   #ifdef DEBUG_THRUSTER_INIT
   MessageInterface::ShowMessage
      ("Hardware::CreateLocalCoordinateSystem() <%p>'%s' returning <%p>\n",
       this, GetName().c_str(), localCS);
   #endif

   return localCS;
}


//------------------------------------------------------------------------------
// void ConvertDirectionToInertial(Real *dir, Real *dirInertial, Real epoch)
//------------------------------------------------------------------------------
/*
 * Converts thruster direction to inertial frame
 *
 * @param dir  Thruster direction in thruster frame
 * @param dirInertial  Thruster direction in inertial frame
 * @param epoch  Epoch to be used for conversion
 */
//------------------------------------------------------------------------------
void Hardware::ConvertDirectionToInertial(Real *dir, Real *dirInertial, Real epoch)
{
   #ifdef DEBUG_THRUSTER_CONVERT
   MessageInterface::ShowMessage
      ("Hardware::ConvertDirectionToInertial() <%p>'%s' entered, epoch=%.15f\n   "
       "dir=%.15f %.15f %.15f\n", this, GetName().c_str(), epoch, dir[0], dir[1], dir[1]);
   MessageInterface::ShowMessage
      ("   usingLocalCoordSys=%d, coordSystemName='%s', coordSystem=<%p>, "
       "localCoordSystem=<%p>\n", usingLocalCoordSys, coordSystemName.c_str(),
       coordSystem, localCoordSystem);
   #endif

   if (usingLocalCoordSys && localCoordSystem == NULL)
   {
      // try creating localCoordSystem
      //localCoordSystem = CreateLocalCoordinateSystem();
      // try Initialize again (LOJ: 2009.07.28)
      Initialize();
      if (!localCoordSystem)
         throw HardwareException
            ("Unable to convert thrust direction to Inertial, the local Coordinate "
             "System has not been created.");
   }
   else if (!usingLocalCoordSys && coordSystem == NULL)
   {
      throw HardwareException
         ("Unable to convert thrust direction to Inertial, the Coordinate "
          "System \"" + coordSystemName + "\" has not been set.");
   }

   Real inDir[6], outDir[6];
   for (Integer i=0; i<3; i++)
      inDir[i] = dir[i];
   for (Integer i=3; i<6; i++)
      inDir[i] = 0.0;

   // if not using local CS, use ref CoordinateSystem
   if (!usingLocalCoordSys)
   {
      // Now rotate to MJ2000Eq axes, we don't want to translate so
      // set coincident to true
      coordSystem->ToMJ2000Eq(epoch, inDir, outDir, true);

      #ifdef DEBUG_BURN_CONVERT_ROTMAT
      Rmatrix33 rotMat = coordSystem->GetLastRotationMatrix();
      MessageInterface::ShowMessage
         ("rotMat=\n%s\n", rotMat.ToString(16, 20).c_str());
      #endif

      dirInertial[0] = outDir[0];
      dirInertial[1] = outDir[1];
      dirInertial[2] = outDir[2];
   }
   else
   {
      // if MJ2000Eq axes rotation matrix is alway identity matrix
      if (isMJ2000EqAxes)
      {
         dirInertial[0] = dir[0];
         dirInertial[1] = dir[1];
         dirInertial[2] = dir[2];
      }
      else if (isSpacecraftBodyAxes)
      {
         Rvector3 inDir(dir[0], dir[1], dir[2]);
         Rvector3 outDir;
         // Get attitude matrix from Spacecraft and transpose since
         // attitide matrix from spacecraft gives rotation matrix from
         // inertial to body
         Rmatrix33 inertialToBody = spacecraft->GetAttitude(epoch);
         Rmatrix33 rotMat = inertialToBody.Transpose();
         outDir = inDir * rotMat;
         for (Integer i=0; i<3; i++)
            dirInertial[i] = outDir[i];
      }
      else
      {
         // Now rotate to MJ2000Eq axes
         localCoordSystem->ToMJ2000Eq(epoch, inDir, outDir, true);

         dirInertial[0] = outDir[0];
         dirInertial[1] = outDir[1];
         dirInertial[2] = outDir[2];
      }
   }

   #ifdef DEBUG_THRUSTER_CONVERT
   MessageInterface::ShowMessage
      ("Hardware::ConvertDirectionToInertial() returning\n   "
       "  direction = %.15f %.15f %.15f\n   "
       "dirInertial = %.15f %.15f %.15f\n",
       direction[0], direction[1], direction[2],
       dirInertial[0], dirInertial[1], dirInertial[2]);
   #endif
}


//---------------------------------------------------------------------------
// void ComputeInertialDirection(Real epoch)
//---------------------------------------------------------------------------
void Hardware::ComputeInertialDirection(Real epoch)
{
   ConvertDirectionToInertial(direction, inertialDirection, epoch);
}


//------------------------------------------------------------------------------
// void WriteDeprecatedMessage(const std::string &oldProp,
//                             const std::string &newProp) const
//------------------------------------------------------------------------------
void Hardware::WriteDeprecatedMessage(const std::string &oldProp,
                                      const std::string &newProp) const
{
   // Write only one message per session
   static bool writeXDirection = true;
   static bool writeYDirection = true;
   static bool writeZDirection = true;
   static bool writeElement1 = true;
   static bool writeElement2 = true;
   static bool writeElement3 = true;

   if ((oldProp == "X_Direction" && writeXDirection) ||
       (oldProp == "Y_Direction" && writeYDirection) ||
       (oldProp == "Z_Direction" && writeZDirection) ||
       (oldProp == "Element1" && writeElement1) ||
       (oldProp == "Element2" && writeElement2) ||
       (oldProp == "Element3" && writeElement3))
   {
      MessageInterface::ShowMessage
         ("*** WARNING *** \"" + oldProp + "\" of Thruster orientation is "
          "deprecated and will be removed from a future build; please use \"" +
          newProp + "\" instead.\n");
   }

   if (oldProp == "X_Direction")
      writeXDirection = false;
   else if (oldProp == "Y_Direction")
      writeYDirection = false;
   else if (oldProp == "Z_Direction")
      writeYDirection = false;
   else if (oldProp == "Element1")
      writeElement1 = false;
   else if (oldProp == "Element2")
      writeElement2 = false;
   else if (oldProp == "Element3")
      writeElement3 = false;

}

//------------------------------------------------------------------------------
//  bool Initialize()
//------------------------------------------------------------------------------
/**
 * Sets up the bodies used in the thrust calculations.
 */
//------------------------------------------------------------------------------
bool Hardware::Initialize()
{
    #ifdef DEBUG_HARDWARE_INIT
    MessageInterface::ShowMessage
       ("Hardware::Initialize() <%p>'%s' entered, spacecraft=<%p>\n", this,
       GetName().c_str(), spacecraft);
    #endif

   bool retval = GmatBase::Initialize();

    if (usingLocalCoordSys)
    {
        if (solarSystem == NULL)
            throw HardwareException
                ("Unable to initialize the hardware object " +
                instanceName + " " + "\"SolarSystem\" was not set for the hardware.");

        if ((!localOrigin) || (!j2000Body))
            throw HardwareException
                ("Unable to initialize the hardware object " +
                instanceName + "; either \"" + j2000BodyName + "\" or \"" +
                localOriginName + "\" was not set for the hardware.");
    }


    // delete old local coordinate system
    if (usingLocalCoordSys && localCoordSystem != NULL)
    {
        #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
                (localCoordSystem, "localCS", "Hardware::Initialize()",
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
      #ifdef DEBUG_HARDWARE_INIT
      MessageInterface::ShowMessage
         ("Hardware::Initialize() <%p>'%s' returning false, localCoordSystem is NULL\n",
          this, GetName().c_str());
      #endif
      return false;
   }

   // Convert direction to inertial coord system
   Real epoch = spacecraft->GetRealParameter("A1Epoch");
   ConvertDirectionToInertial(direction, inertialDirection, epoch);

   #ifdef DEBUG_HARDWARE_INIT
   MessageInterface::ShowMessage
      ("   Inertial thrust direction:  %18le  %18le  %18le\n",
       inertialDirection[0], inertialDirection[1], inertialDirection[2]);
   MessageInterface::ShowMessage
      ("   %s tank mass computation\n", decrementMass ? "Continue with " : "Skipping");
   #endif

   #ifdef DEBUG_HARDWARE_INIT
   MessageInterface::ShowMessage
      ("Hardware::Initialize() <%p>'%s' returning false\n", this, GetName().c_str());
   #endif

   return retval;
}


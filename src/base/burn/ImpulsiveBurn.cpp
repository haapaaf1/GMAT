//$Id$
//------------------------------------------------------------------------------
//                              ImpulsiveBurn
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2003/12/17
//
/**
 * Defines the ImpulsiveBurn class used for maneuvers. 
 */
//------------------------------------------------------------------------------


#include "ImpulsiveBurn.hpp"
#include "BurnException.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_IMPBURN_INIT
//#define DEBUG_IMPBURN_SET
//#define DEBUG_IMPBURN_FIRE
//#define DEBUG_IMPBURN_DECMASS

//---------------------------------
// static data
//---------------------------------

/// Labels used for the parameters.
const std::string
ImpulsiveBurn::PARAMETER_TEXT[ImpulsiveBurnParamCount - BurnParamCount] =
{
   "DecrementMass",
   "Tank",
   "Isp",
   "GravitationalAccel",
   "DeltaTankMass",
};

/// Types of the parameters
const Gmat::ParameterType
ImpulsiveBurn::PARAMETER_TYPE[ImpulsiveBurnParamCount - BurnParamCount] =
{
   Gmat::BOOLEAN_TYPE,
   Gmat::OBJECTARRAY_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
};

//------------------------------------------------------------------------------
//  ImpulsiveBurn(const std::string &nomme)
//------------------------------------------------------------------------------
/**
 * Constructs the impulsive burn (default constructor).
 * 
 * @param <nomme> Name for the object
 */
//------------------------------------------------------------------------------
ImpulsiveBurn::ImpulsiveBurn(const std::string &nomme) :
   Burn(Gmat::IMPULSIVE_BURN, "ImpulsiveBurn", nomme),
   isp                      (300.0),
   gravityAccel             (9.81),
   deltaTankMass            (0),
   totalTankMass            (0),
   decrementMass            (false),
   constantExpressions      (true),  // used?
   simpleExpressions        (true)   // used?
{
   objectTypes.push_back(Gmat::IMPULSIVE_BURN);
   objectTypeNames.push_back("ImpulsiveBurn");
   parameterCount = ImpulsiveBurnParamCount;
}


//------------------------------------------------------------------------------
//  ImpulsiveBurn()
//------------------------------------------------------------------------------
/**
 * Destroys the impulsive burn (destructor).
 */
//------------------------------------------------------------------------------
ImpulsiveBurn::~ImpulsiveBurn()
{
}


//------------------------------------------------------------------------------
//  ImpulsiveBurn(const ImpulsiveBurn &ib)
//------------------------------------------------------------------------------
/**
 * Constructs the impulsive burn from the input burn (ib constructor).
 *
 * @param <ib> ImpulsiveBurn object to copy
 */
//------------------------------------------------------------------------------
ImpulsiveBurn::ImpulsiveBurn(const ImpulsiveBurn &ib) :
   Burn                 (ib),
   isp                  (ib.isp),
   gravityAccel         (ib.gravityAccel),
   deltaTankMass        (ib.deltaTankMass),
   totalTankMass        (ib.totalTankMass),
   decrementMass        (ib.decrementMass),
   constantExpressions  (ib.constantExpressions),
   simpleExpressions    (ib.simpleExpressions),
   tankNames            (ib.tankNames),
   tankMap              (ib.tankMap)
{
}

//------------------------------------------------------------------------------
//  ImpulsiveBurn& operator=(const ImpulsiveBurn &ib)
//------------------------------------------------------------------------------
/**
 * Assignment operator for impulsive burns.
 *
 * @param <ib> ImpulsiveBurn object to copy
 *
 * @return this instance, configured like the imput instance.
 */
//------------------------------------------------------------------------------
ImpulsiveBurn& ImpulsiveBurn::operator=(const ImpulsiveBurn &ib)
{
   if (this == &ib)
      return *this;
   
   Burn::operator=(ib);
   
   isp                 = ib.isp;
   gravityAccel        = ib.gravityAccel;
   totalTankMass       = ib.totalTankMass;
   deltaTankMass       = ib.deltaTankMass;
   decrementMass       = ib.decrementMass;
   constantExpressions = ib.constantExpressions;
   simpleExpressions   = ib.simpleExpressions;
   tankNames           = ib.tankNames;
   tankMap             = ib.tankMap;
   return *this;
}


//------------------------------------------------------------------------------
//  void SetSpacecraftToManeuver(Spacecraft *sat)
//------------------------------------------------------------------------------
/**
 * Accessor method used by Maneuver to pass in the spacecraft pointer
 * 
 * @param <sat> the Spacecraft
 */
//------------------------------------------------------------------------------
void ImpulsiveBurn::SetSpacecraftToManeuver(Spacecraft *sat)
{
   #ifdef DEBUG_IMPBURN_SET
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::SetSpacecraftToManeuver() sat=<%p>'%s', spacecraft=<%p>'%s'\n",
       sat, sat->GetName().c_str(), spacecraft,
       spacecraft ? spacecraft->GetName().c_str() : "NULL");
   #endif
   
   if (sat == NULL)
      return;
   
   Burn::SetSpacecraftToManeuver(sat);
   
   // Associate tank of the spacecraft
   if (decrementMass && spacecraft != sat)
      SetTankFromSpacecraft();
   
   #ifdef DEBUG_IMPBURN_SET
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::SetSpacecraftToManeuver() returning\n");
   #endif
}


//------------------------------------------------------------------------------
//  bool Fire(Real *burnData, Real epoch)
//------------------------------------------------------------------------------
/**
 * Applies the burn.  
 * 
 * Provides the mathematics that model an impulsive burn.  The parameter 
 * (burnData) can be used to pass in a Cartesian state (x, y, z, Vx, Vy, Vz) 
 * that needs to incorporate the burn.  If the class has an assigned Spacecraft,
 * that spacecraft is used instead of the input state.
 * 
 * @param <burnData>    Array of data specific to the derived burn class.
 * @param <epoch>       Epoch of the burn fire
 *
 * @return true on success, throws on failure.
 */
//------------------------------------------------------------------------------
bool ImpulsiveBurn::Fire(Real *burnData, Real epoch)
{
   #ifdef DEBUG_IMPBURN_FIRE
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::Fire() <%p>'%s' entered\n", this, instanceName.c_str());
   MessageInterface::ShowMessage
      ("   deltaV: %18le  %18le  %18le\n", deltaV[0], deltaV[1], deltaV[2]);
   #endif
   
   #ifdef DEBUG_IMPBURN_FIRE
   MessageInterface::ShowMessage
      ("   usingLocalCoordSys=%d, spacecraft=<%p>, initialized=%d, "
       "localCoordSystem=<%p>\n", usingLocalCoordSys, spacecraft, initialized,
       localCoordSystem);
   #endif
   
   // By this time, the spacecraft should have been set
   if (usingLocalCoordSys && spacecraft == NULL)
      throw BurnException
         ("Unable to initialize the ImpulsiveBurn object " + 
          instanceName + " " + satName + " was not set for the burn.");
   
   if (!initialized  || localCoordSystem == NULL)
   {
      if (Initialize())
         initialized = true;
   }
   
   if (epoch == 21545.0)
      epoch = spacecraft->GetRealParameter("A1Epoch");
   
   Real *satState = spacecraft->GetState().GetState();
   
   #ifdef DEBUG_IMPBURN_FIRE
   MessageInterface::ShowMessage
      ("   Maneuvering spacecraft %s\n", spacecraft->GetName().c_str());
   MessageInterface::ShowMessage
      ("   Position for burn:    %18le  %18le  %18le\n",
       satState[0], satState[1], satState[2]);
   MessageInterface::ShowMessage
      ("   Velocity before burn: %18le  %18le  %18le\n",
       satState[3], satState[4], satState[5]);
   #endif
   
   ConvertDeltaVToInertial(deltaV, deltaVInertial, epoch);
   
   satState[3] += deltaVInertial[0];
   satState[4] += deltaVInertial[1];
   satState[5] += deltaVInertial[2];
      
   #ifdef DEBUG_IMPBURN_FIRE
   MessageInterface::ShowMessage
      ("   Velocity after burn:  %18le  %18le  %18le\n",
       satState[3], satState[4], satState[5]);
   MessageInterface::ShowMessage
      ("   %s tank mass computation\n", decrementMass ? "Continue with " : "Skipping");
   #endif
   
   if (decrementMass)
      DecrementMass();
      
   #ifdef DEBUG_IMPBURN_FIRE
   MessageInterface::ShowMessage("ImpulsiveBurn::Fire() returning true\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes ImpulsiveBurn variables
 */
//------------------------------------------------------------------------------
bool ImpulsiveBurn::Initialize()
{
   #ifdef DEBUG_IMPBURN_INIT
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::Initialize() '%s' entered, localCoordSystem=<%p>, "
       "decrementMass=%d, tankNames.size()=%d\n", GetName().c_str(),
       localCoordSystem, decrementMass, tankNames.size());
   #endif
   
   if (!Burn::Initialize())
   {
      #ifdef DEBUG_IMPBURN_INIT
      MessageInterface::ShowMessage
         ("ImpulsiveBurn::Initialize() '%s' returning false\n", GetName().c_str());
      #endif
      return false;
   }
   
   bool retval = false;
   
   if (decrementMass && !tankNames.empty())
      retval = SetTankFromSpacecraft();
   
   if (localCoordSystem == NULL)
      retval = retval | false;
   
   #ifdef DEBUG_IMPBURN_INIT
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::Initialize() '%s' returning %d\n", GetName().c_str(), retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// void TransformDeltaVToJ2kFrame(Real *deltaV, Real epoch)
//------------------------------------------------------------------------------
void ImpulsiveBurn::TransformDeltaVToJ2kFrame(Real *deltaV, Real epoch)
{
   if ((j2000Body == NULL) || (localOrigin == NULL))
      Initialize();
      
   if (j2000Body != localOrigin)
   {
      Rvector6 j2kState = j2000Body->GetMJ2000State(epoch);
      Rvector6 originState = localOrigin->GetMJ2000State(epoch);
      Rvector6 delta = j2kState - originState;
      
      deltaV[0] += delta[3];
      deltaV[1] += delta[4];
      deltaV[2] += delta[5];
   }
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ImpulsiveBurn.
 *
 * @return clone of the ImpulsiveBurn.
 *
 */
//------------------------------------------------------------------------------
GmatBase* ImpulsiveBurn::Clone() const
{
   return (new ImpulsiveBurn(*this));
}


//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void ImpulsiveBurn::Copy(const GmatBase* orig)
{
   operator=(*((ImpulsiveBurn *)(orig)));
}


//---------------------------------------------------------------------------
//  bool TakeAction(const std::string &action, const std::string &actionData)
//---------------------------------------------------------------------------
/**
 * Interface used to support user actions.
 *
 * ImpulsiveBurns use this method to clear the pointers and names of tanks used for
 * a burn, prior to reassigning the tanks.
 *
 * @param <action> The string descriptor for the requested action.
 * @param <actionData> Optional data used for the action.
 *
 * @return true if the action was performed, false if not.
 */
//---------------------------------------------------------------------------
bool ImpulsiveBurn::TakeAction(const std::string &action,
                               const std::string &actionData)
{
   if (action == "ClearTanks")
   {
      tankNames.clear();
      tankMap.clear();
      return true;
   }
   
   return Burn::TakeAction(action, actionData);
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
bool ImpulsiveBurn::IsParameterReadOnly(const Integer id) const
{
   if (id == DELTA_TANK_MASS)
      return true;
   
   return Burn::IsParameterReadOnly(id);
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
std::string ImpulsiveBurn::GetParameterText(const Integer id) const
{
   if (id >= BurnParamCount && id < ImpulsiveBurnParamCount)
      return PARAMETER_TEXT[id - BurnParamCount];
   
   return Burn::GetParameterText(id);
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
Integer ImpulsiveBurn::GetParameterID(const std::string &str) const
{
   for (Integer i = BurnParamCount; i < ImpulsiveBurnParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - BurnParamCount])
         return i;
   }
   
   return Burn::GetParameterID(str);
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
Gmat::ParameterType ImpulsiveBurn::GetParameterType(const Integer id) const
{
   if (id >= BurnParamCount && id < ImpulsiveBurnParamCount)
      return PARAMETER_TYPE[id - BurnParamCount];
      
   return Burn::GetParameterType(id);
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
bool ImpulsiveBurn::GetBooleanParameter(const Integer id) const
{
   if (id == DECREMENT_MASS)
      return decrementMass;
   
   return Burn::GetBooleanParameter(id);
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
bool ImpulsiveBurn::SetBooleanParameter(const Integer id, const bool value)
{
   if (id == DECREMENT_MASS)
   {
      decrementMass = value;
      return decrementMass;
   }
   
   return Burn::SetBooleanParameter(id, value);
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
Real ImpulsiveBurn::GetRealParameter(const Integer id) const
{
   switch (id)
   {
      case ISP:
         return isp;
      case GRAVITATIONAL_ACCELERATION:
         return gravityAccel;
      case DELTA_TANK_MASS:
         return deltaTankMass;
      default:
         break;   // Default just drops through
   }
   
   return Burn::GetRealParameter(id);
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
Real ImpulsiveBurn::SetRealParameter(const Integer id, const Real value)
{
   switch (id)
   {
      // Isp Coefficients
      case ISP:
         isp = value;
         return isp;
      case GRAVITATIONAL_ACCELERATION:
         gravityAccel = value;
         return gravityAccel;
      case DELTA_TANK_MASS:
         deltaTankMass = value;
         return deltaTankMass;
      default:
         break;   // Default just drops through
   }
   
   return Burn::SetRealParameter(id, value);
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
std::string ImpulsiveBurn::GetStringParameter(const Integer id) const
{
   return Burn::GetStringParameter(id);
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
bool ImpulsiveBurn::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_IMPBURN_SET
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::SetStringParameter() id=%d, value='%s'\n", id, value.c_str());
   #endif
   switch (id)
   {
   case FUEL_TANK:
      if (find(tankNames.begin(), tankNames.end(), value) == tankNames.end())
      {
         #ifdef DEBUG_IMPBURN_SET
         MessageInterface::ShowMessage
            ("   Adding FuelTank '%s' to the list\n", value.c_str());
         #endif
         
         tankNames.push_back(value);
         initialized = false;
      }
      return true;
   default:
      return Burn::SetStringParameter(id, value);
   }
   
   return Burn::SetStringParameter(id, value);
}


//---------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string &value,
//                          const Integer index)
//---------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool ImpulsiveBurn::SetStringParameter(const Integer id, const std::string &value,
                                       const Integer index)
{
   switch (id)
   {
   case FUEL_TANK:
      {
         if (index < (Integer)tankNames.size())
            tankNames[index] = value;
         else
            // Add the tank only if it is not in the list already
            if (find(tankNames.begin(), tankNames.end(), value) == tankNames.end()) 
               tankNames.push_back(value);
         
         return true;
      }
   default:
      return Burn::SetStringParameter(id, value, index);
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
const StringArray& ImpulsiveBurn::GetStringArrayParameter(const Integer id) const
{
   if (id == FUEL_TANK)
      return tankNames;
   
   return Burn::GetStringArrayParameter(id);
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
const ObjectTypeArray& ImpulsiveBurn::GetRefObjectTypeArray()
{
   Burn::GetRefObjectTypeArray();
   refObjectTypes.push_back(Gmat::FUEL_TANK);
   return refObjectTypes;
}


//------------------------------------------------------------------------------
// virtual const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
const StringArray& ImpulsiveBurn::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   Burn::GetRefObjectNameArray(type);
   if (type == Gmat::UNKNOWN_OBJECT || type == Gmat::FUEL_TANK)
      refObjectNames.insert(refObjectNames.begin(),
                            tankNames.begin(), tankNames.end());
   
   #ifdef DEBUG_IMPBURN_GET
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::GetRefObjectNameArray(), refObjectNames.size()=%d\n",
       refObjectNames.size());
   for (UnsignedInt i=0; i<refObjectNames.size(); i++)
      MessageInterface::ShowMessage("   '%s'\n", refObjectNames[i].c_str());
   #endif
   
   return refObjectNames;
}


//------------------------------------------------------------------------------
//  bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                    const std::string &name)
//------------------------------------------------------------------------------
/**
* This method sets a reference object for the CoordinateSystem class.
 *
 * @param obj   pointer to the reference object
 * @param type  type of the reference object 
 * @param name  name of the reference object
 *
 * @return true if successful; otherwise, false.
 *
 */
//------------------------------------------------------------------------------
bool ImpulsiveBurn::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                 const std::string &name)
{
   #ifdef DEBUG_BURN_SET
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::SetRefObject() this=<%p> '%s', objType=%d, objTypeName=%s, "
       "objName=%s, type=%d, name=%s\n", this, GetName().c_str(), obj->GetType(),
       obj->GetTypeName().c_str(), obj->GetName().c_str(), type, name.c_str());
   #endif
   
   switch (type)
   {
   case Gmat::FUEL_TANK:
      {
         if (tankMap.find(name) != tankMap.end())
            tankMap[name] = obj;
         return true;
      }
   default:
      return Burn::SetRefObject(obj, type, name);
   }
}


//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
/**
 * Renames reference object name used in this class.
 *
 * @param <type> reference object type.
 * @param <oldName> object name to be renamed.
 * @param <newName> new object name.
 * 
 * @return true if object name changed, false if not.
 */
//---------------------------------------------------------------------------
bool ImpulsiveBurn::RenameRefObject(const Gmat::ObjectType type,
                                    const std::string &oldName,
                                    const std::string &newName)
{
   if (type == Gmat::FUEL_TANK)
   {
      StringArray::iterator pos;
      pos == find(tankNames.begin(), tankNames.end(), oldName);
      if (pos != tankNames.end())
         *pos = newName;
   }
   
   return Burn::RenameRefObject(type, oldName, newName);
}


//------------------------------------------------------------------------------
// bool SetTankFromSpacecraft()
//------------------------------------------------------------------------------
bool ImpulsiveBurn::SetTankFromSpacecraft()
{
   #ifdef DEBUG_IMPBURN_SET
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::SetTankFromSpacecraft() entered, spacecraft=<%p>'%s'\n",
       spacecraft, spacecraft ? spacecraft->GetName().c_str() : "NULL");
   MessageInterface::ShowMessage("   tankNames.size()=%d\n", tankNames.size());
   #endif
   
   if (spacecraft == NULL)
      return false;
   
   if (tankNames.empty())
      throw BurnException("ImpulsiveBurn::Initialize() " + instanceName +
                          " has no associated tank");
   
   ObjectArray tankArray = spacecraft->GetRefObjectArray(Gmat::FUEL_TANK);
   
   #ifdef DEBUG_IMPBURN_SET
   MessageInterface::ShowMessage
      ("   spacecraft tankArray.size()=%d\n", tankArray.size());
   #endif
   
   if (!tankNames.empty() && !tankArray.empty())
   {
      ObjectArray::iterator scTank = tankArray.begin();
      
      // Find the tank on the spacecraft
      for (StringArray::iterator tankName = tankNames.begin();
           tankName != tankNames.end(); ++tankName)
      {
         while (scTank != tankArray.end())
         {
            #ifdef DEBUG_IMPBURN_SET
            MessageInterface::ShowMessage
               ("   The tank '%s' associated with spacecraft is <%p>'%s'\n",
                (*tankName).c_str(), (*scTank),
                (*scTank) ? (*scTank)->GetName().c_str() : "NULL");
            #endif
            
            // Just in case, check for NULL tank pointer
            if (*scTank == NULL)
               continue;
            
            // Assign the tank
            if ((*scTank)->GetName() == *tankName)
            {
               tankMap[*tankName] = (*scTank);
               #ifdef DEBUG_IMPBURN_SET
               MessageInterface::ShowMessage
                  ("   Assigned <%p>'%s' to tankMap\n", *scTank, (*tankName).c_str());
               #endif
            }
            ++scTank;
         }
      }
   }
   
   #ifdef DEBUG_IMPBURN_SET
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::SetTankFromSpacecraft() returning true\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// void DecrementMass()
//------------------------------------------------------------------------------
void ImpulsiveBurn::DecrementMass()
{
   #ifdef DEBUG_IMPBURN_DECMASS
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::DecrementMass() <%p>'%s' entered. There are %d tank(s)\n",
       this, instanceName.c_str(), tankMap.size());
   #endif
   
   totalTankMass = spacecraft->GetRealParameter("TotalMass");
   
   #ifdef DEBUG_IMPBURN_DECMASS
   MessageInterface::ShowMessage
      ("   Now decrementing mass\n      before maneuver totalTankMass = %f\n",
       totalTankMass);
   #endif
   
   Real dv = sqrt( deltaV[0]*deltaV[0] + deltaV[1]*deltaV[1] + deltaV[2]*deltaV[2]);
   deltaTankMass = totalTankMass * (exp(-dv * 1000/(isp * gravityAccel)) - 1.0);
   
   #ifdef DEBUG_IMPBURN_DECMASS
   MessageInterface::ShowMessage
      ("       after maneuver deltaTankMass = %f\n", deltaTankMass);
   #endif
   
   totalTankMass = totalTankMass + deltaTankMass;
   
   #ifdef DEBUG_IMPBURN_DECMASS
   MessageInterface::ShowMessage
      ("       after maneuver totalTankMass = %f\n", totalTankMass);
   #endif
   
   // Update tank mass
   if (!tankMap.empty())
   {
      for (ObjectMap::iterator tankPos = tankMap.begin();
           tankPos != tankMap.end(); ++tankPos)
      {
         GmatBase *currTank = tankPos->second;
         #ifdef DEBUG_IMPBURN_DECMASS
         MessageInterface::ShowMessage
            ("       Decrementing tank mass for <%p>'%s'\n", currTank,
             (tankPos->first).c_str());
         #endif
         Integer paramID = currTank->GetParameterID("FuelMass");
         Real oldTankMass = currTank->GetRealParameter(paramID);
         Real currTankMass = oldTankMass + deltaTankMass;
         #ifdef DEBUG_IMPBURN_DECMASS
         MessageInterface::ShowMessage
            ("       it was %f, it is now %f\n", oldTankMass, currTankMass);
         #endif
         //@todo What should we do if decremented tank mass is below zero?
         currTank->SetRealParameter(paramID, currTankMass);
      }
   }
   
   #ifdef DEBUG_IMPBURN_DECMASS
   MessageInterface::ShowMessage
      ("ImpulsiveBurn::DecrementMass() <%p>'%s' returning\n", this, GetName().c_str());
   #endif
}
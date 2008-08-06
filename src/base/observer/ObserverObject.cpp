//$Header$
//------------------------------------------------------------------------------
//                              ObserverObject
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
//
/**
 * Implements the base class used for Ground Station objects. 
 */
//------------------------------------------------------------------------------


#include "ObserverObject.hpp"
#include "MessageInterface.hpp"


// #define DEBUG_J2000_STATE 1
// #define DEBUG_STOPCONDITION_TRACKING

//---------------------------------
// static data
//---------------------------------

const std::string ObserverObject::PARAMETER_TEXT[ObserverObjectParamCount -
                                              SpacePointParamCount] =
   {
      "A1Epoch"
   };


const Gmat::ParameterType ObserverObject::PARAMETER_TYPE[ObserverObjectParamCount - 
                                                      SpacePointParamCount] =
   {
      Gmat::REAL_TYPE
   };


//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// ObserverObject(Gmat::ObjectType typeId, const std::string &typeStr,
//             const std::string &instName)
//------------------------------------------------------------------------------
/**
 * Default constructor.
 *
 * @param <typeId>   Gmat::ObjectType of the constructed object.
 * @param <typeStr>  String describing the type of object created.
 * @param <instName> Name of the constructed instance.
 */
//------------------------------------------------------------------------------
ObserverObject::ObserverObject(Gmat::ObjectType typeId, const std::string &typeStr,
                         const std::string &instName) :
   SpacePoint        (typeId, typeStr, instName),
   isSpaceBased      (false),
   isGroundBased     (false),
   isShipBased       (false),
   isAirplaneBased   (false),
   isManeuvering     (false),
   originName        ("Earth"),
   origin            (NULL),
   parmsChanged      (true)
{
   objectTypes.push_back(Gmat::OBSERVEROBJECT);
   objectTypeNames.push_back("ObserverObject");
}


//------------------------------------------------------------------------------
// ~ObserverObject()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
ObserverObject::~ObserverObject()
{
}


//------------------------------------------------------------------------------
// ObserverObject(const ObserverObject& so)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * @param <so> ObserverObject that is copied onto this one.
 */
//------------------------------------------------------------------------------
ObserverObject::ObserverObject(const ObserverObject& oo) :
   SpacePoint        (oo),
   state             (oo.state),
   originName        (oo.originName),
   origin            (oo.origin),
   isSpaceBased      (oo.isSpaceBased),
   isGroundBased     (oo.isGroundBased),
   isShipBased       (oo.isShipBased),
   isAirplaneBased   (oo.isAirplaneBased),
   isManeuvering     (oo.isManeuvering),
   parmsChanged      (oo.parmsChanged),
   //lastStopTriggered (so.lastStopTriggered)
{
   j2000Body = oo.j2000Body;
}


//------------------------------------------------------------------------------
// ObserverObject& operator=(const ObserverObject& so)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 *
 * @param <so> ObserverObject that is copied onto this one.
 *
 * @return this instance, configured like the input instance.
 */
//------------------------------------------------------------------------------
ObserverObject& ObserverObject::operator=(const ObserverObject& oo)
{
   if (this == &oo)
      return *this;
      
   SpacePoint::operator=(oo);
   state         = oo.state;
   originName    = oo.originName;
   origin        = oo.origin;
   parmsChanged  = true;       // Always update after using assignment
   //lastStopTriggered = so.lastStopTriggered;
   return *this;
}


//------------------------------------------------------------------------------
// PropState& GetState()
//------------------------------------------------------------------------------
/**
 * Accessor for the PropState of the object.
 *
 * @return The embedded PropState.
 */
//------------------------------------------------------------------------------
PropState& ObserverObject::GetState()
{
   return state;
}


//------------------------------------------------------------------------------
// Real GetEpoch()
//------------------------------------------------------------------------------
/**
 * Accessor for the current epoch of the object, in A.1 Modified Julian format.
 *
 * @return The A.1 epoch.
 *
 * @todo The epoch probably should be TAI throughout GMAT.
 */
//------------------------------------------------------------------------------
Real ObserverObject::GetEpoch()
{
   return state.GetEpoch();
}


//------------------------------------------------------------------------------
// Real SetEpoch(const Real ep)
//------------------------------------------------------------------------------
/**
 * Accessor used to set epoch (in A.1 Modified Julian format) of the object.
 *
 * @param <ep> The new A.1 epoch.
 *
 * @return The updated A.1 epoch.
 *
 * @todo The epoch probably should be TAI throughout GMAT.
 */
//------------------------------------------------------------------------------
Real ObserverObject::SetEpoch(const Real ep)
{
   return state.SetEpoch(ep);
}

//------------------------------------------------------------------------------
// bool ParametersHaveChanged()
//------------------------------------------------------------------------------
/**
 * Function used to test the parmsChanged flag.
 * 
 * @return The value of the flag.
 */
//------------------------------------------------------------------------------
bool ObserverObject::ParametersHaveChanged()
{
   return parmsChanged;
}

//------------------------------------------------------------------------------
// void ParametersHaveChanged(bool flag)
//------------------------------------------------------------------------------
/**
 * Uset to set or clear the parmsChanged flag from outside of the ObserverObject.
 * 
 * @param <flag>  The new value for the flag.
 */
//------------------------------------------------------------------------------
void ObserverObject::ParametersHaveChanged(bool flag)
{
   parmsChanged = flag;
}


/// @todo Waiting for CoordinateSystems in Spacecraft, then see if needed
void ObserverObject::SetOriginName(std::string cbName)
{
   originName = cbName;
}

const std::string ObserverObject::GetOriginName()
{
   return originName;
}


void ObserverObject::SetOrigin(SpacePoint *cb)
{
   origin = cb;
}



//------------------------------------------------------------------------------
// const Rvector6 GetMJ2000State(const A1Mjd &atTime)
//------------------------------------------------------------------------------
/**
 * Access the MJ2000 state for this ObserverObject.
 *
 * @param <atTime> Epoch for the state data.
 *
 * @return The Cartesian MJ2000 state.
 *
 * @todo Determine if this calculation should ber moved into the derived
 *       classes.
 */
//------------------------------------------------------------------------------
const Rvector6 ObserverObject::GetMJ2000State(const A1Mjd &atTime)
{
   #ifdef DEBUG_J2000_STATE
      MessageInterface::ShowMessage(
         "ObserverObject::GetMJ2000State entered; epoch is %lf\n", atTime.Get());
   #endif
   if (j2000Body == NULL)
      throw ObserverObjectException(
         "ObserverObject::GetMJ2000State MJ2000 body not yet set for " +
         instanceName + "\n");
         
   PropState ps = GetState();
   
   Real *st = ps.GetState();

   #ifdef DEBUG_J2000_STATE
      MessageInterface::ShowMessage("   Accessing J2000 body state for %s\n",
         j2000Body->GetName().c_str());
   #endif
   Rvector6 bodyState = j2000Body->GetMJ2000State(atTime);

   #ifdef DEBUG_J2000_STATE
      MessageInterface::ShowMessage("   MJ2000: [%lf %lf %lf %lf %lf %lf]\n",
         bodyState[0], bodyState[1], bodyState[2], bodyState[3], bodyState[4], 
         bodyState[5]);
   #endif

   // If origin is NULL, assume it is set at the J2000 origin.
   if (origin)
   {
      #ifdef DEBUG_J2000_STATE
         MessageInterface::ShowMessage("   Accessing origin state for %s\n",
            origin->GetName().c_str());
      #endif
      
      Rvector6 offset = origin->GetMJ2000State(atTime);
      
      #ifdef DEBUG_J2000_STATE
         MessageInterface::ShowMessage("   origin: [%lf %lf %lf %lf %lf %lf]\n",
            offset[0], offset[1], offset[2], offset[3], offset[4], offset[5]);
      #endif
      
      bodyState -= offset;
      
      #ifdef DEBUG_J2000_STATE
         MessageInterface::ShowMessage("   Diff: [%lf %lf %lf %lf %lf %lf]\n",
            bodyState[0], bodyState[1], bodyState[2], bodyState[3], bodyState[4], 
            bodyState[5]);
      #endif
   }
   
   Rvector6 j2kState;
   
   j2kState[0] = st[0] - bodyState[0];
   j2kState[1] = st[1] - bodyState[1];
   j2kState[2] = st[2] - bodyState[2];

   j2kState[3] = st[3] - bodyState[3];
   j2kState[4] = st[4] - bodyState[4];
   j2kState[5] = st[5] - bodyState[5];

   return j2kState;
}


//------------------------------------------------------------------------------
// const Rvector3 GetMJ2000Position(const A1Mjd &atTime)
//------------------------------------------------------------------------------
/**
 * Access the MJ2000 position for this ObserverObject.
 *
 * @param <atTime> Epoch for the state data.
 *
 * @return The Cartesian MJ2000 position.
 *
 * @todo Implement GetMJ2000Position in the derived classes, and remove this
 *       implementation.
 */
//------------------------------------------------------------------------------
const Rvector3 ObserverObject::GetMJ2000Position(const A1Mjd &atTime)
{
   const Rvector6 rv6 = GetMJ2000State(atTime);
   return (rv6.GetR()); 
}


//------------------------------------------------------------------------------
// const Rvector3 GetMJ2000Velocity(const A1Mjd &atTime)
//------------------------------------------------------------------------------
/**
 * Access the MJ2000 velocity for this ObserverObject.
 *
 * @param <atTime> Epoch for the state data.
 *
 * @return The Cartesian MJ2000 velocity.
 *
 * @todo Implement GetMJ2000Velocity in the derived classes, and remove this
 *       implementation.
 */
//------------------------------------------------------------------------------
const Rvector3 ObserverObject::GetMJ2000Velocity(const A1Mjd &atTime)
{
   const Rvector6 rv6 = GetMJ2000State(atTime);
   return (rv6.GetV());
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
 */
//------------------------------------------------------------------------------
Integer ObserverObject::GetParameterID(const std::string &str) const
{
   for (Integer i = SpacePointParamCount; i < ObserverObjectParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - SpacePointParamCount])
         return i;
   }
   
   return SpacePoint::GetParameterID(str);
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string ObserverObject::GetParameterText(const Integer id) const
{
   if (id >= SpacePointParamCount && id < ObserverObjectParamCount)
      return PARAMETER_TEXT[id - SpacePointParamCount];
   return SpacePoint::GetParameterText(id);
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
Gmat::ParameterType ObserverObject::GetParameterType(const Integer id) const
{
   if (id >= SpacePointParamCount && id < ObserverObjectParamCount)
      return PARAMETER_TYPE[id - SpacePointParamCount];
   
   return SpacePoint::GetParameterType(id);
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
std::string ObserverObject::GetParameterTypeString(const Integer id) const
{
   return SpacePoint::PARAM_TYPE_STRING[GetParameterType(id)];
}


//------------------------------------------------------------------------------
//  Real  GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the Real parameter value, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter value.
 *
 * @return Real value of the requested parameter.
 */
//------------------------------------------------------------------------------
Real ObserverObject::GetRealParameter(const Integer id) const
{
   if (id == EPOCH_PARAM)
      return state.GetEpoch();
   return SpacePoint::GetRealParameter(id);
}


//------------------------------------------------------------------------------
//  Real  GetRealParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * This method returns the Real parameter value, given the input parameter ID.
 *
 * @param <label> String description for the requested parameter value.
 *
 * @return Real value of the requested parameter.
 */
//------------------------------------------------------------------------------
Real ObserverObject::GetRealParameter(const std::string &label) const
{
   if (label == "A1Epoch")
      return state.GetEpoch();
   return GetRealParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
//  Real  SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * This method sets the Real parameter value, given the input parameter ID.
 *
 * @param <id>    ID for the parameter whose value to change.
 * @param <value> value for the parameter.
 *
 * @return Real value of the requested parameter.
 */
//------------------------------------------------------------------------------
Real ObserverObject::SetRealParameter(const Integer id, const Real value)
{
   if (id == EPOCH_PARAM)
      return state.SetEpoch(value);
   return SpacePoint::GetRealParameter(id);
}


//------------------------------------------------------------------------------
//  Real  SetRealParameter(const std::string &label, const Real value)
//------------------------------------------------------------------------------
/**
 * This method sets the Real parameter value, given the input parameter ID.
 *
 * @param <label> String description for the parameter value.
 * @param <value> value for the parameter.
 *
 * @return Real value of the requested parameter.
 */
//------------------------------------------------------------------------------
Real ObserverObject::SetRealParameter(const std::string &label, const Real value)
{
   return SetRealParameter(GetParameterID(label), value);
}



//------------------------------------------------------------------------------
// void ClearLastStopTriggered()
//------------------------------------------------------------------------------
/*
 * Clears the names of the last stopping conditions that triggered a stop.
 */
//------------------------------------------------------------------------------
// void ObserverObject::ClearLastStopTriggered()
//{
//   lastStopTriggered.clear();
//
//   #ifdef DEBUG_STOPCONDITION_TRACKING
//      MessageInterface::ShowMessage("Cleared stop identifier from \"%s\"\n", 
//         instanceName.c_str());
//   #endif
//}


//------------------------------------------------------------------------------
// void SetLastStopTriggered(const std::string &stopCondName)
//------------------------------------------------------------------------------
/*
 * Adds name of a triggered stopping condition to the list of stops triggered.
 *
 * @param  stopCondName  The name of the triggering stopping condition.
 *
//------------------------------------------------------------------------------
//void ObserverObject::SetLastStopTriggered(const std::string &stopCondName)
//{
//   lastStopTriggered.push_back(stopCondName);
//   
//   #ifdef DEBUG_STOPCONDITION_TRACKING
//      MessageInterface::ShowMessage("Set stop identifier on \"%s\" to \"%s\"\n", 
//         instanceName.c_str(), stopCondName.c_str());
//   #endif
//}


//------------------------------------------------------------------------------
// bool WasLastStopTriggered(const std::string &stopCondName)
//------------------------------------------------------------------------------
/*
 * Compares the name of the last stopping condition that triggered with the 
 * input stopping condition name.
 *
 * @param  stopCondName  The name of the stopping condition being compared.
 * 
 * @return true if the names match, false otherwise.
 *
//------------------------------------------------------------------------------
//bool ObserverObject::WasLastStopTriggered(const std::string &stopCondName)
//{
//   #ifdef DEBUG_STOPCONDITION_TRACKING
//      MessageInterface::ShowMessage(
//         "Checking to see if triggered stop \"%s\" on \"%s\" is in the last "
//         "stop triggered list\n", lastStopTriggered.c_str(), 
//         instanceName.c_str());
//   #endif
//
//   if (find(lastStopTriggered.begin(), lastStopTriggered.end(), stopCondName) != 
//         lastStopTriggered.end())
//      return true;
//   return false;
//}

//---------------------------------------------------------------------------
//  bool IsGroundBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is ground based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsGroundBased() const
{  
   return isGroundBased;
}

//---------------------------------------------------------------------------
//  bool IsGroundBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is ground based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsGroundBased(bool groundFlag) const
{
  isGroundBased = groundFlag;
}

//---------------------------------------------------------------------------
//  bool IsSpaceBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is space based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsSpaceBased() const
{
   return isSpaceBased;
}

//---------------------------------------------------------------------------
//  bool IsSpaceBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is space based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsSpaceBased(bool spaceFlag) const
{
  isSpaceBased = spaceFlag;
}

//---------------------------------------------------------------------------
//  bool IsAirplaneBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is airplane based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsAirplaneBased() const
{
   return isAirplaneBased;
}

//---------------------------------------------------------------------------
//  bool IsAirplaneBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is airplane based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsAirplaneBased(bool airplaneFlag) const
{
  isAirplaneBased = airplaneFlag;
}

//---------------------------------------------------------------------------
//  bool IsShipBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is ship based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsShipBased() const
{
   return isShipBased;
}

//---------------------------------------------------------------------------
//  bool IsShipBased(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested Observer Object is ship based.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ObserverObject::IsShipBased(bool shipFlag) const
{
  isShipBased = shipFlag;
}

//------------------------------------------------------------------------------
// bool IsManeuvering()
//------------------------------------------------------------------------------
/**
 * Function that checks to see if a finite burn needs to be applied to this 
 * object.
 *
 * @return true if a finite burn is active, false otherwise.
 */
//------------------------------------------------------------------------------
bool SpaceObject::IsManeuvering()
{
   return isManeuvering;
}


//------------------------------------------------------------------------------
// void IsManeuvering(bool mnvrFlag)
//------------------------------------------------------------------------------
/**
 * Function that sets or clears the maneuvering flag.
 * 
 * Derived classes may override this method so that the flag is updated based on
 * the state of the hardware attached to the SpaceObjects.
 * 
 * @param <mnvrFlag> The new value for the flag.
 */
//------------------------------------------------------------------------------
void SpaceObject::IsManeuvering(bool mnvrFlag)
{
  if (isSpaceBased)
   isManeuvering = mnvrFlag;
}

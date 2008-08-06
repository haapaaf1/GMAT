//$Id$
//------------------------------------------------------------------------------
//                                  GroundStation 
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
// 
/**
 * Implements the Ground Station base class.
 *    Ground Station internal state is in EarthMJ2000Eq Cartesian.
 *    Internal time is in A1ModJulian.
 *
 *    It converts to proper format using epochType, stateType, anomalyType
 *    before generating scripts from the internal data.
 */ 
//------------------------------------------------------------------------------

#include <sstream>
#include "GroundStation.hpp"
#include "MessageInterface.hpp"
#include "GroundStationObjectException.hpp"
#include "StringUtil.hpp"

// Do we want to write anomaly type?
//#define __WRITE_ANOMALY_TYPE__


//#define DEBUG_GROUNDSTATION 1 
//#define DEBUG_GROUNDSTATION_SET 1 
//#define DEBUG_GROUNDSTATION_CS 1 
//#define DEBUG_RENAME 1
//#define DEBUG_DATE_FORMAT
//#define DEBUG_STATE_INTERFACE
//#define DEBUG_GS_ATTITUDE
//#define DEBUG_GET_REAL
//#define DEBUG_GS_PARAMETER_TEXT
//#define DEBUG_GS_REF_OBJECT
//#define DEBUG_GS_EPOCHSTR
//#define DEBUG_WRITE_PARAMETERS
//#define DEBUG_GS_ATTITUDE
//#define DEBUG_GS_SET_STRING

#if DEBUG_GROUNDSTATION
#include <iostream>
#include <sstream> 
#endif

// GroundStation parameter types
const Gmat::ParameterType   
GroundStation::PARAMETER_TYPE[GroundStationParamCount - GroundStationObjectParamCount] = 
   {
      Gmat::STRING_TYPE,      // Epoch 
      Gmat::REAL_TYPE,        // Element1
      Gmat::REAL_TYPE,        // Element2
      Gmat::REAL_TYPE,        // Element3
      Gmat::REAL_TYPE,        // Element4
      Gmat::REAL_TYPE,        // Element5
      Gmat::REAL_TYPE,        // Element6
      Gmat::STRING_TYPE,      // Element1Units
      Gmat::STRING_TYPE,      // Element2Units
      Gmat::STRING_TYPE,      // Element3Units
      Gmat::STRING_TYPE,      // Element4Units
      Gmat::STRING_TYPE,      // Element5Units
      Gmat::STRING_TYPE,      // Element6Units
      Gmat::ENUMERATION_TYPE, // StateType
      Gmat::ENUMERATION_TYPE, // DisplayStateType
      Gmat::OBJECT_TYPE,      // CoordinateSystem
      Gmat::ENUMERATION_TYPE, // DateFormat
      //Gmat::REAL_TYPE,        // Cd
      Gmat::OBJECT_TYPE,      // Attitude 
   };
   
const std::string 
GroundStation::PARAMETER_LABEL[GroundStationParamCount - GroundStationObjectParamCount] = 
   {
      "Epoch",
      "Element1", 
      "Element2", 
      "Element3", 
      "Element4", 
      "Element5", 
      "Element6", 
      "Element1Units", 
      "Element2Units", 
      "Element3Units", 
      "Element4Units", 
      "Element5Units", 
      "Element6Units", 
      "StateType", 
      "DisplayStateType", 
      "CoordinateSystem",
      "DateFormat", 
      //"Cd", 
      "Attitude",
   };

const std::string GroundStation::MULT_REP_STRINGS[EndMultipleReps - CART_X] = 
{
   // Cartesian
   "X",
   "Y",
   "Z",
   "VX",
   "VY",
   "VZ",
   // Speherical AZFPA
   "RMAG",
   "RA",
   "DEC",
   "VMAG",
   "AZI",
   "FPA",
   // Spherical RADEC
   "RAV",
   "DECV",
};

const Integer GroundStation::ATTITUDE_ID_OFFSET = 20000;

//-------------------------------------
// public methods
//-------------------------------------

//---------------------------------------------------------------------------
//  GroundStation(const std::string &name)
//---------------------------------------------------------------------------
/**
 * Creates constructors with parameters.
 *
 * @param <name> Optional name for the object.  Defaults to "".
 *
 */
GroundStation::GroundStation(const std::string &name, const std::string &typeStr) :
   GroundStationObject          (Gmat::GROUNDSTATION, typeStr, name),
   scEpochStr           ("21545.000000000"), 
   //coeffDrag            (2.2),
   epochSystem          ("TAI"),
   epochFormat          ("ModJulian"),
   epochType            ("TAIModJulian"),  // Should be A1ModJulian?
   stateType            ("Cartesian"),
   displayStateType     ("Cartesian"),
   internalCoordSystem  (NULL),
   coordinateSystem     (NULL),
   coordSysName         ("EarthMJ2000Eq"),
   attitude             (NULL),
   initialDisplay       (false),
   csSet                (false)
{
   //MessageInterface::ShowMessage("=====> GroundStation::GroundStation(%s) entered\n",
   //                              name.c_str());
   
   objectTypes.push_back(Gmat::GROUNDSTATION);
   objectTypeNames.push_back("GroundStation");
   
   Real a1mjd = -999.999;
   std::string outStr;
   Real taimjd = 21545.0;
   
   // Internal epoch is in A1ModJulian, so convert
   TimeConverterUtil::Convert("TAIModJulian", taimjd, "",
                              "A1ModJulian", a1mjd, outStr);
   
   //state.SetEpoch(21545.0);
   state.SetEpoch(a1mjd);
   
   state[0] = 7100.0;
   state[1] = 0.0;
   state[2] = 1300.0;
   state[3] = 0.0;
   state[4] = 7.35;
   state[5] = 1.0;

   stateElementLabel.push_back("X");
   stateElementLabel.push_back("Y");
   stateElementLabel.push_back("Z");
   stateElementLabel.push_back("VX");
   stateElementLabel.push_back("VY");
   stateElementLabel.push_back("VZ");
   
   stateElementUnits.push_back("km");
   stateElementUnits.push_back("km");
   stateElementUnits.push_back("km");
   stateElementUnits.push_back("km/s");
   stateElementUnits.push_back("km/s");
   stateElementUnits.push_back("km/s");

   representations.push_back("Cartesian");
   representations.push_back("SphericalAZFPA");
   representations.push_back("SphericalRADEC");
   
   parameterCount = GroundStationParamCount;
   
   BuildElementLabelMap();
   
   //MessageInterface::ShowMessage("=====> GroundStation::GroundStation(%s) exiting\n",
   //                              name.c_str());
}


//---------------------------------------------------------------------------
//  ~GroundStation()
//---------------------------------------------------------------------------
/**
 * Destructor.
 */
GroundStation::~GroundStation()
{
   // Delete the attached hardware (it was set as clones)
   for (ObjectArray::iterator i = tanks.begin(); i < tanks.end(); ++i)
      delete *i;
   for (ObjectArray::iterator i = thrusters.begin(); i < thrusters.end(); ++i)
      delete *i;
   if (attitude) delete attitude;
}


//---------------------------------------------------------------------------
//  GroundStation(const GroundStation &a)
//---------------------------------------------------------------------------
/**
 * Copy Constructor for base GroundStation structures.
 *
 * @param <a> The original that is being copied.
 *
 * @notes We need to copy internal and display coordinate systems to work
 * properly in the mission sequence for object copy.
 */
GroundStation::GroundStation(const GroundStation &a) :
   GroundStationObject          (a),
   scEpochStr           (a.scEpochStr),
   //coeffDrag            (a.coeffDrag),
   epochSystem          (a.epochSystem),
   epochFormat          (a.epochFormat),
   epochType            (a.epochType),
   stateType            (a.stateType),
   displayStateType     (a.displayStateType),
   internalCoordSystem  (a.internalCoordSystem),   // need to copy
   coordinateSystem     (a.coordinateSystem),      // need to copy
   coordSysName         (a.coordSysName),
   stateConverter       (a.stateConverter),
   coordConverter       (a.coordConverter),
   initialDisplay       (false),
   csSet                (a.csSet)
{
   objectTypes.push_back(Gmat::GROUNDSTATION);
   objectTypeNames.push_back("GroundStation");
   parameterCount = a.parameterCount;
   
   state.SetEpoch(a.state.GetEpoch());
   // clone the attitude
   if (a.attitude) attitude = (Attitude*) a.attitude->Clone();
   else            attitude = NULL;
   
   state[0] = a.state[0];
   state[1] = a.state[1];
   state[2] = a.state[2];
   state[3] = a.state[3];
   state[4] = a.state[4];
   state[5] = a.state[5];
   
   stateElementLabel = a.stateElementLabel;
   stateElementUnits = a.stateElementUnits;
   representations   = a.representations;

   BuildElementLabelMap();
}


//---------------------------------------------------------------------------
//  GroundStation& operator=(const GroundStation &a)
//---------------------------------------------------------------------------
/**
 * Assignment operator for GroundStation structures.
 * 
 * @note: Coordinate systems are not copied here.
 *
 * @param <a> The original that is being copied.
 *
 * @return Reference to this object
 * 
 * @todo Determine how to handle hardware when copying GroundStation objects.
 */
GroundStation& GroundStation::operator=(const GroundStation &a)
{
   // Don't do anything if copying self
   if (&a == this)
      return *this;

   GroundStationObject::operator=(a);

   scEpochStr           = a.scEpochStr;
   //coeffDrag            = a.coeffDrag;
   epochSystem          = a.epochSystem;
   epochFormat          = a.epochFormat;
   epochType            = a.epochType;
   stateType            = a.stateType;
   displayStateType     = a.displayStateType;
   coordSysName         = a.coordSysName;
   internalCoordSystem  = a.internalCoordSystem; // need to copoy
   coordinateSystem     = NULL;
   //attitude             = a.attitude,        // correct?
   //attitude             = (Attitude*) a.attitude->Clone(),        // correct?
   stateConverter       = a.stateConverter;
   coordConverter       = a.coordConverter;
   initialDisplay       = false;
   csSet                = a.csSet;

   state.SetEpoch(a.state.GetEpoch());
   // clone the attitude
   if (a.attitude) attitude = (Attitude*) a.attitude->Clone();
   else            attitude = NULL;
   
   state[0] = a.state[0];
   state[1] = a.state[1];
   state[2] = a.state[2];
   state[3] = a.state[3];
   state[4] = a.state[4];
   state[5] = a.state[5];
   
   if (a.csSet)
   {
      coordinateSystem = a.coordinateSystem;
      csSet = true;
   }
   
   stateElementLabel = a.stateElementLabel;
   stateElementUnits = a.stateElementUnits;
   representations   = a.representations;
   if (attitude) delete attitude;  // right?
   if (a.attitude)  attitude = (Attitude*) a.attitude->Clone();
   else             attitude = NULL;
   
   BuildElementLabelMap();
   
   return *this;
}


//---------------------------------------------------------------------------
// CoordinateSystem* GetInternalCoordSystem()
//---------------------------------------------------------------------------
CoordinateSystem* GroundStation::GetInternalCoordSystem()
{
   return internalCoordSystem;
}


//---------------------------------------------------------------------------
// void SetInternalCoordSystem(CoordinateSystem *cs)
//---------------------------------------------------------------------------
void GroundStation::SetInternalCoordSystem(CoordinateSystem *cs)
{
   #if DEBUG_GROUNDSTATION_CS
      MessageInterface::ShowMessage
         ("GroundStation::SetInternalCoordSystem() cs=%s<%p> on %s\n",
          cs->GetName().c_str(),cs, instanceName.c_str());
   #endif
   
   if (internalCoordSystem != cs)
   {
      internalCoordSystem = cs;
      if (coordinateSystem == NULL)
         coordinateSystem = cs;
   }
}


//---------------------------------------------------------------------------
//  void SetState(const Rvector6 &cartState)
//---------------------------------------------------------------------------
/**
 * Set the elements to Cartesian states.
 * 
 * @param <cartState> cartesian state
 *
 */
//---------------------------------------------------------------------------
void GroundStation::SetState(const Rvector6 &cartState)
{
   #if DEBUG_GROUNDSTATION_SET
      MessageInterface::ShowMessage("GroundStation::SetState(Rvector6)\n");
      MessageInterface::ShowMessage(
      "GroundStation::SetState(Rvector6) cartesianState=%s\n",
       cartState.ToString().c_str());
   #endif
      
   SetState(cartState[0], cartState[1], cartState[2],
            cartState[3], cartState[4], cartState[5]);
}


//---------------------------------------------------------------------------
//  void SetState(const std::string elementType, Real *instate)
//---------------------------------------------------------------------------
/**
 * Set the elements to Cartesian states.
 * 
 * @param <elementType>  Element Type
 * @param <instate>      element states
 *
 */
//---------------------------------------------------------------------------
void GroundStation::SetState(const std::string &elementType, Real *instate)
{
   #if DEBUG_GROUNDSTATION_SET
      MessageInterface::ShowMessage(
         "GroundStation::SetState() elementType = %s, instate =\n"
         "   %.9lf, %.9lf, %.9lf, %.14lf, %.14lf, %.14lf\n",
         elementType.c_str(), instate[0], instate[1], instate[2], instate[3],
         instate[4], instate[5]);
   #endif
      
   Rvector6 newState;

   newState.Set(instate[0],instate[1],instate[2],
                instate[3],instate[4],instate[5]);

   if (elementType != "Cartesian")
   {
      stateType = "Cartesian";  // why not use SetStateFromRepresentation here?? wcs
      newState = stateConverter.Convert(instate, elementType, 
         stateType, trueAnomaly);
   }
   
   SetState(newState.Get(0),newState.Get(1),newState.Get(2),
            newState.Get(3),newState.Get(4),newState.Get(5));
}


//------------------------------------------------------------------------------
//  void SetState(const Real s1, const Real s2, const Real s3, 
//                const Real s4, const Real s5, const Real s6)
//------------------------------------------------------------------------------
/**
 * Set the elements of a Cartesian state.
 * 
 * @param <s1>  First element
 * @param <s2>  Second element
 * @param <s3>  Third element
 * @param <s4>  Fourth element
 * @param <s5>  Fifth element
 * @param <s6>  Sixth element  
 */
//------------------------------------------------------------------------------
void GroundStation::SetState(const Real s1, const Real s2, const Real s3, 
                          const Real s4, const Real s5, const Real s6)
{
    state[0] = s1;
    state[1] = s2;
    state[2] = s3;
    state[3] = s4;
    state[4] = s5;
    state[5] = s6;
}


//------------------------------------------------------------------------------
//  PropState& GetState() 
//------------------------------------------------------------------------------
/**
 * "Unhide" the GroundStationObject method.
 * 
 * @return the core PropState.   
 */
//------------------------------------------------------------------------------
PropState& GroundStation::GetState()
{
   return GroundStationObject::GetState();
}

//------------------------------------------------------------------------------
//  Rvector6 GetState(std::string &rep) 
//------------------------------------------------------------------------------
/**
 * Get the converted Cartesian states from states in different coordinate type.
 * 
 * @return converted Cartesian states   
 */
//------------------------------------------------------------------------------
Rvector6 GroundStation::GetState(std::string rep) 
{
   #ifdef DEGUG_STATE_INTERFACE
      MessageInterface::ShowMessage("Getting state in representation %s", 
         rep.c_str());
   #endif
   rvState = GetStateInRepresentation(rep);
   return rvState;
}


//------------------------------------------------------------------------------
//  Rvector6 GetState(std::string &rep) 
//------------------------------------------------------------------------------
/**
 * Get the converted Cartesian states from states in different coordinate type.
 * 
 * @return converted Cartesian states   
 */
//------------------------------------------------------------------------------
Rvector6 GroundStation::GetState(Integer rep) 
{
   rvState = GetStateInRepresentation(rep);
   return rvState;
}


//------------------------------------------------------------------------------
//  Rvector6 GetCartesianState() 
//------------------------------------------------------------------------------
/**
 * Get the converted Cartesian states from states in different coordinate type.
 * 
 * @return converted Cartesian states   
 */
//------------------------------------------------------------------------------
Rvector6 GroundStation::GetCartesianState() 
{   
//   Real *tempState = state.GetState();
//
//   for (int i=0; i<6; i++)
//      rvState[i] = tempState[i];
//   
   MessageInterface::ShowMessage("GetCartesianState() is obsolete; "
      "use GetState(\"Cartesian\") or GetState(%d) instead.\n", CARTESIAN_ID);
   return GetState("Cartesian");//rvState;
}

Rmatrix33 GroundStation::GetAttitude(Real a1mjdTime) const
{
   if (attitude) return attitude->GetCosineMatrix(a1mjdTime);
   else 
   {
      //MessageInterface::PopupMessage(Gmat::WARNING_, 
      //"No attitude defined for spacecraft %s, returning identity matrix.\n",
      //instanceName.c_str());
      return Rmatrix33();  // temporary - return identity matrix
   }
}


//Rvector3  GroundStation::GetAngularVelocity(Real a1mjdTime) const
//{
//   if (attitude) return attitude->GetAngularVelocity(a1mjdTime);
//   else 
//   {
//      //MessageInterface::PopupMessage(Gmat::WARNING_, 
//      //"No attitude defined for spacecraft %s, returning zero angular velocity vector.\n",
//      //instanceName.c_str());
//      return Rvector3(); // temporary - return zero vector
//   }
//}

UnsignedIntArray GroundStation::GetEulerAngleSequence() const
{
   if (attitude) 
      return attitude->GetUnsignedIntArrayParameter("EulerSequenceArray");
   else
   {
      std::string errmsg = 
         "Error attempting to retrieve Euler Angle Sequence for ground station \"";
      errmsg += instanceName + "\", for which no attitude has been set.\n";
      throw GroundStationObjectException(errmsg);
   }
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the GroundStation.
 *
 * @return clone of the GroundStation.
 *
 */
//------------------------------------------------------------------------------
GmatBase* GroundStation::Clone() const
{
   GroundStation *clone = new GroundStation(*this);
   
   #ifdef DEBUG_GROUNDSTATION
      MessageInterface::ShowMessage("Cloning %s (%x) to %x\n", 
         instanceName.c_str(), this, clone);
   #endif

   return (clone);
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
void GroundStation::Copy(const GmatBase* orig)
{
   operator=(*((GroundStation *)(orig)));
}


//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool GroundStation::RenameRefObject(const Gmat::ObjectType type,
                                 const std::string &oldName,
                                 const std::string &newName)
{
   #if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("GroundStation::RenameRefObject() type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   //if (type != Gmat::HARDWARE)
   //   return true;
   
   //for (UnsignedInt i=0; i<tankNames.size(); i++)
   //{
   //   if (tankNames[i] == oldName)
   //   {
   //      tankNames[i] = newName;
   //      break;
   //   }
   //}
   
   return true;
}


//------------------------------------------------------------------------------
// std::string GetRefObjectName(const Gmat::ObjectType type) const
//------------------------------------------------------------------------------
/**
 * This method returns a name of the referenced objects.
 *
 * @return a name of objects of the requested type.
 */
std::string GroundStation::GetRefObjectName(const Gmat::ObjectType type) const
{
   if (type == Gmat::COORDINATE_SYSTEM)
   {
      return coordSysName;
   }
   if (type == Gmat::ATTITUDE)   return "";   // Attitude objects don't have names
   return GroundStationObject::GetRefObjectName(type);
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
const ObjectTypeArray& GroundStation::GetRefObjectTypeArray()
{
   refObjectTypes.clear();
   refObjectTypes.push_back(Gmat::COORDINATE_SYSTEM);
   refObjectTypes.push_back(Gmat::ATTITUDE);
   return refObjectTypes;
}


//------------------------------------------------------------------------------
//  const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * This method returns an array with the names of the referenced objects.
 *
 * @return a vector with the names of objects of the requested type.
 */
//------------------------------------------------------------------------------
const StringArray& 
      GroundStation::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   static StringArray fullList;  // Maintain scope if the full list is requested
   fullList.clear();
   
   if (attitude)
   {
      try
      {
         fullList.push_back(attitude->GetRefObjectName(type));
      }
      catch (GmatBaseException& be)
      {
         // ignore exceptions here
      }
   }
   
   if (type == Gmat::UNKNOWN_OBJECT)
   {
      fullList.push_back(coordSysName);
      return fullList;      
   }
   else
   {
      if (type == Gmat::ATTITUDE)
         return fullList;
      
      //if (type == Gmat::THRUSTER)
      //   return thrusterNames;
            
      if (type == Gmat::COORDINATE_SYSTEM)
      {
         fullList.push_back(coordSysName);
         return fullList;
      }
   }
   
   return GroundStationObject::GetRefObjectNameArray(type);
}


// DJC: Not sure if we need this yet...
//------------------------------------------------------------------------------
// bool SetRefObjectName(const Gmat::ObjectType type, const std::string &name)
//------------------------------------------------------------------------------
bool GroundStation::SetRefObjectName(const Gmat::ObjectType type, const std::string &name)
{
   if (type == Gmat::COORDINATE_SYSTEM)
   {
      coordSysName = name;
      return true;
   }
   
   return GroundStationObject::SetRefObjectName(type, name);
}


//---------------------------------------------------------------------------
// GmatBase* GetRefObject(const Gmat::ObjectType type, const std::string &name)
//---------------------------------------------------------------------------
/**
 * Returns the reference object pointer.
 *
 * @param type type of the reference object.
 * @param name name of the reference object.
 *
 * @return reference object pointer.
 */
GmatBase* GroundStation::GetRefObject(const Gmat::ObjectType type, 
                                   const std::string &name)
{
   //MessageInterface::ShowMessage("Accessing ref object on %s of type ", instanceName.c_str());
   // This switch statement intentionally drops through without breaks, so that
   // the search in the tank and thruster name lists only need to be coded once. 
   switch (type)
   {
      case Gmat::COORDINATE_SYSTEM:
         //MessageInterface::ShowMessage("CoordinateSystem named %s\n", name.c_str());
         return coordinateSystem;
         
      case Gmat::ATTITUDE:
         #ifdef DEBUG_GS_ATTITUDE
         MessageInterface::ShowMessage(
         "In GS::GetRefObject - returning Attitude poinetr <%p>\n", attitude);
         #endif
         //MessageInterface::ShowMessage("CoordinateSystem named %s\n", name.c_str());
         return attitude;
         
	 //case Gmat::THRUSTER:
         //MessageInterface::ShowMessage("Thruster named %s\n", name.c_str());
         //for (ObjectArray::iterator i = thrusters.begin(); 
         //     i < thrusters.end(); ++i) {
         //   if ((*i)->GetName() == name)
         //      return *i;
         //}
         
         // Other Hardware cases go here...
         
         return NULL;      // Hardware requested, but not in the hardware lists
         
      default:
         break;
   }
   
   //MessageInterface::ShowMessage("Unknown named %s\n", name.c_str());
   return GroundStationObject::GetRefObject(type, name);
}


//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
//                   const std::string &name)
//------------------------------------------------------------------------------
bool GroundStation::SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
                              const std::string &name)
{
   #ifdef DEBUG_GS_REF_OBJECT
   MessageInterface::ShowMessage("Entering GS::SetRefObject\n");
   #endif
   
   if (obj == NULL)
      return false;
   
   // first, try setting it on the attitude (owned object)
   if (attitude)
   {
      try
      {
         attitude->SetRefObject(obj, type, name);
      }
      catch (BaseException &be)
      {
         #ifdef DEBUG_GS_ATTITUDE
         MessageInterface::ShowMessage(
         "------ error setting ref object %s on attitude\n",
         name.c_str());
         #endif
      }
   }
   if (type == Gmat::HARDWARE) {
      std::string typeStr = obj->GetTypeName();
    
      if (typeStr == "FuelTank") {
         if (find(tanks.begin(), tanks.end(), obj) == tanks.end()) {
            tanks.push_back(obj);
            return true;
         }
         return false;
      }
      
      if (typeStr == "Thruster") {
         if (find(thrusters.begin(), thrusters.end(), obj) == thrusters.end()) {
            thrusters.push_back(obj);
            return true;
         }
         return false;
      }
      
      return false;
   }
   else if (type == Gmat::COORDINATE_SYSTEM)
   {
      CoordinateSystem *cs = (CoordinateSystem*)obj;
      
      #if DEBUG_GROUNDSTATION_CS
      MessageInterface::ShowMessage
         ("GroundStation::SetRefObject() coordinateSystem=%s<%p>, cs=%s<%p> on %s\n",
          coordinateSystem->GetName().c_str(), coordinateSystem,
          cs->GetName().c_str(), cs, instanceName.c_str());
      #endif
      
      if (coordinateSystem != cs)
      {
         coordinateSystem = cs;         
         TakeAction("ApplyCoordinateSystem");
         
         #if DEBUG_GROUNDSTATION_CS
         MessageInterface::ShowMessage
            ("GroundStation::SetRefObject() coordinateSystem applied ----------\n");
         #endif
      }
      
      return true;
   }
   else if (type == Gmat::ATTITUDE)
   {
    #ifdef DEBUG_GS_ATTITUDE
       MessageInterface::ShowMessage("Setting attitude object on spacecraft %s\n",
       instanceName.c_str());
    #endif
    if ((attitude != NULL) && (attitude != (Attitude*) obj)) delete attitude;
    attitude = (Attitude*) obj;
    // set epoch ...
    #ifdef DEBUG_GS_ATTITUDE
       MessageInterface::ShowMessage("Setting attitude object on spacecraft %s\n",
       instanceName.c_str());
       MessageInterface::ShowMessage(
       "Setting epoch on attitude object for spacecraft %s\n",
       instanceName.c_str());
    #endif
    attitude->SetEpoch(state.GetEpoch());
    ownedObjectCount++;
    return true;
   }
   
   #ifdef DEBUG_GS_REF_OBJECT
   MessageInterface::ShowMessage
      ("Exiting GS::SetRefObject, Calling GroundStationObject::SetRefObject()\n");
   #endif
   
   return GroundStationObject::SetRefObject(obj, type, name);
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
ObjectArray& GroundStation::GetRefObjectArray(const Gmat::ObjectType type)
{
  //if (type == Gmat::THRUSTER)
  //    return thrusters;
   return GroundStationObject::GetRefObjectArray(type);
}


//---------------------------------------------------------------------------
//  ObjectArray& GetRefObjectArray(const Gmat::ObjectType type)
//---------------------------------------------------------------------------
/**
 * Obtains an array of GmatBase pointers based on a string (e.g. the typename).
 * 
 * @param typeString The string used to find the objects requested.
 *
 * @return Reference to the array.
 */
ObjectArray& GroundStation::GetRefObjectArray(const std::string& typeString)
{
  //if ((typeString == "Thruster") || (typeString == "Thrusters"))
  //    return thrusters;
   return GroundStationObject::GetRefObjectArray(typeString);
}


//---------------------------------------------------------------------------
//  Integer GetParameterID(const std::string &str) const
//---------------------------------------------------------------------------
/**
 * Retrieve the ID for the parameter given its description.
 *
 * @param <str> Description for the parameter.
 *
 * @return the parameter ID, or -1 if there is no associated ID.
 */
Integer GroundStation::GetParameterID(const std::string &str) const
{
   #ifdef DEBUG_PARM_PERFORMANCE
      MessageInterface::ShowMessage("GroundStation::GetParameterID(%s)\n", str.c_str());
   #endif
   #ifdef DEBUG_GET_REAL
   MessageInterface::ShowMessage("In GS::GetParameterID, str = %s\n ",
   str.c_str());
   #endif
   
   // first check the multiple reps
   Integer sz = EndMultipleReps - CART_X;
   for (Integer ii = 0; ii < sz; ii++)
      if (str == MULT_REP_STRINGS[ii])
      {
         #ifdef DEBUG_GET_REAL
         MessageInterface::ShowMessage(
         "In GS::GetParameterID, multiple reps found!! - str = %s and id = %d\n ",
         str.c_str(), (ii + CART_X));
         #endif
         return ii + CART_X;
      }

   Integer retval = -1;
   if (str == "Element1" || str == "X" || str == "RMAG")  
      retval =  ELEMENT1_ID;
      //return ELEMENT1_ID;

   else if (str == "Element2" || str == "Y" || str == "RA") 
      retval =  ELEMENT2_ID;
      //return ELEMENT2_ID;

   else if (str == "Element3" || str == "Z" || str == "DEC")
      retval =  ELEMENT3_ID;
      //return ELEMENT3_ID;

   else if (str == "Element4" || str == "VX" || str == "VMAG") 
      retval =  ELEMENT4_ID;
      //return ELEMENT4_ID;

   else if (str == "Element5" || str == "VY" || str == "AZI" || str == "RAV")
      retval =  ELEMENT5_ID;
      //return ELEMENT5_ID;

   else if (str == "Element6" || str == "VZ" || str == "FPA" || str == "DECV") 
      retval =  ELEMENT6_ID;
      //return ELEMENT6_ID;

   #ifdef DEBUG_GET_REAL
   MessageInterface::ShowMessage(
   "In GS::GetParameterID, after checking for elements, id = %d\n ",
   retval);
   #endif
   if (retval != -1) return retval;
   
   for (Integer i = GroundStationObjectParamCount; i < GroundStationParamCount; ++i)
   {
      if (str == PARAMETER_LABEL[i - GroundStationObjectParamCount])
      {
         #ifdef DEBUG_GROUNDSTATION_SET
         MessageInterface::ShowMessage(
         "In GS::GetParameterID, setting id to %d for str = %s\n ",
         i, str.c_str());
         #endif
         return i;
      }
   }
   if (attitude)
   {
      try
      {
         Integer attId = attitude->GetParameterID(str);
         #ifdef DEBUG_GS_ATTITUDE
         MessageInterface::ShowMessage(
            "------ Now calling attitude to get id for label %s\n",
            str.c_str());
            MessageInterface::ShowMessage(" ------ and the id = %d\n", attId);
         #endif
         return attId + ATTITUDE_ID_OFFSET;
      }
      catch (BaseException& be)
      {
         // continue - not an attitude parameter
      }
      
   }
   return GroundStationObject::GetParameterID(str);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool GroundStation::IsParameterReadOnly(const Integer id) const
{
   if (id >= ATTITUDE_ID_OFFSET)
   {
      if (attitude) 
         return attitude->IsParameterReadOnly(id - ATTITUDE_ID_OFFSET);
   }
   if ((id >= ELEMENT1UNIT_ID) && (id <= ELEMENT6UNIT_ID))
   {
      return true;
   }

   // Hide GroundStationObject epoch so ground station can treat it as a string   
   if (id == EPOCH_PARAM)
   {
      return true;
   }
   
   // if (id == STATE_TYPE) return true;   when deprecated stuff goes away
   
   return GroundStationObject::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool GroundStation::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


std::string GroundStation::GetParameterText(const Integer id) const
{
   #ifdef DEBUG_GS_PARAMETER_TEXT
   MessageInterface::ShowMessage("GS::GetParameterText - called with id = %d\n",
   id);
   #endif
   if ((id >= CART_X) && (id < EndMultipleReps))
   {
      #ifdef DEBUG_GS_PARAMETER_TEXT
      MessageInterface::ShowMessage("GS::GetParameterText - returning text = %s\n",
      (MULT_REP_STRINGS[id-CART_X]).c_str());
      #endif
      return MULT_REP_STRINGS[id - CART_X];
   }
   // Handle the dynamic labels for the elements first
   if (id == ELEMENT1_ID || id == ELEMENT2_ID || id == ELEMENT3_ID 
       || id == ELEMENT4_ID || id == ELEMENT5_ID || id == ELEMENT6_ID)
      return stateElementLabel[id - ELEMENT1_ID];

   if ((id >= GroundStationObjectParamCount) && (id < GroundStationParamCount))
      return PARAMETER_LABEL[id - GroundStationObjectParamCount];
      
   if (id >= ATTITUDE_ID_OFFSET)
   {
      if (attitude) 
         return attitude->GetParameterText(id - ATTITUDE_ID_OFFSET);
   }

   #ifdef DEBUG_GS_PARAMETER_TEXT
   MessageInterface::ShowMessage(
   "GS::GetParameterText - calling through to base class .....\n");
   #endif
   return GroundStationObject::GetParameterText(id);
}

//------------------------------------------------------------------------------
//  Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the type of a parameter.
 *
 * @param <id> Integer ID of the parameter.
 *
 * @return The type of the parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType GroundStation::GetParameterType(const Integer id) const
{
   if ((id >= CART_X) && (id < EndMultipleReps))
      return Gmat::REAL_TYPE;
   if ((id >= GroundStationObjectParamCount) && (id < GroundStationParamCount))
      return PARAMETER_TYPE[id - GroundStationObjectParamCount];
   if (id >= ATTITUDE_ID_OFFSET)
      if (attitude) 
         return attitude->GetParameterType(id - ATTITUDE_ID_OFFSET);

    return GroundStationObject::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the text description for the type of a parameter.
 *
 * @param <id> Integer ID of the parameter.
 *
 * @return The text description of the type of the parameter.
 */
//------------------------------------------------------------------------------
std::string GroundStation::GetParameterTypeString(const Integer id) const
{
//    return GroundStationObject::PARAM_TYPE_STRING[GetParameterType(id)];
    return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}

//---------------------------------------------------------------------------
//  Real GetRealParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the value for a Real parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The parameter's value.
 */
Real GroundStation::GetRealParameter(const Integer id) const
{
   #ifdef DEBUG_GET_REAL
      MessageInterface::ShowMessage(
      "In GS::GetReal, asking for parameter %d, whose string is \"%s\"\n", 
      id, (GetParameterText(id)).c_str());
      //for (Integer i=0; i<6;i++)
      //   MessageInterface::ShowMessage("   state(%d) = %.12f\n",
      //   i, state[i]);
      //MessageInterface::ShowMessage("    and stateType = %s\n",
      //   stateType.c_str());
   #endif

   if ((id >= ELEMENT1_ID && id <= ELEMENT6_ID) ||
      (id >= CART_X      && id < EndMultipleReps))
   {
      #ifdef DEBUG_GET_REAL
         MessageInterface::ShowMessage(
         "In GS::GetReal, calling GetElement ....... \n");
      #endif
      return (const_cast<GroundStation*>(this))->GetElement(GetParameterText(id));
   }
   
   ///if (id == CD_ID) return coeffDrag;
   
   if (id >= ATTITUDE_ID_OFFSET)
      if (attitude)
      {
         #ifdef DEBUG_GS_ATTITUDE
         MessageInterface::ShowMessage(
            "------ Now calling attitude to get real parameter for id =  %d\n",
            id);
         #endif
         return attitude->GetRealParameter(id - ATTITUDE_ID_OFFSET);
      }
   
   return GroundStationObject::GetRealParameter(id);
}

//---------------------------------------------------------------------------
//  Real GetRealParameter(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Retrieve the value for a Real parameter.
 *
 * @param <label> The label of the parameter.
 *
 * @return The parameter's value.
 */
Real GroundStation::GetRealParameter(const std::string &label) const
{
   // Performance!
    if (label == "A1Epoch")
       return state.GetEpoch();
   
    // First check with anomaly
    /* OLD CODE
    if (label == "TA" || label == "MA" || label == "EA" || label == "HA")
    {
       //return trueAnomaly.GetValue();
       return trueAnomaly.GetValue(label);
    }
    */
    
    return GetRealParameter(GetParameterID(label));
}

//---------------------------------------------------------------------------
//  Real SetRealParameter(const Integer id, const Real value)
//---------------------------------------------------------------------------
/**
 * Set the value for a Real parameter.
 *
 * @param <id> The integer ID for the parameter.
 * @param <value> The new parameter value.
 *
 * @return the parameter value at the end of this call, or 
 *         REAL_PARAMETER_UNDEFINED if the parameter id is invalid or the 
 *         parameter type is not Real.
 */
Real GroundStation::SetRealParameter(const Integer id, const Real value)
{
   #ifdef DEBUG_GROUNDSTATION_SET
   MessageInterface::ShowMessage("In GS::SetRealParameter, id = %d and value = %.12f\n",
   id, value);
   #endif
   if (id >= CART_X && id < EndMultipleReps)
   {
      std::string idString = MULT_REP_STRINGS[id - CART_X];
      return SetRealParameter(idString,value);
   }
   if (id == ELEMENT1_ID) return SetRealParameter(stateElementLabel[0],value); 
   if (id == ELEMENT2_ID) return SetRealParameter(stateElementLabel[1],value); 
   if (id == ELEMENT3_ID) return SetRealParameter(stateElementLabel[2],value); 
   if (id == ELEMENT4_ID) return SetRealParameter(stateElementLabel[3],value); 
   if (id == ELEMENT5_ID) return SetRealParameter(stateElementLabel[4],value); 
   if (id == ELEMENT6_ID) return SetRealParameter(stateElementLabel[5],value); 

   //if (id == CD_ID)
   //{
   //   parmsChanged = true;
   //   return SetRealParameter("Cd",value);
   //}
   
   if (id >= ATTITUDE_ID_OFFSET)
      if (attitude) 
         return attitude->SetRealParameter(id - ATTITUDE_ID_OFFSET,value);

   return GroundStationObject::SetRealParameter(id, value);
}

//---------------------------------------------------------------------------
//  Real SetRealParameter(const std::string &label, const Real value)
//---------------------------------------------------------------------------
/**
 * Set the value for a Real parameter.
 *
 * @param <label> The label of the parameter.
 * @param <value> The new parameter value.
 *
 * @return the parameter value at the end of this call, or 
 *         REAL_PARAMETER_UNDEFINED if the parameter id is invalid or the 
 *         parameter type is not Real.
 */
Real GroundStation::SetRealParameter(const std::string &label, const Real value)
{
   #ifdef DEBUG_GROUNDSTATION_SET
   MessageInterface::ShowMessage(
   "In GS::SetRealParameter(label), label = %s and value = %.12f\n",
   label.c_str(), value);
   #endif
   // first (really) see if it's a parameter for an owned object (i.e. attitude)
   if (GetParameterID(label) >= ATTITUDE_ID_OFFSET)
      if (attitude)
         return attitude->SetRealParameter(label, value);
         
   // First try to set as a state element
   if (SetElement(label, value))
      return value;
      
   if (label == "A1Epoch")
   {
      state.SetEpoch(value);
      return value;
   }
   
   //if (label == "Cd")
   //{
   //   if (value >= 0.0)
   //     coeffDrag = value;
   //   else
   //   {
   //      std::stringstream buffer;
   //      buffer << value;
   //     throw GroundStationObjectException(
   //         "The value of " + buffer.str() + " on object " + instanceName +
   //         " parameter " + label +
   //         " is not an allowed value.\nThe allowed values are "
   //         " [Real Number >= 0.0].");
   //   }
   //   parmsChanged = true;
   //   return coeffDrag;
   //}

   return GroundStationObject::SetRealParameter(label, value);
}


//---------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The string stored for this parameter, or the empty string if there
 *         is no string association.
 */
std::string GroundStation::GetStringParameter(const Integer id) const
{
    if (id == GS_EPOCH_ID)
       return scEpochStr;
    
    if (id == DATE_FORMAT_ID)
       return epochType;

    if (id == STATE_TYPE_ID)
    {
       MessageInterface::ShowMessage( "\"StateType\" is deprecated as the "
          "string specifying the state type for display, and will be "
          "removed from a future build; please use \"DisplayStateType\" "
          "instead.\n" ); 
       return displayStateType; 
       //return stateType; 
    }

    if (id == DISPLAY_STATE_TYPE_ID)
    {
       return displayStateType; 
    }

    if (id == COORD_SYS_ID)
       return coordSysName; 

    if ((id >= ELEMENT1UNIT_ID) && (id <= ELEMENT6UNIT_ID))
       return stateElementUnits[id - ELEMENT1UNIT_ID];
       
    if (id >= ATTITUDE_ID_OFFSET)
       if (attitude)
       {
         #ifdef DEBUG_GS_ATTITUDE
         MessageInterface::ShowMessage(
            "------ Now calling attitude to get string parameter for id =  %d\n",
            id);
         #endif
          return attitude->GetStringParameter(id - ATTITUDE_ID_OFFSET);
       }

    return GroundStationObject::GetStringParameter(id); 
}

//---------------------------------------------------------------------------
//  std::string GetStringParameter(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <label> The label for the parameter.
 *
 * @return The string stored for this parameter, or the empty string if there
 *         is no string association.
 */
std::string GroundStation::GetStringParameter(const std::string &label) const
{
//   if (label == "StateType")
//      return stateType;
//   
   return GetStringParameter(GetParameterID(label));
}


//---------------------------------------------------------------------------
//  const StringArray& GetStringArrayParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Accesses lists of tank and thruster (and, eventually, other hardware) names.
 *
 * @param id The integer ID for the parameter.
 *
 * @return The requested StringArray; throws if the parameter is not a 
 *         StringArray.
 */
const StringArray& GroundStation::GetStringArrayParameter(const Integer id) const
{
   if (id >= ATTITUDE_ID_OFFSET)
      if (attitude)
      {
         #ifdef DEBUG_GS_ATTITUDE
         MessageInterface::ShowMessage(
            "------ Now calling attitude to SET string parameter for id =  %d\n",
            id);
         #endif
         return attitude->GetStringArrayParameter(id - ATTITUDE_ID_OFFSET);
      }
   return GroundStationObject::GetStringArrayParameter(id);
}


//---------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string &value)
//---------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 * @param <value> The new string for this parameter.
 *
 * @return true if the string is stored, false if not.
 */
bool GroundStation::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_GS_SET_STRING
      MessageInterface::ShowMessage
         ("GroundStation::SetStringParameter() string parameter %d (%s) to %s\n", 
         id, GetParameterText(id).c_str(), value.c_str());
   #endif
   
   if (id >= ATTITUDE_ID_OFFSET)
      if (attitude)
      {
         #ifdef DEBUG_GS_ATTITUDE
         MessageInterface::ShowMessage(
            "------ Now calling attitude to SET string parameter for id =  %d"
            " and value = %s\n", id, value.c_str());
         #endif
         return attitude->SetStringParameter(id - ATTITUDE_ID_OFFSET, value);
      }
   
   if ((id < GroundStationObjectParamCount) || (id >= GroundStationParamCount))
      return GroundStationObject::SetStringParameter(id, value);
   
   // Since we changed to OBJECT_TYPE or ENUMERATION_TYPE,
   // the lines were commented out (loj: 2008.03.26)
   //if ((GetParameterType(id) != Gmat::STRING_TYPE) && 
   //    (GetParameterType(id) != Gmat::STRINGARRAY_TYPE))
   //   throw GroundStationObjectException("Parameter " + GetParameterText(id) +
   //      " is being accessed as a string, but it is of type " +
   //      GetParameterTypeString(GetParameterType(id)));
   
   if (id == GS_EPOCH_ID)
   {
      // Validate first...

      // and set the epoch value in the state
      SetEpoch(value);
   }   
   else if (id == DATE_FORMAT_ID)
   {
      SetDateFormat(value);
   }
   else if ((id == STATE_TYPE_ID) || (id == DISPLAY_STATE_TYPE_ID))
   {  
      if (id == STATE_TYPE_ID)
          MessageInterface::ShowMessage( "\"StateType\" is deprecated as the "
          "string specifying the state type for display, and will be "
          "removed from a future build; please use \"DisplayStateType\" "
          "instead.\n" ); 
      
      // Check for invalid input then return unknown value from GmatBase 
      if (value != "Cartesian" &&  value != "SphericalAZFPA" && 
          value != "SphericalRADEC")
      {   
         throw GroundStationObjectException("Unknown state element representation: " + 
            value);
      }
      #ifdef DEBUG_GS_SET_STRING
      MessageInterface::ShowMessage("GS::SetString - setting display state type to %s\n",
      value.c_str());
      #endif
      
      if ((value == "Keplerian") || (value == "ModifiedKeplerian"))
      {
         // Load trueAnomaly with the state data
         Rvector6 kep = GetStateInRepresentation("Keplerian");
         trueAnomaly.SetSMA(kep[0]);
         trueAnomaly.SetECC(kep[1]);
         trueAnomaly.SetValue(kep[5]);
      }
      
      //stateType = value;
      displayStateType = value;
      UpdateElementLabels();
   }
   else (id == COORD_SYS_ID) 
   {
      parmsChanged = true;
      coordSysName = value;
   }
   
   #ifdef DEBUG_GS_SET_STRING
   MessageInterface::ShowMessage
      ("GroundStation::SetStringParameter() returning true\n");
   #endif
   
   return true;
}

//---------------------------------------------------------------------------
//  bool SetStringParameter(const std::string &label, const std::string &value)
//---------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param <label> The label for the parameter.
 * @param <value> The new string for this parameter.
 *
 * @return true if the string is stored, false if not.
 */
//---------------------------------------------------------------------------
bool GroundStation::SetStringParameter(const std::string &label, 
                                    const std::string &value)
{
   #if DEBUG_GROUNDSTATION
       MessageInterface::ShowMessage
          ("\nGroundStation::SetStringParameter(\"%s\", \"%s\") enters\n",
           label.c_str(), value.c_str() ); 
       Integer id = GetParameterID(label);
       MessageInterface::ShowMessage
          ("GetParameterText: %s\n", GetParameterText(id).c_str());
       MessageInterface::ShowMessage
          ("GroundStation::SetStringParameter exits sooner\n\n"); 
   #endif

   return SetStringParameter(GetParameterID(label),value);
}


//---------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string &value, 
//                          const Integer index)
//---------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//---------------------------------------------------------------------------
bool GroundStation::SetStringParameter(const Integer id, const std::string &value, 
                                    const Integer index)
{
   if (index < 0)
   {
      GroundStationObjectException ex;
      ex.SetDetails("The index %d is out-of-range for field \"%s\"", index,
                    GetParameterText(id).c_str());
      throw ex;
   }
   
   switch (id)
   {
     //case FUEL_TANK_ID:
     // {
     //    if (index < (Integer)tankNames.size())
     //       tankNames[index] = value;
     //    else
     //       // Only add the tank if it is not in the list already
     //       if (find(tankNames.begin(), tankNames.end(), value) == tankNames.end()) 
     //          tankNames.push_back(value);
     //    
     //    return true;
     // }
   default:
      return GmatBase::SetStringParameter(id, value, index);
   }
}


//---------------------------------------------------------------------------
//  bool SetStringParameter(const std::string &label, const std::string &value,
//                          const Integer index)
//---------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//---------------------------------------------------------------------------
bool GroundStation::SetStringParameter(const std::string &label, 
                                    const std::string &value,
                                    const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


//---------------------------------------------------------------------------
//  bool TakeAction(const std::string &action, const std::string &actionData)
//---------------------------------------------------------------------------
/**
 * Set the epoch.
 * 
 */
bool GroundStation::TakeAction(const std::string &action, 
                            const std::string &actionData)
{
   #ifdef DEBUG_GS_REF_OBJECT
   MessageInterface::ShowMessage("Entering GS::TakeAction with action = %s, and actionData = %s\n",
   action.c_str(), actionData.c_str());
   #endif   
   
   if (action == "ApplyCoordinateSystem")
   {
      #if DEBUG_GROUNDSTATION_CS
      MessageInterface::ShowMessage
         ("In TakeAction(): Calling StateConverter::SetMu(%p)\n", coordinateSystem);
      #endif
      
      if (!stateConverter.SetMu(coordinateSystem))
      {
         throw GroundStationObjectException(
            "\nError:  GroundStation has empty coordinate system\n");
      }
      
      if (csSet == false)
      {
         #if DEBUG_GROUNDSTATION_CS
         MessageInterface::ShowMessage
            ("In TakeAction(): Calling SetStateFromRepresentation()\n");
         #endif
         Rvector6 st(state.GetState());
         SetStateFromRepresentation(stateType, st); // this doesn't look right to me *****
         
         csSet = true;
      }
      
      #if DEBUG_GROUNDSTATION_CS
      MessageInterface::ShowMessage("In TakeAction(): returning true\n");
      #endif
      return true;
   }
   
   // 6/12/06 - arg: reset scEpochStr to epoch from prop state
   if (action == "UpdateEpoch")
   {
      Real currEpoch = state.GetEpoch();
      
      if (epochSystem != "")
      {
         if (epochSystem != "A1")
            currEpoch = TimeConverterUtil::Convert(currEpoch, 
                          TimeConverterUtil::A1,  
                          TimeConverterUtil::GetTimeTypeID(epochSystem),
                          GmatTimeUtil::JD_JAN_5_1941);
      }
      
      if (epochFormat != "")
      {  
         if (epochFormat == "Gregorian")
            scEpochStr = TimeConverterUtil::ConvertMjdToGregorian(currEpoch);
         else
         {
            std::stringstream timestream;
            timestream.precision(GetTimePrecision());
            timestream << currEpoch;
            scEpochStr = timestream.str();
         }
      }
      #ifdef DEBUG_GS_EPOCHSTR
      MessageInterface::ShowMessage("In TakeAction, epochSystem = %s\n",
      epochSystem.c_str());
      MessageInterface::ShowMessage("In TakeAction, epochFormat = %s\n",
      epochFormat.c_str());
      MessageInterface::ShowMessage("In TakeAction, scEpochStr being set to %s\n",
      scEpochStr.c_str());
      #endif
      
      return true;
   }
   
   return GroundStationObject::TakeAction(action, actionData);
}


//---------------------------------------------------------------------------
// bool IsOwnedObject(Integer id) const;
//---------------------------------------------------------------------------
bool GroundStation::IsOwnedObject(Integer id) const
{
   if (id == ATTITUDE)
      return true;
   else
      return false;
}


//---------------------------------------------------------------------------
// GmatBase* GetOwnedObject(Integer whichOne)
//---------------------------------------------------------------------------
GmatBase* GroundStation::GetOwnedObject(Integer whichOne)
{
   // only one owned object at the moment 
   if (attitude) 
      return attitude;
   
   return NULL;
}


//---------------------------------------------------------------------------
//  bool Initialize()
//---------------------------------------------------------------------------
/** 
 * Initialize the default values of ground station information. 
 * 
 * @return always success unless the coordinate system is empty 
 * 
 */
bool GroundStation::Initialize()
{
   #if DEBUG_GROUNDSTATION_CS
      MessageInterface::ShowMessage("GroundStation::Initialize() "
         "internalCoordSystem=%p, coordinateSystem=%p\n", internalCoordSystem, 
         coordinateSystem);
   
      if (internalCoordSystem)
         MessageInterface::ShowMessage("   internalCoordSystem is '%s'\n", 
            internalCoordSystem->GetName().c_str());
      if (coordinateSystem)
         MessageInterface::ShowMessage("   coordinateSystem is '%s'\n", 
            coordinateSystem->GetName().c_str());
          
      MessageInterface::ShowMessage
         ("   stateType=%s, state=\n   %.9f, %.9f, %.9f, %.14f, %.14f, %f.14\n",
          stateType.c_str(), state[0], state[1], state[2], state[3],
          state[4], state[5]);
   #endif
   
   // Set the mu if CelestialBody is there through coordinate system's origin;   
   // Otherwise, discontine process and send the error message   
   if (!stateConverter.SetMu(coordinateSystem))
   {      
      throw GroundStationObjectException("GroundStation has empty coordinate system");
   }   
   //if (!attitude)
     //{
     // #ifdef DEBUG_GS_ATTITUDE
     //MessageInterface::ShowMessage("GroundStation %s has no defined attitude object.\n",
     //                instanceName.c_str());
      //#endif
      //throw GroundStationObjectException("GroundStation has no attitude set.");
      //}
   //else
     //{
     //#ifdef DEBUG_GS_ATTITUDE
     //    MessageInterface::ShowMessage(
     //    "Initializing attitude object for spacecraft %s\n",
     //    instanceName.c_str());
     // #endif
     // attitude->Initialize();
      //#ifdef DEBUG_GS_ATTITUDE
      //   MessageInterface::ShowMessage(
      //   "***Finished initializing attitude object for spacecraft %s\n",
      //   instanceName.c_str());
      //#endif
   //}
   #if DEBUG_GROUNDSTATION_CS
      MessageInterface::ShowMessage("GroundStation::Initialize() exiting ----------\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// std::string GetEpochString()
//------------------------------------------------------------------------------
std::string GroundStation::GetEpochString()
{
   Real outMjd = -999.999;
   std::string outStr;
   
   TimeConverterUtil::Convert("A1ModJulian", GetEpoch(), "",
                              epochType, outMjd, outStr);
   
   return outStr;
}


//------------------------------------------------------------------------------
// void SetDateFormat(const std::string &dateType) 
//------------------------------------------------------------------------------
/**
 * Sets the output date format of epoch.
 * 
 * @param <dateType> date type given. 
 */
//------------------------------------------------------------------------------
void GroundStation::SetDateFormat(const std::string &dateType) 
{
   #ifdef DEBUG_DATE_FORMAT
      MessageInterface::ShowMessage("GroundStation::SetDateFormat() "
         "Setting date format to %s; initial epoch is %s\n", 
         dateType.c_str(), scEpochStr.c_str());
   #endif
      
   epochType = dateType;
   scEpochStr = GetEpochString();
   
}


//------------------------------------------------------------------------------
//  void SetEpoch(std::string ep)
//------------------------------------------------------------------------------
/**
 * Set the epoch.
 *
 * @param <ep> The new epoch. 
 */
//------------------------------------------------------------------------------
void GroundStation::SetEpoch(const std::string &ep)
{
   #ifdef DEBUG_DATE_FORMAT
   MessageInterface::ShowMessage("GroundStation::SetEpoch() Setting epoch to %s\n",
                                 ep.c_str());
   #endif
   
   scEpochStr = ep;
   
   Real fromMjd = -999.999;
   Real outMjd = -999.999;
   std::string outStr;
   
   TimeConverterUtil::Convert(epochType, fromMjd, ep, "A1ModJulian", outMjd,
                              outStr);
   
   if (outMjd != -999.999)
   {
      state.SetEpoch(outMjd);
      if (attitude) attitude->SetEpoch(outMjd);
   }
   else
   {
      #ifdef DEBUG_DATE_FORMAT
      MessageInterface::ShowMessage("GroundStation::SetEpoch() oops!  outMjd = -999.999!!\n");
      #endif
   }
   
}


//------------------------------------------------------------------------------
// void SetEpoch(const std::string &type, const std::string &ep, Real a1mjd)
//------------------------------------------------------------------------------
/*
 * Sets output epoch type, system, format, and epoch.  No conversion is done here.
 *
 * @param  type   epoch type to be used for output (TAIModJulian, TTGregorian, etc)
 * @param  epoch  epoch string
 * @param  a1mjd  epoch in TAIModJulian format
 */
//------------------------------------------------------------------------------
void GroundStation::SetEpoch(const std::string &type, const std::string &ep, Real a1mjd)
{
   #ifdef DEBUG_GS_EPOCHSTR
   MessageInterface::ShowMessage("In GS::SetEpoch, type = %s, ep = %s, a1mjd = %.12f\n",
   type.c_str(), ep.c_str(), a1mjd);
   #endif
   TimeConverterUtil::GetTimeSystemAndFormat(type, epochSystem, epochFormat);
   epochType = type;
   scEpochStr = ep;
   state.SetEpoch(a1mjd); 
   if (attitude) attitude->SetEpoch(a1mjd);  
   #ifdef DEBUG_GS_EPOCHSTR
   MessageInterface::ShowMessage("and in GS::SetEpoch, epochSystem = %s, epochFormat = %s\n",
   epochSystem.c_str(), epochFormat.c_str());
   #endif
}


//------------------------------------------------------------------------------
// void  SetState(const std::string &type, const Rvector6 &cartState)
//------------------------------------------------------------------------------
/*
 * Sets output state type and state in cartesian representation. Just set to
 * internal cartesian state. No conversion is done here.
 *
 * @param  type       state type to be used for output
 * @param  cartState  cartesian state to set as internal state
 */
//------------------------------------------------------------------------------
void GroundStation::SetState(const std::string &type, const Rvector6 &cartState)
{
   #if DEBUG_GROUNDSTATION_SET
   MessageInterface::ShowMessage
      ("GroundStation::SetState() type=%s, cartState=\n   %s\n", type.c_str(),
       cartState.ToString().c_str());
   #endif
   
   //stateType = type;
   displayStateType = type;
   SetState(cartState[0], cartState[1], cartState[2],
            cartState[3], cartState[4], cartState[5]);
   UpdateElementLabels();
}

//-------------------------------------
// protected methods
//-------------------------------------

//------------------------------------------------------------------------------
// const std::string&  GetGeneratingString(Gmat::WriteMode mode,
//                const std::string &prefix, const std::string &useName)
//------------------------------------------------------------------------------
/**
 * Produces a string, possibly multi-line, containing the text that produces an
 * object.
 * 
 * @param mode Specifies the type of serialization requested.
 * @param prefix Optional prefix appended to the object's name
 * @param useName Name that replaces the object's name.
 * 
 * @return A string containing the text.
 */
//------------------------------------------------------------------------------
const std::string& GroundStation::GetGeneratingString(Gmat::WriteMode mode,
                                                   const std::string &prefix,
                                                   const std::string &useName)
{
   std::stringstream data;

   data.precision(GetDataPrecision());   // Crank up data precision so we don't lose anything
   std::string preface = "", nomme;
   
   if ((mode == Gmat::SCRIPTING) || (mode == Gmat::OWNED_OBJECT) ||
       (mode == Gmat::SHOW_SCRIPT))
      inMatlabMode = false;
   if (mode == Gmat::MATLAB_STRUCT || mode == Gmat::EPHEM_HEADER)
      inMatlabMode = true;
   
   if (useName != "")
      nomme = useName;
   else
      nomme = instanceName;
   
   if ((mode == Gmat::SCRIPTING) || (mode == Gmat::SHOW_SCRIPT))
   {
      std::string tname = typeName;
      data << "Create " << tname << " " << nomme << ";\n";
      preface = "GMAT ";
   }
   else if (mode == Gmat::EPHEM_HEADER)
   {
      data << typeName << " = " << "'" << nomme << "';\n";
      preface = "";
   }
   
   nomme += ".";
   
   if (mode == Gmat::OWNED_OBJECT) {
      preface = prefix;
      nomme = "";
   }
   
   preface += nomme;
   WriteParameters(mode, preface, data);
   
   generatingString = data.str();
   
   // Then call the parent class method for preface and inline comments
   return GroundStationObject::GetGeneratingString(mode, prefix, useName);
}


//------------------------------------------------------------------------------
// void WriteParameters(std::string &prefix, GmatBase *obj)
//------------------------------------------------------------------------------
/**
 * Code that writes the parameter details for an object.
 * 
 * @param prefix Starting portion of the script string used for the parameter.
 * @param obj The object that is written.
 */
//------------------------------------------------------------------------------
void GroundStation::WriteParameters(Gmat::WriteMode mode, std::string &prefix, 
                                 std::stringstream &stream)
{
   #ifdef DEBUG_WRITE_PARAMETERS
   MessageInterface::ShowMessage("--- Entering GS::WriteParameters ...\n");
   MessageInterface::ShowMessage("--- mode = %d, prefix = %s\n",
   (Integer) mode, prefix.c_str());
   MessageInterface::ShowMessage(" --- stateType = %s, displayStateType = %s\n",
   stateType.c_str(), displayStateType.c_str());
   #endif
   Integer i;
   Gmat::ParameterType parmType;
   std::stringstream value;
   value.precision(GetDataPrecision()); 
   
   bool showAnomaly = false;
   if (stateType == "Keplerian" || stateType == "ModKeplerian")
      showAnomaly = true;
   
   Integer *parmOrder = new Integer[parameterCount];   
   Integer parmIndex = 0;
   
   parmOrder[parmIndex++] = DATE_FORMAT_ID;
   parmOrder[parmIndex++] = GS_EPOCH_ID;
   parmOrder[parmIndex++] = COORD_SYS_ID;
   parmOrder[parmIndex++] = DISPLAY_STATE_TYPE_ID;
   parmOrder[parmIndex++] = ELEMENT1_ID;
   parmOrder[parmIndex++] = ELEMENT2_ID;
   parmOrder[parmIndex++] = ELEMENT3_ID;
   parmOrder[parmIndex++] = ELEMENT4_ID;
   parmOrder[parmIndex++] = ELEMENT5_ID;
   parmOrder[parmIndex++] = ELEMENT6_ID;
   parmOrder[parmIndex++] = ELEMENT1UNIT_ID;
   parmOrder[parmIndex++] = ELEMENT2UNIT_ID;
   parmOrder[parmIndex++] = ELEMENT3UNIT_ID;
   parmOrder[parmIndex++] = ELEMENT4UNIT_ID;
   parmOrder[parmIndex++] = ELEMENT5UNIT_ID;
   parmOrder[parmIndex++] = ELEMENT6UNIT_ID;
   
   bool registered;
   for (i = 0; i < parameterCount; ++i)
   {
      registered = false;
      for (Integer j = 0; j < parmIndex; ++j)
      {
         if (parmOrder[j] == i)
         {
            registered = true;
            break;
         }
      }
      if (!registered) 
         parmOrder[parmIndex++] = i;
   }
      
   Rvector6 repState = GetStateInRepresentation(displayStateType);
   
   #if DEBUG_GROUNDSTATION_GEN_STRING
   MessageInterface::ShowMessage
      ("   stateType=%s, repState=%s\n", stateType.c_str(),
       repState.ToString().c_str());
   MessageInterface::ShowMessage
      ("   displayStateType=%s, repState=%s\n", displayStateType.c_str(),
       repState.ToString().c_str());
   #endif
   
   for (i = 0; i < parameterCount; ++i)
   {
      if ((IsParameterReadOnly(parmOrder[i]) == false) &&
          (parmOrder[i] != J2000_BODY_NAME) &&
          (parmOrder[i] != STATE_TYPE_ID))         // deprecated
      {
         parmType = GetParameterType(parmOrder[i]);
         
         // Handle StringArray parameters separately
         if (parmType != Gmat::STRINGARRAY_TYPE &&
             parmType != Gmat::OBJECTARRAY_TYPE)
         {
            // Skip unhandled types
            if (
                (parmType != Gmat::UNSIGNED_INTARRAY_TYPE) &&
                (parmType != Gmat::RVECTOR_TYPE) &&
                (parmType != Gmat::RMATRIX_TYPE) &&
                (parmType != Gmat::UNKNOWN_PARAMETER_TYPE)
               )
            {
               // Fill in the l.h.s.
               value.str("");
               if ((parmOrder[i] >= ELEMENT1_ID) && 
                   (parmOrder[i] <= ELEMENT6_ID))
               {
                  #ifdef DEBUG_WRITE_PARAMETERS
                   MessageInterface::ShowMessage("--- parmOrder[i] = %d\n",
                  (Integer) parmOrder[i]);
                  MessageInterface::ShowMessage(" --- and that is for element %s\n",
                  (GetParameterText(parmOrder[i])).c_str());
                  #endif
                  value.precision(GetDataPrecision()); 
                  value << repState[parmOrder[i] - ELEMENT1_ID];
                  value.precision(GetDataPrecision()); 
               }
               else if (parmOrder[i] == DISPLAY_STATE_TYPE_ID)
               {
                  if (mode != Gmat::MATLAB_STRUCT)
                     value << displayStateType;
                  else
                     value << "'" << displayStateType << "'"; 
               }
               else
               {
                  #ifdef DEBUG_WRITE_PARAMETERS
                  MessageInterface::ShowMessage(
                  "--- about to call WriteParameterValue with parmOrder[i] = %d\n",
                  (Integer) parmOrder[i]);
                  MessageInterface::ShowMessage("--- and the string associated with it is %s\n",
                  GetParameterText(parmOrder[i]).c_str());
                  #endif
                  WriteParameterValue(parmOrder[i], value);
               }
               
               if (value.str() != "")
               {
                  stream << prefix << GetParameterText(parmOrder[i])
                         << " = " << value.str() << ";\n";
               }
            }
         }
         else 
         {
            bool writeQuotes = inMatlabMode || parmType == Gmat::OBJECTARRAY_TYPE;
            
            // Handle StringArrays
            StringArray sar = GetStringArrayParameter(parmOrder[i]);
            if (sar.size() > 0)
            {
               stream << prefix << GetParameterText(parmOrder[i]) << " = {";
               for (StringArray::iterator n = sar.begin(); n != sar.end(); ++n)
               {
                  if (n != sar.begin())
                     stream << ", ";
                  if (writeQuotes)
                     stream << "'";
                  stream << (*n);
                  if (writeQuotes)
                     stream << "'";
               }
               stream << "};\n";
            }
         }
      }
      // handle ATTITUDE differently
      else if (parmOrder[i] == ATTITUDE)
      {
         if (attitude)
            stream << prefix << "Attitude = " << attitude->GetAttitudeModelName() << ";\n";
         else 
            ;// ignore
      }      
      
   }

   // Prep in case spacecraft "own" the attached hardware
   GmatBase *ownedObject;
   std::string nomme, newprefix;

   #ifdef DEBUG_OWNED_OBJECT_STRINGS
      MessageInterface::ShowMessage("\"%s\" has %d owned objects\n",
         instanceName.c_str(), GetOwnedObjectCount());
   #endif

   for (i = 0; i < GetOwnedObjectCount(); ++i)
   {
      newprefix = prefix;
      ownedObject = GetOwnedObject(i);
      nomme = ownedObject->GetName();
      
      #ifdef DEBUG_OWNED_OBJECT_STRINGS
          MessageInterface::ShowMessage(
             "   id %d has type %s and name \"%s\"\n",
             i, ownedObject->GetTypeName().c_str(),
             ownedObject->GetName().c_str());
      #endif
          
      if (nomme != "")
         newprefix += nomme + ".";
      //else if (GetType() == Gmat::FORCE_MODEL)  wcs - why is this here? GetType on s/c?
      //   newprefix += ownedObject->GetTypeName();
      stream << ownedObject->GetGeneratingString(Gmat::OWNED_OBJECT, newprefix);
   }
   
   delete [] parmOrder;
}


//------------------------------------------------------------------------------
// void UpdateElementLabels()
//------------------------------------------------------------------------------
/**
 * Code used to set the element labels. 
 */
//------------------------------------------------------------------------------
void GroundStation::UpdateElementLabels()
{
   //if (stateType == "Cartesian")
   if (displayStateType == "Cartesian")
   {
      stateElementLabel[0] = "X";
      stateElementLabel[1] = "Y";
      stateElementLabel[2] = "Z";
      stateElementLabel[3] = "VX";
      stateElementLabel[4] = "VY";
      stateElementLabel[5] = "VZ";
      
      stateElementUnits[0] = "km";
      stateElementUnits[1] = "km";
      stateElementUnits[2] = "km";
      stateElementUnits[3] = "km/s";
      stateElementUnits[4] = "km/s";
      stateElementUnits[5] = "km/s";
      
      return;
   }

   //if (stateType == "SphericalAZFPA")
   if (displayStateType == "SphericalAZFPA")
   {
      stateElementLabel[0] = "RMAG";
      stateElementLabel[1] = "RA";
      stateElementLabel[2] = "DEC";
      stateElementLabel[3] = "VMAG";
      stateElementLabel[4] = "AZI";
      stateElementLabel[5] = "FPA";

      stateElementUnits[0] = "km";
      stateElementUnits[1] = "deg";
      stateElementUnits[2] = "deg";
      stateElementUnits[3] = "km/s";
      stateElementUnits[4] = "deg";
      stateElementUnits[5] = "deg";

      return;
   }
    
   //if (stateType == "SphericalRADEC")
   if (displayStateType == "SphericalRADEC")
   {
      stateElementLabel[0] = "RMAG";
      stateElementLabel[1] = "RA";
      stateElementLabel[2] = "DEC";
      stateElementLabel[3] = "VMAG";
      stateElementLabel[4] = "RAV";
      stateElementLabel[5] = "DECV";

      stateElementUnits[0] = "km";
      stateElementUnits[1] = "deg";
      stateElementUnits[2] = "deg";
      stateElementUnits[3] = "km/s";
      stateElementUnits[4] = "deg";
      stateElementUnits[5] = "deg";

      return;
   }   

   //if (stateType == "LATLONHGT")
   if (displayStateType == "LATLONHGT")
   {
      stateElementLabel[0] = "LAT";
      stateElementLabel[1] = "LON";
      stateElementLabel[2] = "HGT";
      stateElementLabel[3] = "";
      stateElementLabel[4] = "";
      stateElementLabel[5] = "";

      stateElementUnits[0] = "deg";
      stateElementUnits[1] = "deg";
      stateElementUnits[2] = "km";
      stateElementUnits[3] = "";
      stateElementUnits[4] = "";
      stateElementUnits[5] = "";

      return;
   }   

}


//------------------------------------------------------------------------------
// Rvector6 GetStateInRepresentation(std::string rep)
//------------------------------------------------------------------------------
/**
 * Code used to obtain a state in a non-Cartesian representation.
 */
//------------------------------------------------------------------------------
Rvector6 GroundStation::GetStateInRepresentation(std::string rep) 
{
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage(
         "GroundStation::GetStateInRepresentation(string): Constructing %s state\n", 
         rep.c_str());
   #endif

   Rvector6 csState;
   Rvector6 finalState;
   
   // First convert from the internal CS to the state CS
   if (internalCoordSystem != coordinateSystem)
   {
      Rvector6 inState(state.GetState());
      coordConverter.Convert(GetEpoch(), inState, internalCoordSystem, csState, 
         coordinateSystem);
   }
   else
   {
      csState.Set(state.GetState());
   }
   
   // Then convert to the desired representation
   if (rep == "")
      rep = stateType;   // do I want displayStateType here?
   
   if (rep == "Cartesian")
      finalState = csState;
   else
      //finalState = stateConverter.Convert(csState, "Cartesian", rep, trueAnomaly);
      finalState = stateConverter.Convert(csState, "Cartesian", rep, anomalyType);
   
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage(
         "GroundStation::GetStateInRepresentation(string): %s state is "
         "[%.9lf %.9lf %.9lf %.14lf %.14lf %.14lf]\n", 
         rep.c_str(), finalState[0], finalState[1], 
         finalState[2], finalState[3], finalState[4], 
         finalState[5]);
   #endif
   
   return finalState;
}


//------------------------------------------------------------------------------
// Rvector6 GetStateInRepresentation(Integer rep)
//------------------------------------------------------------------------------
/**
 * Code used to obtain a state in a non-Cartesian representation.
 */
//------------------------------------------------------------------------------
Rvector6 GroundStation::GetStateInRepresentation(Integer rep)
{
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage(
         "GroundStation::GetStateInRepresentation(int): Constructing %s state\n", 
         representations[rep].c_str());
   #endif
   Rvector6 csState;
   Rvector6 finalState;
   
   // First convert from the internal CS to the state CS
   if (internalCoordSystem != coordinateSystem)
   {
      Rvector6 inState(state.GetState());
      coordConverter.Convert(GetEpoch(), inState, internalCoordSystem, csState, 
         coordinateSystem);
   }
   else
   {
      csState.Set(state.GetState());
   }
   
   // Then convert to the desired representation
   if (rep == CARTESIAN_ID)
      finalState = csState;
   else
   {
      finalState = stateConverter.Convert(csState, "Cartesian", 
                   representations[rep], anomalyType); //trueAnomaly);
   }
   
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage(
         "GroundStation::GetStateInRepresentation(int): %s state is "
         "[%.9lf %.9lf %.9lf %.14lf %.14lf %.14lf]\n", 
         representations[rep].c_str(), finalState[0], finalState[1], 
         finalState[2], finalState[3], finalState[4], 
         finalState[5]);
   #endif

   return finalState;
}


//------------------------------------------------------------------------------
// void SetStateFromRepresentation(std::string rep, Rvector6 &st)
//------------------------------------------------------------------------------
/**
 * Code used to obtain a state in a non-Cartesian representation.
 */
//------------------------------------------------------------------------------
void GroundStation::SetStateFromRepresentation(std::string rep, Rvector6 &st)
{
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage(
         "GroundStation::SetStateFromRepresentation: Setting %s state to %s\n", 
         rep.c_str(), st.ToString().c_str());
   #endif

   // First convert from the representation to Cartesian   
   static Rvector6 csState, finalState;
   
   if (rep == "Cartesian")
      csState = st;
   else
      //loj:csState = stateConverter.Convert(st, rep, "Cartesian", trueAnomaly);
      csState = stateConverter.Convert(st, rep, "Cartesian", anomalyType);
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage(
         "GroundStation::SetStateFromRepresentation: state has been converted\n");
   #endif
   
   // Then convert to the internal CS
   if (internalCoordSystem != coordinateSystem)
      coordConverter.Convert(GetEpoch(), csState, coordinateSystem, finalState, 
         internalCoordSystem);
   else
      finalState = csState;

   for (int i=0; i<6; i++)
      state[i] = finalState[i];
   
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage(
         //"GroundStation::SetStateFromRepresentation: State is now\n   %s"
         "GroundStation::SetStateFromRepresentation: State is now\n"
         "%.9lf %.9lf %.9lf %.14lf %.14lf %.14lf\n", state[0], state[1], 
         state[2], state[3], state[4], state[5]);
   #endif
}


//------------------------------------------------------------------------------
// Real GroundStation::GetElement(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain a state element.
 * 
 * @param <label> The test label for the element.
 * 
 * @return The element's value, or -9999999999.999999 on failure.
 */
//------------------------------------------------------------------------------
Real GroundStation::GetElement(const std::string &label)
{
   #ifdef DEBUG_GET_REAL
      MessageInterface::ShowMessage(
      "In GS::GetElement, asking for parameter %s\n", label.c_str());
   #endif
   Integer baseID;
   std::string rep = "";
   baseID = LookUpLabel(label,rep);
   #ifdef DEBUG_GET_REAL
   MessageInterface::ShowMessage(
   "In GS::GetElement, after LookUpLabel, id = %d, its string = \"%s\",  and rep = \"%s\"\n",
   baseID, (GetParameterText(baseID)).c_str(), rep.c_str());
   #endif
   Rvector6 stateInRep = GetStateInRepresentation(rep);
   #ifdef DEBUG_GET_REAL
      MessageInterface::ShowMessage(
      "In GS::GetElement, stateInRep = \n");
      for (Integer jj=0;jj<6;jj++)
         MessageInterface::ShowMessage("    %.12f\n", stateInRep[jj]);
   #endif

   if (baseID == ELEMENT1_ID) return stateInRep[0];
   if (baseID == ELEMENT2_ID) return stateInRep[1];
   if (baseID == ELEMENT3_ID) return stateInRep[2];
   if (baseID == ELEMENT4_ID) return stateInRep[3];
   if (baseID == ELEMENT5_ID) return stateInRep[4];
   if (baseID == ELEMENT6_ID) return stateInRep[5];

   return -9999999999.999999;  // some kind of error
}


//------------------------------------------------------------------------------
// bool SetElement(const std::string &label, const Real &value)
//------------------------------------------------------------------------------
/**
 * Set a state element.
 * 
 * @param <label> Label for the element -- 'X', 'Y', 'SMA', etc.
 * @param <value> New value for the element.
 * 
 * @return true on success, false on failure.
 */
//------------------------------------------------------------------------------
bool GroundStation::SetElement(const std::string &label, const Real &value)
{
   #ifdef DEBUG_GROUNDSTATION_SET
   MessageInterface::ShowMessage("In GS::SetElement, label = %s, value = %.12f\n",
   label.c_str(), value);
   #endif
   std::string rep = "";
   Integer id = LookUpLabel(label, rep) - ELEMENT1_ID;
   ///////
   //MessageInterface::ShowMessage
   //   ("==> rep=<%s>, stateType=<%s>, csSet=%d\n", rep.c_str(), stateType.c_str(), csSet);
   
   if ((rep != "") && (stateType != rep))
   {
      //SetStringParameter(STATE_TYPE_ID, rep); 
     stateType = rep;
   }
   ///////
   #ifdef DEBUG_GROUNDSTATION_SET
   if (id >= 0)
   MessageInterface::ShowMessage(
   "In GS::SetElement, after LookUpLabel, id+ELEMENT1_ID = %d, its string = \"%s\",  and rep = \"%s\"\n",
   id+ELEMENT1_ID, (GetParameterText(id+ELEMENT1_ID)).c_str(), rep.c_str());
   #endif

   if (id >= 0)
   {
      if (csSet)
      {
         Rvector6 tempState = GetStateInRepresentation(rep);
         tempState[id] = value;
         SetStateFromRepresentation(rep, tempState);
      }
      else
      {
         Real *tempState = state.GetState();
         tempState[id] = value;
      }
      
      #ifdef DEBUG_GROUNDSTATION_SET
      MessageInterface::ShowMessage("In GS::SetElement, returning TRUE\n");
      #endif
      return true;
   }
   
   #ifdef DEBUG_GROUNDSTATION_SET
   MessageInterface::ShowMessage("In GS::SetElement, returning FALSE\n");
   #endif
   return false;
}


//------------------------------------------------------------------------------
// void SetStateFromRepresentation(std::string rep, Rvector6 &st)
//------------------------------------------------------------------------------
/**
 * Code used to obtain a state in a non-Cartesian representation.
 */
//------------------------------------------------------------------------------
Integer GroundStation::LookUpLabel(const std::string &label, std::string &rep)
{
   Integer retval = -1;
   
   if (label == "Element1")
   {
      rep = stateType;
      return ELEMENT1_ID;
   }
   if (label == "Element2")
   {
      rep = stateType;
      return ELEMENT2_ID;
   }
   if (label == "Element3")
   {
      rep = stateType;
      return ELEMENT3_ID;
   }
   if (label == "Element4")
   {
      rep = stateType;
      return ELEMENT4_ID;
   }
   if (label == "Element5")
   {
      rep = stateType;
      return ELEMENT5_ID;
   }
   if (label == "Element6")
   {
      rep = stateType;
      return ELEMENT6_ID;
   }
   
   if (label == "X" || label == "RMAG" || label == "LAT")
      retval = ELEMENT1_ID;

   if (label == "Y" || label == "RA" || label == "LON" )
      retval = ELEMENT2_ID;

   if (label == "Z" || label == "DEC" || label == "HGT" )  
      retval = ELEMENT3_ID;

   if (label == "VX" || label == "VMAG")  
      retval = ELEMENT4_ID;

   if (label == "VY" || label == "AZI" || label == "RAV")  
      retval = ELEMENT5_ID;

   if (label == "VZ" || label == "FPA" || label == "DECV")  
      retval = ELEMENT6_ID;
   
   rep = elementLabelMap[label];
   
   #ifdef DEBUG_STATE_INTERFACE
      MessageInterface::ShowMessage("GroundStation::LookUpLabel(%s..) gives rep %s\n",
         label.c_str(), rep.c_str());
   #endif
   
   return retval;
}

Integer GroundStation::LookUpID(const Integer id, std::string &label, std::string &rep)
{
   label = GetParameterText(id);
   // if it's not one of the multiple reps IDs, just return the ID
   if (id < CART_X) 
   {
      rep   = stateType;
      return id;
   }
   // otherwise, figure out the base ID to use for the state data
   return LookUpLabel(label, rep);
}

//------------------------------------------------------------------------------
// void BuildElementLabelMap()
//------------------------------------------------------------------------------
/**
 * Set the mapping between elements and representations.
 */
//------------------------------------------------------------------------------
void GroundStation::BuildElementLabelMap()
{
   if (elementLabelMap.size() == 0)
   {
      elementLabelMap["X"] = "Cartesian";
      elementLabelMap["Y"] = "Cartesian";
      elementLabelMap["Z"] = "Cartesian";
      elementLabelMap["VX"] = "Cartesian";
      elementLabelMap["VY"] = "Cartesian";
      elementLabelMap["VZ"] = "Cartesian";

      elementLabelMap["RMAG"] = "SphericalAZFPA";
      elementLabelMap["RA"]   = "SphericalAZFPA";
      elementLabelMap["DEC"]  = "SphericalAZFPA";
      elementLabelMap["VMAG"] = "SphericalAZFPA";
      elementLabelMap["AZI"]  = "SphericalAZFPA";
      elementLabelMap["FPA"]  = "SphericalAZFPA";

      elementLabelMap["RAV"]  = "SphericalRADEC";
      elementLabelMap["DECV"] = "SphericalRADEC";

      elementLabelMap["LAT"]    = "LATLONHGT";
      elementLabelMap["LON"]    = "LATLONHGT";
      elementLabelMap["HGT"] = "LATLONHGT";
            
   }
}

//------------------------------------------------------------------------------
// Real frequencyRampingFunction()
//------------------------------------------------------------------------------
/**
 * This function adjusts the send/receive frequency of the ground station.
 */
//------------------------------------------------------------------------------

Real GroundStation::frequencyRampingFunction(){
  return 1;
}

   

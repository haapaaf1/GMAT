//$Id$
//------------------------------------------------------------------------------
//                            GroundStation
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/08/01
//
/**
 * Implements the Groundstation class used to model ground based tracking stations.
 */
//------------------------------------------------------------------------------

#include "GroundStation.hpp"
#include "MessageInterface.hpp"
#include "GmatBaseException.hpp"


//#define DEBUG_OBJECT_MAPPING
//#define DEBUG_INIT
//#define TEST_GROUNDSTATION


//---------------------------------
// static data
//---------------------------------

/// Labels used for the ground station parameters.
const std::string 
GroundStation::PARAMETER_TEXT[GroundStationParamCount - SpacePointParamCount] =
   {
         "CentralBody",
         "StateType",         // Cartesian or Geographical
         "HorizonReference",  // Sphere or Ellipsoid
         "Location1",         // X or Latitude value
         "Location2",         // Y or Longitude value
         "Location3",         // Z or Height value
         "LOCATION_LABEL_1",  // "X" or "Latitude"
         "LOCATION_LABEL_2",  // "Y" or "Longitude"
         "LOCATION_LABEL_3"   // "Z" or "Height"
   };

const Gmat::ParameterType 
GroundStation::PARAMETER_TYPE[GroundStationParamCount - SpacePointParamCount] =
   {
         Gmat::OBJECT_TYPE,
         Gmat::STRING_TYPE,
         Gmat::STRING_TYPE,
         Gmat::REAL_TYPE,
         Gmat::REAL_TYPE,
         Gmat::REAL_TYPE,
         Gmat::STRING_TYPE,
         Gmat::STRING_TYPE,
         Gmat::STRING_TYPE
   };



//---------------------------------
// public methods
//---------------------------------

   
//---------------------------------------------------------------------------
//  GroundStation(const std::string &itsName)
//---------------------------------------------------------------------------
/**
 * Constructs a GroundStation object (default constructor).
 *
 * @param <itsName> Optional name for the object.  Defaults to "".
 */
//---------------------------------------------------------------------------
GroundStation::GroundStation(const std::string &itsName) :
   SpacePoint        (Gmat::GROUND_STATION, "GroundStation", itsName),
   cBodyName         ("Earth"),
   theBody           (NULL),
   stateType         ("Cartesian"),
   horizon           ("Sphere"),
   solarSystem       (NULL),
   bfcs              (NULL),
   mj2kcs            (NULL)
{
   parameterCount = GroundStationParamCount;
   
   locationLabels.push_back("X");
   locationLabels.push_back("Y");
   locationLabels.push_back("Z");
   
   location[0] = 6378.14;
   location[1] = 0.0;
   location[2] = 0.0;
   
   bfLocation[0] = 6378.14;
   bfLocation[1] = 0.0;
   bfLocation[2] = 0.0;
}

//---------------------------------------------------------------------------
// ~GroundStation()
//---------------------------------------------------------------------------
/**
 * Destructor.
 */
//---------------------------------------------------------------------------
GroundStation::~GroundStation()
{
}

//---------------------------------------------------------------------------
//  GroundStation(const GroundStation& gs)
//---------------------------------------------------------------------------
/**
 * Constructs a new GroundStation by copying the input instance (copy 
 * constructor).
 *
 * @param gs  GroundStation instance to copy to create "this" instance.
 */
//---------------------------------------------------------------------------
GroundStation::GroundStation(const GroundStation& gs) :
   SpacePoint        (gs),
   cBodyName         (gs.cBodyName),
   theBody           (NULL),
   locationLabels    (gs.locationLabels),
   stateType         (gs.stateType),
   horizon           (gs.horizon),
   solarSystem       (NULL),
   bfcs              (NULL),
   mj2kcs            (NULL)
{
   location[0] = gs.location[0];
   location[1] = gs.location[1];
   location[2] = gs.location[2];;
   
   bfLocation[0] = gs.bfLocation[0];
   bfLocation[1] = gs.bfLocation[1];
   bfLocation[2] = gs.bfLocation[2];
}


//---------------------------------------------------------------------------
//  GroundStation& operator=(const GroundStation& gs)
//---------------------------------------------------------------------------
/**
 * Assignment operator for GroundStations.
 *
 * @param gs The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
GroundStation& GroundStation::operator=(const GroundStation& gs)
{
   if (&gs != this)
   {
      SpacePoint::operator=(*this);
      
      theBody        = gs.theBody;
      locationLabels = gs.locationLabels;
      stateType      = gs.stateType;
      horizon        = gs.horizon;

      location[0]    = gs.location[0];
      location[1]    = gs.location[1];
      location[2]    = gs.location[2];;
      
      bfLocation[0]  = gs.bfLocation[0];
      bfLocation[1]  = gs.bfLocation[1];
      bfLocation[2]  = gs.bfLocation[2];
   }
   
   return *this;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Makes a copy of this instance and returns it as a GmatBase pointer.
 *
 * @return The clone of the GroundStation, as a GmatBase pointer.
 */
//------------------------------------------------------------------------------
GmatBase* GroundStation::Clone() const
{
   return new GroundStation(*this);
}



// Parameter access methods - overridden from GmatBase 

//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param id Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string GroundStation::GetParameterText(const Integer id) const
{
   if (id >= SpacePointParamCount && id < GroundStationParamCount)
      return PARAMETER_TEXT[id - SpacePointParamCount];
   return SpacePoint::GetParameterText(id);
}

//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param str string for the requested parameter.
 *
 * @return ID for the requested parameter.
 */
//------------------------------------------------------------------------------
Integer GroundStation::GetParameterID(const std::string &str) const
{
   // Handle 3 special cases
   if (str == locationLabels[0])
      return LOCATION_1;
   
   if (str == locationLabels[1])
      return LOCATION_2;
   
   if (str == locationLabels[2])
      return LOCATION_3;
   
   for (Integer i = SpacePointParamCount; i < GroundStationParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - SpacePointParamCount])
         return i;
   }
   
   return SpacePoint::GetParameterID(str);
}

//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param id ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType GroundStation::GetParameterType(const Integer id) const
{
   if (id >= SpacePointParamCount && id < GroundStationParamCount)
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
std::string GroundStation::GetParameterTypeString(const Integer id) const
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
bool GroundStation::IsParameterReadOnly(const Integer id) const
{
   if ((id == LOCATION_LABEL_1) || 
       (id == LOCATION_LABEL_2) || 
       (id == LOCATION_LABEL_3) )
      return true;

   return SpacePoint::IsParameterReadOnly(id);
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

//------------------------------------------------------------------------------
//  std::string  GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  string value of the requested parameter.
 */
//------------------------------------------------------------------------------
std::string GroundStation::GetStringParameter(const Integer id) const
{
   if (id == CENTRAL_BODY)   
   {
      if (theBody) 
         return theBody->GetName();
      else
         return cBodyName;
   }
   
   if (id == STATE_TYPE)
      return stateType;

   if (id == HORIZON_REFERENCE)
      return horizon;

   if (id == LOCATION_LABEL_1)
      return locationLabels[0];

   if (id == LOCATION_LABEL_2)
      return locationLabels[1];

   if (id == LOCATION_LABEL_3)
      return locationLabels[2];

   return SpacePoint::GetStringParameter(id);
}

//------------------------------------------------------------------------------
//  std::string  SetStringParameter(const Integer id, const std::string value)
//------------------------------------------------------------------------------
/**
 * This method sets the string parameter value, given the input
 * parameter ID.
 *
 * @param id ID for the requested parameter.
 * @param value string value for the requested parameter.
 *
 * @return  success flag.
 */
//------------------------------------------------------------------------------
bool GroundStation::SetStringParameter(const Integer id, 
                                       const std::string &value)
{
   bool retval = false;
   
   if (id == CENTRAL_BODY)   
   {
      if (theBody) 
         theBody = NULL;
      cBodyName = value;
      retval = true;
   }   
   else if (id == STATE_TYPE)
   {
      if ((value == "Cartesian") || (value == "Geographical"))
      {
         stateType = value;
         if (value == "Cartesian")
         {
            locationLabels[0] = "X";
            locationLabels[1] = "Y";
            locationLabels[2] = "Z";
         }
         else
         {
            locationLabels[0] = "Latitude";
            locationLabels[1] = "Longitude";
            locationLabels[2] = "Height";
         }
         retval = true;
      }
   }
   else if (id == HORIZON_REFERENCE)
   {
      if ((value == "Sphere") || (value == "Ellipsoid"))
      {
         horizon = value;
         retval = true;
      }
   }
   else if (id == LOCATION_LABEL_1)
      retval = false;
   else if (id == LOCATION_LABEL_2)
      retval = false;
   else if (id == LOCATION_LABEL_3)
      retval = false;
   else 
      retval = SpacePoint::SetStringParameter(id, value);
   
   return retval;
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Accessor method used to get a parameter value
 *
 * @param   label  label ID for the parameter
 *
 * @return the value of the parameter
 */
//------------------------------------------------------------------------------
std::string GroundStation::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
/**
* Accessor method used to get a parameter value
 *
 * @param    label Integer ID for the parameter
 * @param    value The new value for the parameter
 */
//------------------------------------------------------------------------------
bool GroundStation::SetStringParameter(const std::string &label, 
                                           const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}

//------------------------------------------------------------------------------
//  GmatBase* GetRefObject(const Gmat::ObjectType type,
//                         const std::string &name)
//------------------------------------------------------------------------------
/**
 * This method returns a reference object from the GroundStation class.
 *
 * @param type  type of the reference object requested
 * @param name  name of the reference object requested
 *
 * @return pointer to the reference object requested.
 */
//------------------------------------------------------------------------------
GmatBase* GroundStation::GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name)
{
   if ((type == Gmat::SPACE_POINT) || (type == Gmat::CELESTIAL_BODY))
      if (name == cBodyName)
         return theBody;
   
   // Not handled here -- invoke the next higher GetRefObject call
   return SpacePoint::GetRefObject(type, name);
}

//------------------------------------------------------------------------------
//  bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                    const std::string &name)
//------------------------------------------------------------------------------
/**
 * This method sets a reference object for the SpacePoint class.
 *
 * @param <obj>   pointer to the reference object
 * @param <type>  type of the reference object 
 * @param <name>  name of the reference object
 *
 * @return true if successful; otherwise, false.
 */
//------------------------------------------------------------------------------
bool GroundStation::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                 const std::string &name)
{
   if (obj == NULL)
      return false;

   #ifdef DEBUG_OBJECT_MAPPING
      MessageInterface::ShowMessage
         ("GroundStation::SetRefObject() this=%s, obj=<%p><%s> entered\n",
          GetName().c_str(), obj, obj->GetName().c_str());
   #endif

   switch (type)
   {
      case Gmat::SPACE_POINT:
      case Gmat::CELESTIAL_BODY:
         if (obj->GetName() == cBodyName)
         {
            theBody = (SpacePoint*)obj;
            // Let ancestors process this object as well

            #ifdef DEBUG_OBJECT_MAPPING
               MessageInterface::ShowMessage
                  ("GroundStation::Set theBody to %s\n", 
                   theBody->GetName().c_str());
            #endif
            
            SpacePoint::SetRefObject(obj, type, name);
            return true;
         }
         break;
         
      case Gmat::COORDINATE_SYSTEM:
         if (name == "EarthFixed")
         {
            bfcs = (CoordinateSystem*)obj;
            return true;
         }
         if (name == "EarthMJ2000Eq")
         {
            mj2kcs = (CoordinateSystem*)obj;
            return true;
         }

         break;

      default:
         break;
   }

   MessageInterface::ShowMessage("GroundStation::SetRefObject calling base for %s\n", name.c_str());

   // Not handled here -- invoke the next higher SetRefObject call
   return SpacePoint::SetRefObject(obj, type, name);
}




Real GroundStation::GetRealParameter(const Integer id) const
{
   if (id == LOCATION_1)
      return location[0];
   
   if (id == LOCATION_2)
      return location[1];
   
   if (id == LOCATION_3)
      return location[2];
   
   return SpacePoint::GetRealParameter(id);
}

Real GroundStation::SetRealParameter(const Integer id,
                                      const Real value)
{
   // Need to add range checking here
   if (id == LOCATION_1)
   {
      location[0] = value;
      return location[0];
   }
   
   if (id == LOCATION_2)
   {
      location[1] = value;
      return location[1];
   }
   
   if (id == LOCATION_3)
   {
      location[2] = value;
      return location[2];
   }
   
   return SpacePoint::SetRealParameter(id, value);
}

//Real GroundStation::GetRealParameter(const Integer id,
//                                      const Integer index) const
//{
//   return SpacePoint::GetRealParameter(id, index);
//}
//
//Real GroundStation::GetRealParameter(const Integer id, const Integer row,
//                                      const Integer col) const
//{
//   return SpacePoint::GetRealParameter(id, row, col);
//}

Real GroundStation::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}

Real GroundStation::SetRealParameter(const std::string &label,
                                      const Real value)
{
   return SetRealParameter(GetParameterID(label), value);
}

// These indexed methods seem like they should NOT be needed, but GCC gets 
// confused about the overloaded versions of the following six methods:

//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id, const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value from a vector of strings, 
 * given the input parameter ID and the index into the vector. 
 *
 * @param id ID for the requested parameter.
 * @param index index for the particular string requested.
 * 
 * @return The requested string.
 */
//------------------------------------------------------------------------------
std::string GroundStation::GetStringParameter(const Integer id, 
                                              const Integer index) const
{
   return SpacePoint::GetStringParameter(id, index);
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value, 
//                         const Integer index)
//------------------------------------------------------------------------------
/**
 * This method sets a value on a string parameter value in a vector of strings, 
 * given the input parameter ID, the value, and the index into the vector. 
 *
 * @param id ID for the requested parameter.
 * @param value The new string.
 * @param index index for the particular string requested.
 * 
 * @return true if successful; otherwise, false.
 */
//------------------------------------------------------------------------------
bool GroundStation::SetStringParameter(const Integer id, 
                                       const std::string &value, 
                                       const Integer index)
{
   return SetStringParameter(id, value, index);
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label, 
//                                const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value from a vector of strings, 
 * given the label associated with the input parameter and the index into the 
 * vector. 
 *
 * @param label String identifier for the requested parameter.
 * @param index index for the particular string requested.
 * 
 * @return The requested string.
 */
//------------------------------------------------------------------------------
std::string GroundStation::GetStringParameter(const std::string &label, 
                                           const Integer index) const
{
   return SpacePoint::GetStringParameter(label,  index);
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value, 
//                         const Integer index)
//------------------------------------------------------------------------------
/**
 * This method sets a value on a string parameter value in a vector of strings, 
 * given the label associated with the input parameter and the index into the 
 * vector. 
 *
 * @param label String identifier for the requested parameter.
 * @param value The new string.
 * @param index index for the particular string requested.
 * 
 * @return true if successful; otherwise, false.
 */
//------------------------------------------------------------------------------
bool GroundStation::SetStringParameter(const std::string &label, 
                                       const std::string &value, 
                                       const Integer index)
{
   return SpacePoint::SetStringParameter(label, value, index);
}


//------------------------------------------------------------------------------
// GmatBase* GetRefObject(const Gmat::ObjectType type, const std::string &name, 
//                        const Integer index)
//------------------------------------------------------------------------------
/**
 * This method returns a pointer to a reference object contained in a vector of
 * objects in the GroundStation class.
 *
 * @param type type of the reference object requested
 * @param name name of the reference object requested
 * @param index index for the particular object requested.
 *
 * @return pointer to the reference object requested.
 */
//------------------------------------------------------------------------------
GmatBase* GroundStation::GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name, 
                                     const Integer index)
{
   return SpacePoint::GetRefObject(type, name, index);
}

//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                   const std::string &name, const Integer index)
//------------------------------------------------------------------------------
/**
 * This method sets a pointer to a reference object in a vector of objects in 
 * the GroundStation class.
 *
 * @param obj The reference object.
 * @param type type of the reference object requested
 * @param name name of the reference object requested
 * @param index index for the particular object requested.
 *
 * @return true if successful; otherwise, false.
 */
//------------------------------------------------------------------------------
bool GroundStation::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name, 
                                     const Integer index)
{
   // Call parent class to add objects to bodyList
   return SpacePoint::SetRefObject(obj, type, name, index);
}


//------------------------------------------------------------------------------
// 
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
std::string GroundStation::GetRefObjectName(const Gmat::ObjectType type) const
{
   return cBodyName;
}

const StringArray& GroundStation::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   // This is a hack assuming Earth-centered coordinates for everything
   static StringArray csNames;
   
   csNames.clear();
   
   if ((type == Gmat::COORDINATE_SYSTEM) || (type == Gmat::UNKNOWN_OBJECT))
   {
      csNames.push_back("EarthFixed");
      csNames.push_back("EarthMJ2000Eq");
   }
   
   return csNames;
}

// Handle the J2000Body methods
//------------------------------------------------------------------------------
//  const Rvector6 GetMJ2000State(const A1Mjd &atTime)
//------------------------------------------------------------------------------
/**
 * Method returning the MJ2000 state of the SpacePoint at the time atTime.
 *
 * @param <atTime> Time for which the state is requested.
 *
 * @return state of the SpacePoint at time atTime.
 *
 * @note This method is pure virtual and must be implemented by the 
 *       'leaf' (non-abstract derived) classes.
 */
//------------------------------------------------------------------------------
const Rvector6 GroundStation::GetMJ2000State(const A1Mjd &atTime)
{
   Real epoch = atTime.Get();
   Rvector6 bfState;
   
   // For now I'm ignoring velocity
   bfState.Set(bfLocation[0], bfLocation[1], bfLocation[2], 0.0, 0.0, 0.0);

   // Assuming you have pointer to coordinate systems mj2k and topo,
   // where mj2k is a J2000 system and topo is Topocentric
   ccvtr.Convert(epoch, bfState, bfcs, j2000PosVel, mj2kcs);
   
   return j2000PosVel;
}

//------------------------------------------------------------------------------
//  const Rvector3 GetMJ2000Position(const A1Mjd &atTime)
//------------------------------------------------------------------------------
/**
 * Method returning the MJ2000 position of the SpacePoint at the time atTime.
 *
 * @param <atTime> Time for which the position is requested.
 *
 * @return position of the SpacePoint at time atTime.
 *
 * @note This method is pure virtual and must be implemented by the 
 *       'leaf' (non-abstract derived) classes.
 */
//------------------------------------------------------------------------------
const Rvector3 GroundStation::GetMJ2000Position(const A1Mjd &atTime)
{
   Rvector6 rv = GetMJ2000State(atTime);
   j2000Pos = rv.GetR();
   return j2000Pos;
}

//------------------------------------------------------------------------------
//  const Rvector3 GetMJ2000Velocity(const A1Mjd &atTime)
//------------------------------------------------------------------------------
/**
 * Method returning the MJ2000 velocity of the SpacePoint at the time atTime.
 *
 * @param <atTime> Time for which the velocity is requested.
 *
 * @return velocity of the SpacePoint at time atTime.
 *
 * @note This method is pure virtual and must be implemented by the 
 *       'leaf' (non-abstract derived) classes.
 */
//------------------------------------------------------------------------------
const Rvector3 GroundStation::GetMJ2000Velocity(const A1Mjd &atTime)
{
   Rvector6 rv = GetMJ2000State(atTime);
   j2000Vel = rv.GetV();
   return j2000Vel;
}


//------------------------------------------------------------------------------
//  bool Initialize()
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
bool GroundStation::Initialize()
{
   #ifdef DEBUG_INIT
      MessageInterface::ShowMessage("Initializing %s\n", instanceName.c_str());
   #endif
   
   std::string sphType;
   
   if (theBody == NULL)
      throw GmatBaseException("Unable to initialize ground station" + 
            instanceName + "; its origin is not set\n");
   
   // Calculate the body-fixed Cartesian position
   if (stateType == "Cartesian")
   {
      bfLocation[0] = location[0];
      bfLocation[1] = location[1];
      bfLocation[2] = location[2];
   }
   else if (stateType == "Geographical")
   {
      sphType = "Geodetic";
      if (horizon == "Sphere")
         sphType = "Geocentric";
      // What key goes with "Reduced"?
      
      llh.SetLatitude(location[0], sphType);
      llh.SetLongitude(location[1]);
      llh.SetHeight(location[2]);
      
      Real equatorialRadius, flattening;
      equatorialRadius = theBody->GetRealParameter("EquatorialRadius");
      flattening = theBody->GetRealParameter("Flattening");
      
      Rvector3 loc = llh.GetSitePosition(equatorialRadius, flattening);
      bfLocation[0] = loc[0];
      bfLocation[1] = loc[1];
      bfLocation[2] = loc[2];
   }
   else
      throw GmatBaseException("Unable to initialize ground station \"" + 
            instanceName + "\"; stateType is not a recognized type (known "
                  "types are either \"Cartesian\" or \"Geographical\")");

   #ifdef DEBUG_INIT
      MessageInterface::ShowMessage("...Initialized!\n", instanceName.c_str());
   #endif
      
   #ifdef TEST_GROUNDSTATION
      MessageInterface::ShowMessage("For %s, %s %s location [%lf "
            "%lf %lf] --> XYZ [%lf %lf %lf]\n", instanceName.c_str(), 
            sphType.c_str(), stateType.c_str(), location[0], location[1], 
            location[2], bfLocation[0], bfLocation[1], bfLocation[2]);

      // Check the MJ2000 methods
      if (theBody == NULL)
      {
         MessageInterface::ShowMessage(
               "Error initializing ground station %s: theBody is not set\n", 
               instanceName.c_str());
         return false;
      }
      if (bfcs == NULL)
      {
         MessageInterface::ShowMessage(
               "Error initializing ground station %s: bfcs is not set\n", 
               instanceName.c_str());
         return false;
      }
      if (mj2kcs == NULL)
      {
         MessageInterface::ShowMessage(
               "Error initializing ground station %s: mj2kcs is not set\n", 
               instanceName.c_str());
         return false;
      }

      Rvector6 j2kState = GetMJ2000State(21545.0);
      MessageInterface::ShowMessage("The resulting MJ2000 Cartesian state is "
            "\n   [%s]\n", j2kState.ToString(16).c_str());
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
//  void SetSolarSystem(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
void GroundStation::SetSolarSystem(SolarSystem *ss)
{
   solarSystem = ss;
}


//------------------------------------------------------------------------------
// const std::string&  GetGeneratingString(Gmat::WriteMode mode,
//                const std::string &prefix, const std::string &useName)
//------------------------------------------------------------------------------
/**
 * Produces a string, containing the text that produces a GroundStation object.
 * 
 * This method overrides the base class method so that it can handle the 
 * changable names for the GS location vector.
 * 
 * @param mode Specifies the type of serialization requested.
 * @param prefix Optional prefix appended to the object's name
 * @param useName Name that replaces the object's name.
 * 
 * @return A string containing the text.
 */
//------------------------------------------------------------------------------
const std::string& GroundStation::GetGeneratingString(Gmat::WriteMode mode,
                        const std::string &prefix, const std::string &useName)
{
   std::stringstream data;

   // Crank up data precision so we don't lose anything
   data.precision(GetDataPrecision());   
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
   
   if (mode == Gmat::OWNED_OBJECT) 
   {
      preface = prefix;
      nomme = "";
   }
   
   preface += nomme;
   WriteParameters(mode, preface, data);
   
   generatingString = data.str();
   
   // Then call the parent class method for preface and inline comments
   return SpacePoint::GetGeneratingString(mode, prefix, useName);
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
   Integer i;
   Gmat::ParameterType parmType;
   std::stringstream value;
   value.precision(GetDataPrecision()); 
   
   for (i = 0; i < parameterCount; ++i)
   {
      if ((IsParameterReadOnly(i) == false))
      {
         parmType = GetParameterType(i);
         
         // Skip unhandled types
         if ((parmType != Gmat::UNSIGNED_INTARRAY_TYPE) &&
             (parmType != Gmat::RVECTOR_TYPE) &&
             (parmType != Gmat::RMATRIX_TYPE) &&
             (parmType != Gmat::UNKNOWN_PARAMETER_TYPE) )
         {
            // Fill in the l.h.s.
            value.str("");
            WriteParameterValue(i, value);
            if (value.str() != "")
            {
               if ((i >= LOCATION_1) && (i <= LOCATION_3))
               {
                  stream << prefix << GetStringParameter(i+3)
                         << " = " << value.str() << ";\n";
               }
               else
                  stream << prefix << GetParameterText(i)
                         << " = " << value.str() << ";\n";
            }
         }
      }
   }
}

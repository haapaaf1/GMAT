//$Id: BodyFixedPoint.cpp 5835 2008-09-12 00:01:15Z djcinsb $
//------------------------------------------------------------------------------
//                            BodyFixedPoint
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy C. Shoan, NASA/GSFC (moved from GroundStation code,
//         original author: Darrel J. Conway, Thinking Systems, Inc.)
// Created: 2008.08.22
//
/**
 * Implements the Groundstation class used to model ground based tracking stations.
 */
//------------------------------------------------------------------------------

#include "BodyFixedPoint.hpp"
#include "AssetException.hpp"
#include "MessageInterface.hpp"
#include "GmatBaseException.hpp"


//#define DEBUG_OBJECT_MAPPING
//#define DEBUG_INIT
//#define TEST_BODYFIXED_POINT
//#define DEBUG_BODYFIXED_STATE


//---------------------------------
// static data
//---------------------------------

/// Labels used for the ground station parameters.
const std::string
BodyFixedPoint::PARAMETER_TEXT[BodyFixedPointParamCount - SpacePointParamCount] =
   {
         "CentralBody",
         "StateType",         // Cartesian or Geographical
         "HorizonReference",  // Sphere, Ellipsoid, or Geoid
	 "LatitudeGeometry",  // Geodetic, Geocentric, or Reduced
         "Location1",         // X or Latitude value
         "Location2",         // Y or Longitude value
         "Location3",         // Z or Height value
         "LOCATION_LABEL_1",  // "X" or "Latitude"
         "LOCATION_LABEL_2",  // "Y" or "Longitude"
         "LOCATION_LABEL_3"   // "Z" or "Height"
   };

const Gmat::ParameterType
BodyFixedPoint::PARAMETER_TYPE[BodyFixedPointParamCount - SpacePointParamCount] =
   {
         Gmat::OBJECT_TYPE,
         Gmat::STRING_TYPE,
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
//  BodyFixedPoint(const std::string &itsName)
//---------------------------------------------------------------------------
/**
 * Constructs a BodyFixedPoint object (default constructor).
 *
 * @param <itsName> Optional name for the object.  Defaults to "".
 */
//---------------------------------------------------------------------------
BodyFixedPoint::BodyFixedPoint(const std::string &itsType, const std::string &itsName) :
   SpacePoint        (Gmat::BODY_FIXED_POINT, itsType, itsName),
   cBodyName         ("Earth"),
   theBody           (NULL),
   stateType         ("Cartesian"),
   horizon           ("Ellipsoid"),
   latitudeGeometry  ("Geodetic"),
   solarSystem       (NULL),
   bfcsName          (""),
   bfcs              (NULL),
   mj2kcsName        (""),
   mj2kcs            (NULL)
{
   objectTypeNames.push_back("BodyFixedPoint");
   parameterCount = BodyFixedPointParamCount;

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
// ~BodyFixedPoint()
//---------------------------------------------------------------------------
/**
 * Destructor.
 */
//---------------------------------------------------------------------------
BodyFixedPoint::~BodyFixedPoint()
{
}

//---------------------------------------------------------------------------
//  BodyFixedPoint(const BodyFixedPoint& bfp)
//---------------------------------------------------------------------------
/**
 * Constructs a new BodyFixedPoint by copying the input instance (copy
 * constructor).
 *
 * @param bfp  BodyFixedPoint instance to copy to create "this" instance.
 */
//---------------------------------------------------------------------------
BodyFixedPoint::BodyFixedPoint(const BodyFixedPoint& bfp) :
   SpacePoint        (bfp),
   cBodyName         (bfp.cBodyName),
   theBody           (NULL),
   locationLabels    (bfp.locationLabels),
   stateType         (bfp.stateType),
   horizon           (bfp.horizon),
   latitudeGeometry  (bfp.latitudeGeometry),
   solarSystem       (NULL),
   bfcsName          (bfp.bfcsName),
   bfcs              (NULL),
   mj2kcsName        (bfp.mj2kcsName),
   mj2kcs            (NULL)
{
   location[0] = bfp.location[0];
   location[1] = bfp.location[1];
   location[2] = bfp.location[2];;

   bfLocation[0] = bfp.bfLocation[0];
   bfLocation[1] = bfp.bfLocation[1];
   bfLocation[2] = bfp.bfLocation[2];
}


//---------------------------------------------------------------------------
//  BodyFixedPoint& operator=(const BodyFixedPoint& bfp)
//---------------------------------------------------------------------------
/**
 * Assignment operator for BodyFixedPoints.
 *
 * @param bfp The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
BodyFixedPoint& BodyFixedPoint::operator=(const BodyFixedPoint& bfp)
{
   if (&bfp != this)
   {
      SpacePoint::operator=(*this);

      theBody        = bfp.theBody;
      locationLabels = bfp.locationLabels;
      stateType      = bfp.stateType;
      horizon        = bfp.horizon;
      latitudeGeometry = bfp.latitudeGeometry;
      solarSystem    = bfp.solarSystem;
      //bfcsName       = bfp.bfcsName;   // yes or no?
      //bfcs           = bfp.bfcs;       // yes or no?
      //mj2kcsName     = bfp.mj2kcsName; // yes or no?
      //mj2kcs         = bfp.mj2kcs;     // yes or no?

      location[0]    = bfp.location[0];
      location[1]    = bfp.location[1];
      location[2]    = bfp.location[2];;

      bfLocation[0]  = bfp.bfLocation[0];
      bfLocation[1]  = bfp.bfLocation[1];
      bfLocation[2]  = bfp.bfLocation[2];
   }

   return *this;
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
std::string BodyFixedPoint::GetParameterText(const Integer id) const
{
   if (id >= SpacePointParamCount && id < BodyFixedPointParamCount)
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
Integer BodyFixedPoint::GetParameterID(const std::string &str) const
{
   // Handle 3 special cases
   if (str == locationLabels[0])
      return LOCATION_1;

   if (str == locationLabels[1])
      return LOCATION_2;

   if (str == locationLabels[2])
      return LOCATION_3;

   for (Integer i = SpacePointParamCount; i < BodyFixedPointParamCount; i++)
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
Gmat::ParameterType BodyFixedPoint::GetParameterType(const Integer id) const
{
   if (id >= SpacePointParamCount && id < BodyFixedPointParamCount)
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
std::string BodyFixedPoint::GetParameterTypeString(const Integer id) const
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
bool BodyFixedPoint::IsParameterReadOnly(const Integer id) const
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
bool BodyFixedPoint::IsParameterReadOnly(const std::string &label) const
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
std::string BodyFixedPoint::GetStringParameter(const Integer id) const
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

   if (id == LATITUDE_GEOMETRY)
      return latitudeGeometry;

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
bool BodyFixedPoint::SetStringParameter(const Integer id,
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
   else if (id == LATITUDE_GEOMETRY)
   {
      if ((value == "Geodetic") || (value == "Geocentric") || (value == "Reduced"))
      {
         latitudeGeometry = value;
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
std::string BodyFixedPoint::GetStringParameter(const std::string &label) const
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
bool BodyFixedPoint::SetStringParameter(const std::string &label,
                                           const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}

//------------------------------------------------------------------------------
//  GmatBase* GetRefObject(const Gmat::ObjectType type,
//                         const std::string &name)
//------------------------------------------------------------------------------
/**
 * This method returns a reference object from the BodyFixedPoint class.
 *
 * @param type  type of the reference object requested
 * @param name  name of the reference object requested
 *
 * @return pointer to the reference object requested.
 */
//------------------------------------------------------------------------------
GmatBase* BodyFixedPoint::GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name)
{
   #ifdef TEST_BODYFIXED_POINT
      MessageInterface::ShowMessage("Entering BodyFixedPoint::GetRefObject()");
      MessageInterface::ShowMessage("name = %s, cBodyName = %s\n",
            name.c_str(), cBodyName.c_str());
      if (!theBody)
         MessageInterface::ShowMessage("The Body is NULL, though!!!\n");
   #endif
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
bool BodyFixedPoint::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                 const std::string &name)
{
   if (obj == NULL)
      return false;

   #ifdef DEBUG_OBJECT_MAPPING
      MessageInterface::ShowMessage
         ("BodyFixedPoint::SetRefObject() this=%s, obj=<%p><%s> entered\n",
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
                  ("BodyFixedPoint::Set theBody to %s\n",
                   theBody->GetName().c_str());
            #endif

            SpacePoint::SetRefObject(obj, type, name);
            return true;
         }
         break;

      case Gmat::COORDINATE_SYSTEM:
         {
            if (!(obj->IsOfType("CoordinateSystem")))
               throw AssetException("BodyFixedPoint expecting a CoordinateSystem\n");
            CoordinateSystem *tmpCS = (CoordinateSystem*)obj;
            if ((name == bfcsName) &&
                (tmpCS->GetOriginName() == cBodyName))
            {
               bfcs = tmpCS;
               return true;
            }
            if ((name == mj2kcsName) &&
                (tmpCS->GetOriginName() == cBodyName))
            {
               mj2kcs = tmpCS;
               return true;
            }

            break;
         }
      default:
         break;
   }

   MessageInterface::ShowMessage("BodyFixedPoint::SetRefObject calling base for %s\n", name.c_str());

   // Not handled here -- invoke the next higher SetRefObject call
   return SpacePoint::SetRefObject(obj, type, name);
}




Real BodyFixedPoint::GetRealParameter(const Integer id) const
{
   if (id == LOCATION_1)
      return location[0];

   if (id == LOCATION_2)
      return location[1];

   if (id == LOCATION_3)
      return location[2];

   return SpacePoint::GetRealParameter(id);
}

Real BodyFixedPoint::SetRealParameter(const Integer id,
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

//Real BodyFixedPoint::GetRealParameter(const Integer id,
//                                      const Integer index) const
//{
//   return SpacePoint::GetRealParameter(id, index);
//}
//
//Real BodyFixedPoint::GetRealParameter(const Integer id, const Integer row,
//                                      const Integer col) const
//{
//   return SpacePoint::GetRealParameter(id, row, col);
//}

Real BodyFixedPoint::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}

Real BodyFixedPoint::SetRealParameter(const std::string &label,
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
std::string BodyFixedPoint::GetStringParameter(const Integer id,
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
bool BodyFixedPoint::SetStringParameter(const Integer id,
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
std::string BodyFixedPoint::GetStringParameter(const std::string &label,
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
bool BodyFixedPoint::SetStringParameter(const std::string &label,
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
 * objects in the BodyFixedPoint class.
 *
 * @param type type of the reference object requested
 * @param name name of the reference object requested
 * @param index index for the particular object requested.
 *
 * @return pointer to the reference object requested.
 */
//------------------------------------------------------------------------------
GmatBase* BodyFixedPoint::GetRefObject(const Gmat::ObjectType type,
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
 * the BodyFixedPoint class.
 *
 * @param obj The reference object.
 * @param type type of the reference object requested
 * @param name name of the reference object requested
 * @param index index for the particular object requested.
 *
 * @return true if successful; otherwise, false.
 */
//------------------------------------------------------------------------------
bool BodyFixedPoint::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
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
std::string BodyFixedPoint::GetRefObjectName(const Gmat::ObjectType type) const
{
   return cBodyName;
}

const StringArray& BodyFixedPoint::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   // This is a hack assuming Earth-centered coordinates for everything
   static StringArray csNames;

   csNames.clear();

   if ((type == Gmat::COORDINATE_SYSTEM) || (type == Gmat::UNKNOWN_OBJECT))
   {
      csNames.push_back(bfcsName);
      csNames.push_back(mj2kcsName);
//      csNames.push_back("EarthFixed");
//      csNames.push_back("EarthMJ2000Eq");
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
const Rvector6 BodyFixedPoint::GetMJ2000State(const A1Mjd &atTime)
{
   #ifdef DEBUG_BODYFIXED_STATE
      MessageInterface::ShowMessage("In GetMJ2000State for BodyFixedPoint %s\n",
            instanceName.c_str());
   #endif
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
const Rvector3 BodyFixedPoint::GetMJ2000Position(const A1Mjd &atTime)
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
const Rvector3 BodyFixedPoint::GetMJ2000Velocity(const A1Mjd &atTime)
{
   Rvector6 rv = GetMJ2000State(atTime);
   j2000Vel = rv.GetV();
   return j2000Vel;
}

//------------------------------------------------------------------------------
//  bool GetBodyFixedLocation(const A1Mjd &atTime) const
//------------------------------------------------------------------------------
/**
 * Method returning the BodyFixed location of the BodyFixedPoint
 * at the time atTime.
 *
 * @param <atTime> Time for which the location is requested.
 *
 * @return location of the BodyFixedPoint at time atTime.
 *
 * @note This method may be moved to an intermediate BodyFixedPoint
 * class, if/when appropriate.
 */
//------------------------------------------------------------------------------
const Rvector3 BodyFixedPoint::GetBodyFixedLocation(const A1Mjd &atTime) const
{
   Rvector3 locBodyFixed;
   locBodyFixed[0] = bfLocation[0];
   locBodyFixed[1] = bfLocation[1];
   locBodyFixed[2] = bfLocation[2];

   return locBodyFixed;
}

//------------------------------------------------------------------------------
//  CoordinateSystem* GetBodyFixedCoordinateSystem() const
//------------------------------------------------------------------------------
/**
 * Method returning the BodyFixed coordinate system used by this BodyFixedPoint.
 *
  * @return the BodyFixed coordinate system.
 *
 * @note This method may be moved to an intermediate BodyFixedPoint
 * class, if/when appropriate.
 */
//------------------------------------------------------------------------------
CoordinateSystem* BodyFixedPoint::GetBodyFixedCoordinateSystem() const
{
   return bfcs;
}


//------------------------------------------------------------------------------
//  void SetSolarSystem(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
void BodyFixedPoint::SetSolarSystem(SolarSystem *ss)
{
   solarSystem = ss;
}



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
GroundStation::PARAMETER_TEXT[GroundStationParamCount - BodyFixedPointParamCount] =
{
    "MeasurementModel"
};

const Gmat::ParameterType 
GroundStation::PARAMETER_TYPE[GroundStationParamCount - BodyFixedPointParamCount] =
{
    Gmat::OBJECT_TYPE,
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
   BodyFixedPoint    ("GroundStation", itsName)
{
   objectTypes.push_back(Gmat::GROUND_STATION);
   objectTypeNames.push_back("GroundStation");
   parameterCount = GroundStationParamCount;
   
   bfcsName   = "EarthFixed";
   mj2kcsName = "EarthMJ2000Eq";
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
   BodyFixedPoint        (gs)
{
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
      BodyFixedPoint::operator=(*this);
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

//------------------------------------------------------------------------------
//  GmatBase* GetRefObject(const Gmat::ObjectType type,
//                                  const std::string &name)
//------------------------------------------------------------------------------
/**
 * This method returns a pointer to a desired GmatBase object.
 *
 * @param <type> Object type of the requested object.
 * @param <name> String name of the requested object.
 *
 * @return  A pointer to a GmatBase object.
 */
//------------------------------------------------------------------------------
GmatBase* GroundStation::GetRefObject(const Gmat::ObjectType type,
                                  const std::string &name)
{
   GmatBase* retval = NULL;

   if (type == Gmat::MEASUREMENT_MODEL)
   {
         if (measModel->GetName() == name)
         {
            retval = (GmatBase*)measModel;
         }
   }

   if (retval != NULL)
      return retval;
   return BodyFixedPoint::GetRefObject(type, name);
}

//------------------------------------------------------------------------------
//  bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                                     const std::string &name)
//------------------------------------------------------------------------------
/**
 * This method returns a pointer to a desired GmatBase object.
 *
 * @param <obj>  Pointer to object
 * @param <type> Object type of the object.
 * @param <name> String name of the object.
 *
 * @return  A pointer to a GmatBase object.
 */
//------------------------------------------------------------------------------
bool GroundStation::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name)
{
    bool retval = false;

    // @TODO: This will overrite the measurement model object pointer
    // if there is more than one measurement model assigned in the
    // user script
    if (obj->IsOfType(Gmat::MEASUREMENT_MODEL))
    {
        measModel = (MeasurementModel*)obj;
        retval = true;
    }

   return retval;
}

///------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
bool GroundStation::Initialize()
{
   #ifdef DEBUG_INIT
      MessageInterface::ShowMessage("GroundStation::Initializing %s\n", instanceName.c_str());
   #endif
   
   std::string sphType;
   
   if (theBody == NULL)
      throw GmatBaseException("Unable to initialize ground station" + 
            instanceName + "; its origin is not set\n");
   
   // Get the body spin rate
   std::string cBodyName = GetStringParameter(GetParameterID("CentralBody"));
   CelestialBody*cBody = (CelestialBody*)(GetRefObject(Gmat::SPACE_POINT,cBodyName));
   bodySpinRate = cBody->GetAngularVelocity().GetMagnitude();
   // TODO: Is this possible?
   //bodySpinRate = theBody->GetRealParameter("AngularRate");

   equatorialRadius = theBody->GetRealParameter("EquatorialRadius");

   // If the horizon reference is a sphere, then the flattening is zero
   // and the geocentric and geodectic latitude angles are coincident
   if (horizon == "Ellipsoid")
   {
     flattening = theBody->GetRealParameter("Flattening");
   }
   else
   {
     flattening = 0.0;
   }

   // Calculate the body-fixed Cartesian position
   if (stateType == "Cartesian")
   {
      bfLocation[0] = location[0];
      bfLocation[1] = location[1];
      bfLocation[2] = location[2];
   }
   else if (stateType == "Geographical")
   {
      llh.SetLatitude(location[0], latitudeGeometry);
      llh.SetLongitude(location[1]);
      llh.SetHeight(location[2]);

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
   std::string preface = "", nomeasModele;
   
   if ((mode == Gmat::SCRIPTING) || (mode == Gmat::OWNED_OBJECT) ||
       (mode == Gmat::SHOW_SCRIPT))
      inMatlabMode = false;
   if (mode == Gmat::MATLAB_STRUCT || mode == Gmat::EPHEM_HEADER)
      inMatlabMode = true;
   
   if (useName != "")
      nomeasModele = useName;
   else
      nomeasModele = instanceName;
   
   if ((mode == Gmat::SCRIPTING) || (mode == Gmat::SHOW_SCRIPT))
   {
      std::string tname = typeName;
      data << "Create " << tname << " " << nomeasModele << ";\n";
      preface = "GMAT ";
   }
   else if (mode == Gmat::EPHEM_HEADER)
   {
      data << typeName << " = " << "'" << nomeasModele << "';\n";
      preface = "";
   }
   
   nomeasModele += ".";
   
   if (mode == Gmat::OWNED_OBJECT) 
   {
      preface = prefix;
      nomeasModele = "";
   }
   
   preface += nomeasModele;
   WriteParameters(mode, preface, data);
   
   generatingString = data.str();
   
   // Then call the parent class method for preface and inline comeasModelents
   return BodyFixedPoint::GetGeneratingString(mode, prefix, useName);
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

//------------------------------------------------------------------------------
// void SetMeasurementModel(Measurement* measModel)
//------------------------------------------------------------------------------
/**
 * Set the measurement model for this ground station.
 *
 * @param measModel The measurement model that is assigned.
 */
//------------------------------------------------------------------------------
void GroundStation::SetMeasurementModel(MeasurementModel* measModel)
{
    measModel = measModel;
}

//------------------------------------------------------------------------------
// MeasurementModel* GetMeasurementModel()
//------------------------------------------------------------------------------
/**
 * Return the measurement model for this ground station.
 *
 * @return A pointer to the measurement model.
 */
//------------------------------------------------------------------------------
MeasurementModel* GroundStation::GetMeasurementModel()
{
    return measModel;
}



//------------------------------------------------------------------------------
// void SetSpinRate(Real &sr)
//------------------------------------------------------------------------------
/**
 * Set the body spin rate for this instance of the measurement model.
 *
 * @param sr The body spin rate.
 */
//------------------------------------------------------------------------------
void GroundStation::SetSpinRate(Real &sr)
{
    bodySpinRate = sr;
}

//------------------------------------------------------------------------------
// Real GetSpinRate()
//------------------------------------------------------------------------------
/**
 * Return the body spin rate for this instance of the measurement model.
 *
 * @return The body spin rate.
 */
//------------------------------------------------------------------------------
Real GroundStation::GetSpinRate()
{
    return bodySpinRate;
}

//------------------------------------------------------------------------------
// void SetEquatorialRadius(Real &er)
//------------------------------------------------------------------------------
/**
 * Set the body equatorial radius for this instance of the measurement model.
 *
 * @param sr The body equatorial radius.
 */
//------------------------------------------------------------------------------
void GroundStation::SetEquatorialRadius(Real &er)
{
    equatorialRadius = er;
}

//------------------------------------------------------------------------------
// Real GetEquatorialRadius()
//------------------------------------------------------------------------------
/**
 * Return the body equatorial radius for this instance of the measurement model.
 *
 * @return The body equatorial radius.
 */
//------------------------------------------------------------------------------
Real GroundStation::GetEquatorialRadius()
{
    return equatorialRadius;
}

//------------------------------------------------------------------------------
// void SetFlattening(Real &sr)
//------------------------------------------------------------------------------
/**
 * Set the body flattening for this instance of the measurement model.
 *
 * @param sr The body flattening.
 */
//------------------------------------------------------------------------------
void GroundStation::SetFlattening(Real &flat)
{
    flattening = flat;
}

//------------------------------------------------------------------------------
// Real GetFlattening()
//------------------------------------------------------------------------------
/**
 * Return the body flattening for this instance of the measurement model.
 *
 * @return The body flattening.
 */
//------------------------------------------------------------------------------
Real GroundStation::GetFlattening()
{
    return flattening;
}


//------------------------------------------------------------------------------
// bool GetTheMeasurements(const SpacePoint* theSpacePoint, const A1Mjd &atTime)
//------------------------------------------------------------------------------
bool GroundStation::GetTheMeasurements(SpacePoint* theSpacePoint,
                                          const A1Mjd &atTime,
                                          LaGenMatDouble &theMeasurements)
{
    return (measModel)->GetTheMeasurements(theSpacePoint,atTime,
                                           theMeasurements);
}

//------------------------------------------------------------------------------
// bool GetThePartials(const std::string &param, const Integer &size,
//                     const SpacePoint* theSpacePoint, const A1Mjd &atTime)
//------------------------------------------------------------------------------
bool GroundStation::GetThePartials(const std::string &param,
                                      SpacePoint* theSpacePoint,
                                      const A1Mjd &atTime,
                                      LaGenMatDouble &theDerivatives)
{
    return (measModel)->GetThePartials((measModel)->GetDependentParamID(param), theSpacePoint,
                                       atTime, theDerivatives);
}

//------------------------------------------------------------------------------
// bool GetThePartials(const Integer paramID, const Integer &size,
//                     const SpacePoint* theSpacePoint, const A1Mjd &atTime)
//------------------------------------------------------------------------------
bool GroundStation::GetThePartials(const Integer &paramID,
                                   SpacePoint* theSpacePoint,
                                   const A1Mjd &atTime,
                                   LaGenMatDouble &theDerivatives)
{
    return (measModel)->GetThePartials(paramID,theSpacePoint,atTime,
                                     theDerivatives);
}




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
//const std::string 
//GroundStation::PARAMETER_TEXT[GroundStationParamCount - BodyFixedPointParamCount] =
//   {
//   };
//
//const Gmat::ParameterType 
//GroundStation::PARAMETER_TYPE[GroundStationParamCount - BodyFixedPointParamCount] =
//   {
//   };



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

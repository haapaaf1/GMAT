//$Header$
//------------------------------------------------------------------------------
//                         MeasurementModel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/23
//
/**
 * Defines the measurement models used for observer objects.
 */
//------------------------------------------------------------------------------

#include "MeasurementModel.hpp"
#include "lapackpp.h"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"
#include "TimeSystemConverter.hpp"
#include "StateConverter.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string MeasurementModel::MODEL_DESCRIPTIONS[EndModelReps] =
{
    "NotDefined",
    "Range",
    "RangeRate",
    "VariableTransmitterRange",
    "AntennaTracking",
    "SunSensor",
    "StarSensor",
    "GyroPackage",
    "HorizonSensor",
    "Videometers",
    "CoherentDoppler",
    "NonCoherentDoppler",
    "VariableTransmitterDoppler",
    "IntegratedDopplerCount",
    "IMU",
    "Magnetometer",
    "AO_AzEl",
    "RangeAzEl",
    "AO_RaDec",
    "RangeRaDec"
};

const std::string
MeasurementModel::PARAMETER_TEXT[MeasurementModelParamCount - GmatBaseParamCount] =
{
   "DataSource",
   "MeasurementTypes",
   "LightTimeCorrection",
   "IonosphericCorrection",
   "TroposphericCorrection",
   "LightTimeModel",
   "IonosphericModel",
   "TroposphericModel"
};


const Gmat::ParameterType
MeasurementModel::PARAMETER_TYPE[MeasurementModelParamCount - GmatBaseParamCount] =
{
   Gmat::OBJECTARRAY_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE   
};

//---------------------------------
// methods overridden from GMAT base
//---------------------------------

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the MeasurementModel.
 *
 * @return clone of the MeasurementModel.
 */
//------------------------------------------------------------------------------
GmatBase* MeasurementModel::Clone() const
{
   GmatBase *clone = new MeasurementModel(*this);
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
void MeasurementModel::Copy(const GmatBase* orig)
{
   operator=(*((MeasurementModel *)(orig)));
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
std::string MeasurementModel::GetParameterText(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < MeasurementModelParamCount))
   {
      //MessageInterface::ShowMessage("'%s':\n",
      //   PARAMETER_TEXT[id - GmatBaseParamCount].c_str());
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   }
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
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < MeasurementModelParamCount; ++i)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}

//------------------------------------------------------------------------------
//  Integer  GetDependentParamID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the dependent parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested dependent parameter.
 *
 * @return ID for the requested dependent parameter or -1 for no associated ID.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetDependentParamID(const std::string &str) const
{
   return -1;
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
Gmat::ParameterType MeasurementModel::GetParameterType(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < MeasurementModelParamCount))
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the value for a std::string parameter.
 * 
 * @param <id> Integer ID of the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetStringParameter(const Integer id) const
{
   if (id == LIGHTTIME_ID)
      return lightTimeModelName;

   if (id == IONOSPHERE_ID)
      return ionoModelName;

   if (id == TROPOSPHERE_ID)
      return tropoModelName;
          
   return GmatBase::GetStringParameter(id);
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == LIGHTTIME_ID)
   {
      lightTimeModelName = value;
      return true;
   }

   if (id == IONOSPHERE_ID)
   {
      ionoModelName = value;
      return true;
   }

   if (id == TROPOSPHERE_ID) 
   {
	tropoModelName = value;
	return true;
   }
 
   return GmatBase::SetStringParameter(id, value);
   
}

//------------------------------------------------------------------------------
//  bool GetBooleanParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the value for a boolean parameter.
 * 
 * @param <id> Integer ID of the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::GetBooleanParameter(const Integer id) const
{
   if (id == LIGHTTIMEFLAG_ID)
      return isLightTimeON;

   if (id == IONOSPHEREFLAG_ID)
      return isIonoON;

   if (id == TROPOSPHEREFLAG_ID)
      return isTropoON;
          
   return GmatBase::GetBooleanParameter(id);
}


//------------------------------------------------------------------------------
//  bool SetBooleanParameter(const Integer id, const bool value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a boolean parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetBooleanParameter(const Integer id, const bool &value)
{
   if (id == LIGHTTIMEFLAG_ID)
   {
      isLightTimeON = value;
      return true;
   }

   if (id == IONOSPHEREFLAG_ID)
   {
      isIonoON = value;
      return true;
   }

   if (id == TROPOSPHEREFLAG_ID) 
   {
	isTropoON = value;
	return true;
   }
 
   return GmatBase::SetBooleanParameter(id, value);
   
}

//------------------------------------------------------------------------------
//  std::string  GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  StringArray value of the requested parameter.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementModel::GetStringArrayParameter(const Integer id) const
{

   if (id == MEASUREMENTTYPES_ID)
      return measurementTypesAllowed;
  
   return GmatBase::GetStringArrayParameter(id);
}

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
//  MeasurementModel::MeasurementModel()
//------------------------------------------------------------------------------
/**
 * Constructs base MeasurementModel structures
 */
MeasurementModel::MeasurementModel(const std::string typeName,
      const std::string name) :
   GmatBase    (Gmat::MEASUREMENT_MODEL, typeName, name),
   modelID     (0),
   ionoModelName (""),
   tropoModelName (""),
   lightTimeModelName (""),
   isIonoON (false),
   isTropoON (false),
   isLightTimeON (false),
   theStation  (NULL)

{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("MeasurementModel");
   measurementTypesAllowed.push_back("");
}

//------------------------------------------------------------------------------
//   MeasurementModel::MeasurementModel(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel::MeasurementModel(const MeasurementModel &mm) :
   GmatBase    (mm),
   modelID  (mm.modelID),
   ionoModelName (mm.ionoModelName),
   tropoModelName (mm.tropoModelName),
   lightTimeModelName (mm.lightTimeModelName),
   isIonoON (mm.isIonoON),
   isTropoON (mm.isTropoON),
   isLightTimeON (mm.isLightTimeON),
   numMeasurements (mm.numMeasurements),
   measurementNames (mm.measurementNames),
   measurementUnits (mm.measurementUnits),
   measurementTypesAllowed (mm.measurementTypesAllowed),
   ccvtr (mm.ccvtr),
   theStation		   (mm.theStation)

{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("MeasurementModel");
}

//------------------------------------------------------------------------------
//  MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
{
   if (this != &mm)
   {
      SetModelID( mm.GetModelID() );
   }
   return *this;
}

//------------------------------------------------------------------------------
//  MeasurementModel::~MeasurementModel()
//------------------------------------------------------------------------------
MeasurementModel::~MeasurementModel()
{
}

//------------------------------------------------------------------------------
//  std::ostream& operator<<(std::ostream& output, MeasurementModel &mm)
//------------------------------------------------------------------------------
std::ostream& operator<<(std::ostream& output, MeasurementModel &mm)
{

    output << mm.modelID << std::endl;

    return output;
}

//------------------------------------------------------------------------------
//  <friend>
//  std::istream& operator>>(std::istream& input, MeasurementModel &mm)
//------------------------------------------------------------------------------
std::istream& operator>>(std::istream& input, MeasurementModel &mm)
{
    input >> mm.modelID;

    return input;
}

//------------------------------------------------------------------------------
// void SetGroundStation(GroundStation* gs)
//------------------------------------------------------------------------------
/**
 * Set the ground station for this instance of the measurement model.
 *
 * @param mm The ground station that is assigned.
 */
//------------------------------------------------------------------------------
void MeasurementModel::SetGroundStation(GroundStation* gs)
{
    theStation = gs;
}

//------------------------------------------------------------------------------
// GroundStation* GetGroundStation()
//------------------------------------------------------------------------------
/**
 * Return the ground station for this instance of the measurement model.
 *
 * @return A pointer to the ground station.
 */
//------------------------------------------------------------------------------
GroundStation* MeasurementModel::GetGroundStation()
{
    return theStation;
}

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::Initialize() const
{
    // Initialize the coordinate converter
    CoordinateConverter ccvtr;

    return true;

}

// Accessors

//------------------------------------------------------------------------------
// Integer SetModelID() const
//------------------------------------------------------------------------------
/**
 * Sets the name of the measurement model.
 */
//------------------------------------------------------------------------------
void MeasurementModel::SetModelID(Integer mId)
{
   modelID = mId;
}

//------------------------------------------------------------------------------
// Integer GetModelID() const
//------------------------------------------------------------------------------
/**
 * Finds the model ID# of the measurement model.
 *
 * @return The model ID#.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetModelID() const
{
   return modelID;
}

//------------------------------------------------------------------------------
// Integer GetNumMeasurments() const
//------------------------------------------------------------------------------
/**
 * Finds the number of measurements returned by the model.
 *
 * @return The numMeasurements.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetNumMeasurements() const
{
   return numMeasurements;
}

//------------------------------------------------------------------------------
// std::string GetMeasurementNameText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Finds the name of thee measurements returned by the model.
 *
 * @return The measurementNames.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetMeasurementNameText(const Integer id) const
{
    if (id >= 0 && id < numMeasurements)
      return measurementNames[id];

    return "Unknown Measurement ID";
}

//------------------------------------------------------------------------------
// std::string MeasurementModel::GetMeasurementUnitText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Finds the units of the measurements returned by the model.
 *
 * @return The measurementUnits.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetMeasurementUnitText(const Integer id) const
{
    if (id >= 0 && id < numMeasurements)
      return measurementUnits[id];

    return "Unknown Measurement Unit ID";
}

//------------------------------------------------------------------------------
// Integer MeasurementModel::GetModelID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the model ID
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetModelID(const std::string &label)
{

   for (Integer i = 0; i < EndModelReps; i++)
   {
      if (label == MODEL_DESCRIPTIONS[i])
         return i;
   }

   // Return -1 indicating not found
   return -1;

}

//------------------------------------------------------------------------------
// const std::string* MeasurementModel::GetModelDescriptions() const
//------------------------------------------------------------------------------
const std::string* MeasurementModel::GetModelDescriptions() const
{
   return MODEL_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string MeasurementModel::GetModelNameText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the model name text corresponding to a model ID
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetModelNameText(const Integer &id) const
{
   if ((id >= 0) && (id < EndModelReps))
   {
      return MODEL_DESCRIPTIONS[id];
   }

   return "INVALID";
}

//------------------------------------------------------------------------------
// Real MeasurementModel::GetDegree(const Real angle, const Real minAngle, 
//                           const Real maxAngle) 
//------------------------------------------------------------------------------
Real MeasurementModel::GetDegree(const Real angle, const Real minAngle, 
                          const Real maxAngle) 
{
   Real angleInRange = GmatMathUtil::Mod(angle,GmatMathUtil::TWO_PI);
   
   if (angleInRange < minAngle)
      angleInRange += GmatMathUtil::TWO_PI;

   else if (angleInRange > maxAngle)
      angleInRange -= GmatMathUtil::TWO_PI;

   return GmatMathUtil::Deg(angleInRange);
}


//------------------------------------------------------------------------------
// bool GetTheMeasurements(const SpacePoint* theSpacePoint, const A1Mjd &atTime)
//------------------------------------------------------------------------------
bool MeasurementModel::GetTheMeasurements(SpacePoint* theSpacePoint,
                                          const A1Mjd &atTime,
                                          LaGenMatDouble &theMeasurements)
{
    return false;
}

//------------------------------------------------------------------------------
// bool GetThePartials(const std::string &param, const Integer &size,
//                     const SpacePoint* theSpacePoint, const A1Mjd &atTime)
//------------------------------------------------------------------------------
bool MeasurementModel::GetThePartials(const std::string &param,
                                      SpacePoint* theSpacePoint,
                                      const A1Mjd &atTime,
                                      LaGenMatDouble &theDerivatives)
{
    return GetThePartials(GetDependentParamID(param), theSpacePoint, atTime,
                          theDerivatives);
}

//------------------------------------------------------------------------------
// bool GetThePartials(const Integer paramID, const Integer &size,
//                     const SpacePoint* theSpacePoint, const A1Mjd &atTime)
//------------------------------------------------------------------------------
bool MeasurementModel::GetThePartials(const Integer &paramID,
                                      SpacePoint* theSpacePoint,
                                      const A1Mjd &atTime,
                                      LaGenMatDouble &theDerivatives)
{
    return false;
}


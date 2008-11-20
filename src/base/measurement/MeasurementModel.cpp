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
const std::string MeasurementModel::IONOSPHERE_MODEL_DESCRIPTIONS[EndIonoModelReps] =
{
    "International Reference Ionosphere 1990 (IRI90)",
    "International Reference Ionosphere 1995 (IRI95)",
    "International Reference Ionosphere 2001 (IRI01)",
    "International Reference Ionosphere 2007 (IRI07)",
    "Parameterized Real-time Ionospheric Specification Model (PRISM)",
    "Ionospheric Forecast Model (IFM)",
    "Coupled Ionosphere-Thermosphere Forecast Model (CITFM)",
    "Sami2 is Another Model of the Ionosphere (SAMI2)",
    "Sami3 is Another Model of the Ionosphere (SAMI3)",
    "Global Theoretical Ionospheric Model (GTIM)",
    "Field Line Interhemispheric Plasma Model (FLIP)",
    "USU model of the global ionosphere (USU)",
    "A Coupled Thermosphere-Ionosphere-Plasmasphere Model (CTIP)",
    "Thermosphere-Ionosphere-Mesosphere-Electrodynamic-General Circulation Model (TIME-GCM)"
};

const std::string MeasurementModel::TROPOSPHERE_MODEL_DESCRIPTIONS[EndTropoModelReps] =
{
    "Ifadis Model",
    "Niell Model",
    "Hopfield Modified Model",
    "Hopfiled Simplified Model",
    "Saastomoinen Model",
    "Differential Refraction Model",
    "Marini Model"
};
const std::string MeasurementModel::LIGHTTIME_MODEL_DESCRIPTIONS[EndLightTimeModelReps] =
{
    "Light Time Model"
};

const std::string MeasurementModel::MODEL_DESCRIPTIONS[EndModelReps] =
{
    "NotDefined",
    "Range",
    "RangeRate",
    "LightTime",
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
   if ((id >= GmatBaseParamCount) && (id < DataFileParamCount))
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
   for (Integer i = GmatBaseParamCount; i < DataFileParamCount; ++i)
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
 */
//------------------------------------------------------------------------------
Gmat::ParameterType MeasurementModel::GetParameterType(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < DataFileParamCount))
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
   if (id == FILENAME_ID)
      return dataFileName;

   if (id == FILEFORMAT_ID)
      return dataFormatID;
          
   return GmatBase::GetStringParameter(id);
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const Real value)
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
   if (id == FILENAME_ID)
   {
      dataFileName = value;
      return true;
   }

   if (id == FILEFORMAT_ID)
   {
      dataFormatID = GetDataFormatID(value);
      return true;
   }

   if (id == DATATYPESALLOWED_ID) {
	dataTypesAllowed.push_back(value);
        return true;
   }
 
   return GmatBase::SetStringParameter(id, value);
   
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
   if (id == DATATYPESALLOWED_ID)
      return dataTypesAllowed;
  
   return GmatBase::GetStringArrayParameter(id);
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer MeasurementModel::GetIntegerParameter(const Integer id) const
{
    if (id == NUMLINES_ID)
      return numLines;

    return GmatBase::GetIntegerParameter(id);
}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 *
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::SetIntegerParameter(const Integer id, const Integer value)
{

   if (id == NUMLINES_ID)
   {
         numLines = value;
         return value;
   }
   
   return GmatBase::SetIntegerParameter(id, value);

}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(std::string &label, const Integer value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::SetIntegerParameter(const std::string &label, const Integer value)
{
   return SetIntegerParameter(GetParameterID(label), value);
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
      MeasurementModel::GetRefObjectNameArray(const Gmat::ObjectType type)
{
//   MessageInterface::ShowMessage("Getting the list of data file objects\n");

//   // If we have more than one type, we'll want to do something like this:
//   if (type == Gmat::UNKNOWN_OBJECT)
//      Fill them all in here; otherwise fill in by type
      
   // I'm using a temporary StringArray in case we need additional ref objects
   tempNameArray = myDataFileNames;

   return tempNameArray;
}


//------------------------------------------------------------------------------
// GmatBase* GetRefObject(const Gmat::ObjectType type, const std::string &name)                             
//------------------------------------------------------------------------------
/**
 * This method returns a GmatBase pointer to the desired object.
 *
 * @return a GmatBase pointer.
 */
//------------------------------------------------------------------------------
GmatBase* MeasurementModel::GetRefObject(const Gmat::ObjectType type,
                                  const std::string &name)
{
   GmatBase* retval = NULL;

   if (type == Gmat::DATA_FILE)
   {
      for (ObjectArray::iterator i = myDataFiles.begin();
           i != myDataFiles.end(); ++i)
      {
         if ((*i)->GetName() == name)
         {
	    retval = *i;
            break;
         }
      }
   }

   if (retval != NULL)
      return retval;
   
   return GmatBase::GetRefObject(type, name);
}


//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
//                   const std::string &name)
//------------------------------------------------------------------------------
bool MeasurementModel::SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
                              const std::string &name)
{
   bool retval = false;

   if (obj->IsOfType(Gmat::DATA_FILE))
   {
      if (find(myDataFiles.begin(), myDataFiles.end(), obj) == myDataFiles.end())
      {
         myDataFiles.push_back(obj);
         retval = true;
      }
   }
          
    return retval;

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
   modelID     (0)
{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("MeasurementModel");
}

//------------------------------------------------------------------------------
//   MeasurementModel::MeasurementModel(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel::MeasurementModel(const MeasurementModel &mm) :
   GmatBase    (mm),
   modelID  (mm.modelID)
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

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
void MeasurementModel::Initialize() const
{
    CoordinateConverter ccvtr;
/*    
    if(dataFormat == "B3")
    {
	myData = new ProcessB3Data();
    }
    else if (dataFormat == "SLR")
    {
	myData = new ProcessSLRData();
    }
    else if (dataFormat == "TLE")
    {
	myData = new ProcessTLEData();
    }
    else
    {
      throw MeasurementModelException("Unable to process data with the specified data format: " + dataFormat);
      MessageInterface::ShowMessage(
            "Unable to process data with the\n specified data format: " + dataFormat);
    }
 
 */
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
// const StringArray& MeasurementModel::GetMeasurements()
//------------------------------------------------------------------------------
/**
 * Returns the measurements generated by the model.
 *
 * @return The measurements.
 */
//------------------------------------------------------------------------------
const Real* MeasurementModel::GetMeasurements() const
{
   return measurements;
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
   if (label == "Range") {
       return RANGE_ID;
   } else if (label == "RangeRate") {
       return RANGERATE_ID;
   } else if (label == "LightTime") {
       return LIGHTTIME_ID;
   } else if (label == "VariableTransmitterRange") {
       return VARIABLETRANSMITTERRANGE_ID;
   } else if (label == "AntennaTracking") {
       return ANTENNATRACKING_ID;
   } else if (label == "SunSensor") {
       return SUNSENSOR_ID;
   } else if (label == "StarSensor") {
       return STARSENSOR_ID;
   } else if (label == "GyroPackage") {
       return GYROPACKAGE_ID;
   } else if (label == "HorizonSensor") {
       return HORIZONSENSOR_ID;
   } else if (label == "Videometers") {
       return VIDEOMETERS_ID;
   } else if (label == "CoherentDoppler") {
       return COHERENTDOPPLER_ID;
   } else if (label == "NonCoherentDoppler") {
       return NONCOHERENTDOPPLER_ID;
   } else if (label == "VariableTransmitterDoppler") {
       return VARIABLETRANSMITTERDOPPLER_ID;
   } else if (label == "IntegratedDopplerCount") {
       return INTEGRATEDDOPPLERCOUNT_ID;
   } else if (label == "IMU") {
       return IMU_ID;
   } else if (label == "Magnetometer") {
       return MAGNETOMETER_ID;
   } else if (label == "AO_AzEl") {
       return AO_AZEL_ID;
   } else if (label == "RangeAzEl") {
       return RANGEAZEL_ID;
   } else if (label == "AO_RaDec") {
       return AO_RADEC_ID;
   } else if (label == "RangeRaDec") {
       return RANGERADEC_ID;
   } else
     return DEFAULT_ID;

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
// Integer ComputeMeasurement(ObjectArray participants,
//			      const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeMeasurement(ObjectArray participants,
					  LaVectorDouble &myMeasurements)
{
      return false;
}

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeMeasurement(GroundStation *theStation,
      Spacecraft *theSat, LaVectorDouble &myMeasurements)
{
      return false;
}

//------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(ObjectArray participants, 
//		const LaGenMatDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeCartesianPartialDerivative(
	    ObjectArray participants, LaGenMatDouble &myCartDerivatives)
{
    return false;
}

//------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaGenMatDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeCartesianPartialDerivative(
      GroundStation *theStation, Spacecraft *theSat, LaGenMatDouble &myCartDerivatives)
{
    return false;
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






//$Header$
//------------------------------------------------------------------------------
//                         SimMeasurementModel
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
 * Defines the simulated measurement models used for observer objects.
 */
//------------------------------------------------------------------------------

#include "SimMeasurementModel.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string SimMeasurementModel::MODEL_DESCRIPTIONS[NUM_MODELS] =
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
DataFile::PARAMETER_TEXT[SimMeasurementModelParamCount - MeasurementModelParamCount] =
{
   "SimErrorModel",
   "SimMeanError",
   "SimStdDevError",
   "SimPropagator",
   "SimComputeParticipants",
   "SimObservationParticipants",
   "SimStartTime",
   "SimStopTime",
   "SimStepSize"
};


const Gmat::ParameterType
DataFile::PARAMETER_TYPE[SimMeasurementModelParamCount - MeasurementModelParamCount] =
{
   Gmat::OBJECT_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::OBJECTARRAY_TYPE,
   Gmat::OBJECTARRAY_TYPE,
   Gmat::TIME_TYPE,
   Gmat::TIME_TYPE,
   Gmat::REAL_TYPE
};

//---------------------------------
// methods overridden from GMAT base
//---------------------------------

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the SimMeasurementModel.
 *
 * @return clone of the SimMeasurementModel.
 */
//------------------------------------------------------------------------------
GmatBase* SimMeasurementModel::Clone() const
{
   GmatBase *clone = new SimMeasurementModel(*this);
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
void SimMeasurementModel::Copy(const GmatBase* orig)
{
   operator=(*((SimMeasurementModel *)(orig)));
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
      SimMeasurementModel::GetRefObjectNameArray(const Gmat::ObjectType type)
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
GmatBase* SimMeasurementModel::GetRefObject(const Gmat::ObjectType type,
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
bool SimMeasurementModel::SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
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
//  SimMeasurementModel::SimMeasurementModel()
//------------------------------------------------------------------------------
/**
 * Constructs base SimMeasurementModel structures
 */
SimMeasurementModel::SimMeasurementModel(const std::string typeName,
      const std::string name) :
   GmatBase    (Gmat::MEASUREMENT_MODEL, typeName, name),
   modelID     (0)
{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("SimMeasurementModel");
}

//------------------------------------------------------------------------------
//   SimMeasurementModel::SimMeasurementModel(const SimMeasurementModel &mm)
//------------------------------------------------------------------------------
SimMeasurementModel::SimMeasurementModel(const SimMeasurementModel &mm) :
   GmatBase    (mm),
   modelID  (mm.modelID)
{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("SimMeasurementModel");
}

//------------------------------------------------------------------------------
//  SimMeasurementModel& SimMeasurementModel::operator=(const SimMeasurementModel &mm)
//------------------------------------------------------------------------------
SimMeasurementModel& SimMeasurementModel::operator=(const SimMeasurementModel &mm)
{
   if (this != &mm)
   {
      SetModelID( mm.GetModelID() );
   }
   return *this;
}

//------------------------------------------------------------------------------
//  SimMeasurementModel::~SimMeasurementModel()
//------------------------------------------------------------------------------
SimMeasurementModel::~SimMeasurementModel()
{
}

//------------------------------------------------------------------------------
//  std::ostream& operator<<(std::ostream& output, SimMeasurementModel &mm)
//------------------------------------------------------------------------------
std::ostream& operator<<(std::ostream& output, SimMeasurementModel &mm)
{

    output << mm.modelID << std::endl;

    return output;
}

//------------------------------------------------------------------------------
//  <friend>
//  std::istream& operator>>(std::istream& input, SimMeasurementModel &mm)
//------------------------------------------------------------------------------
std::istream& operator>>(std::istream& input, SimMeasurementModel &mm)
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
void SimMeasurementModel::Initialize() const
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
void SimMeasurementModel::SetModelID(Integer mId)
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
Integer SimMeasurementModel::GetModelID() const
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
Integer SimMeasurementModel::GetNumMeasurements() const
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
std::string SimMeasurementModel::GetMeasurementNameText(const Integer id) const
{
    if (id >= 0 && id < numMeasurements)
      return measurementNames[id];

    return "Unknown Measurement ID";
}

//------------------------------------------------------------------------------
// std::string SimMeasurementModel::GetMeasurementUnitText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Finds the units of the measurements returned by the model.
 *
 * @return The measurementUnits.
 */
//------------------------------------------------------------------------------
std::string SimMeasurementModel::GetMeasurementUnitText(const Integer id) const
{
    if (id >= 0 && id < numMeasurements)
      return measurementUnits[id];

    return "Unknown Measurement Unit ID";
}
//------------------------------------------------------------------------------
// const StringArray& SimMeasurementModel::GetMeasurements()
//------------------------------------------------------------------------------
/**
 * Returns the measurements generated by the model.
 *
 * @return The measurements.
 */
//------------------------------------------------------------------------------
const Real* SimMeasurementModel::GetMeasurements() const
{
   return measurements;
}

//------------------------------------------------------------------------------
// Integer SimMeasurementModel::GetModelID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the model ID
 */
//------------------------------------------------------------------------------
Integer SimMeasurementModel::GetModelID(const std::string &label)
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
// const std::string* SimMeasurementModel::GetModelDescriptions() const
//------------------------------------------------------------------------------
const std::string* SimMeasurementModel::GetModelDescriptions() const
{
   return MODEL_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string SimMeasurementModel::GetModelNameText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the model name text corresponding to a model ID
 */
//------------------------------------------------------------------------------
std::string SimMeasurementModel::GetModelNameText(const Integer &id) const
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
bool SimMeasurementModel::ComputeMeasurement(ObjectArray participants,
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
bool SimMeasurementModel::ComputeMeasurement(GroundStation *theStation,
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
bool SimMeasurementModel::ComputeCartesianPartialDerivative(
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
bool SimMeasurementModel::ComputeCartesianPartialDerivative(
      GroundStation *theStation, Spacecraft *theSat, LaGenMatDouble &myCartDerivatives)
{
    return false;
}

//------------------------------------------------------------------------------
// Real SimMeasurementModel::GetDegree(const Real angle, const Real minAngle, 
//                           const Real maxAngle) 
//------------------------------------------------------------------------------
Real SimMeasurementModel::GetDegree(const Real angle, const Real minAngle, 
                          const Real maxAngle) 
{
   Real angleInRange = GmatMathUtil::Mod(angle,GmatMathUtil::TWO_PI);
   
   if (angleInRange < minAngle)
      angleInRange += GmatMathUtil::TWO_PI;

   else if (angleInRange > maxAngle)
      angleInRange -= GmatMathUtil::TWO_PI;

   return GmatMathUtil::Deg(angleInRange);
}






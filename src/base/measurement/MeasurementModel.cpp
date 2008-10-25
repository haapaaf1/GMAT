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
 
#include <valarray>
#include "Observer.hpp"
#include "Rvector6.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"
#include "TimeSystemConverter.hpp"
#include "StateConverter.hpp"
#include <map>

//---------------------------------
//  static data
//---------------------------------
const std::string MeasurementModel::MODEL_DESCRIPTIONS[NUM_MODELS] =
{
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

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
//  MeasurementModel::MeasurementModel() 
//------------------------------------------------------------------------------
/**
 * Constructs base MeasurementModel structures 
 */
MeasurementModel::MeasurementModel() :
    modelName  (std::string(""))
{
}

//------------------------------------------------------------------------------
//   MeasurementModel::MeasurementModel(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel::MeasurementModel(const MeasurementModel &mm) :
  modelName  (mm.modelName)
{
}

//------------------------------------------------------------------------------
//  MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
{
   if (this != &mm)
   {
      SetName( mm.GetName() );
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

    output << mm.modelName << std::endl;

    return output;
}

//------------------------------------------------------------------------------
//  <friend>
//  std::istream& operator>>(std::istream& input, MeasurementModel &mm)
//------------------------------------------------------------------------------
std::istream& operator>>(std::istream& input, MeasurementModel &mm)
{
    input >> mm.modelName;

    return input;
}

// Accessors

//------------------------------------------------------------------------------
// Integer GetName() const
//------------------------------------------------------------------------------
/**
 * Finds the name of the measurement model vector.
 *
 * @return The model name.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetName() const
{
   return modelName;
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
      return MeasurementModel::measurementNames[id];

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
      return MeasurementModel::measurementUnits[id];
   
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
const Rvector& MeasurementModel::GetMeasurements()
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
   Integer retval = -1;
   if (!strcmp(label.c_str(),"Range")) {
       return RANGE_ID;
   } else if (!strcmp(label.c_str(),"RangeRate")) {
       return RANGERATE_ID;
   } else if (!strcmp(label.c_str(),"LightTime")) {
       return LIGHTTIME_ID;
   } else if (!strcmp(label.c_str(),"VariableTransmitterRange")) {
       return VARIABLETRANSMITTERRANGE_ID;
   } else if (!strcmp(label.c_str(),"AntennaTracking")) {
       return ANTENNATRACKING_ID;
   } else if (!strcmp(label.c_str(),"SunSensor")) {
       return SUNSENSOR_ID;
   } else if (!strcmp(label.c_str(),"StarSensor")) {
       return STARSENSOR_ID;
   } else if (!strcmp(label.c_str(),"GyroPackage")) {
       return GYROPACKAGE_ID;
   } else if (!strcmp(label.c_str(),"HorizonSensor")) {
       return HORIZONSENSORS_ID;
   } else if (!strcmp(label.c_str(),"Videometers")) {
       return VIDEOMETERS_ID;
   } else if (!strcmp(label.c_str(),"CoherentDoppler")) {
       return COHERENTDOPPLER_ID;
   } else if (!strcmp(label.c_str(),"NonCoherentDoppler")) {
       return NONCOHERENTDOPPLER_ID;
   } else if (!strcmp(label.c_str(),"VariableTransmitterDoppler")) {
       return VARIABLETRANSMITTERDOPPLER_ID;
   } else if (!strcmp(label.c_str(),"IntegratedDopplerCount")) {
       return INTEGRATEDDOPPLERCOUNT_ID;
   } else if (!strcmp(label.c_str(),"IMU")) {
       return IMU_ID;
   } else if (!strcmp(label.c_str(),"Magnetometer")) {              
       return MAGNETOMETER_ID;
   } else if (!strcmp(label.c_str(),"AO_AzEl")) {
       return AO_AZEL_ID;
   } else if (!strcmp(label.c_str(),"RangeAzEl")) {
       return RANGEAZEL_ID;
   } else if (!strcmp(label.c_str(),"AO_RaDec")) {
       return AO_RADEC_ID;
   } else if (!strcmp(label.c_str(),"RangeRaDec")) {
       return RANGERADEC_ID;
   } else
     return retval;

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
// Integer ComputeMeasurement(const GroundStation &theStation, 
//		const Spacecraft &theSat, const Rvector &myMeasurements); 
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
  virtual Integer MeasurementModel::ComputeMeasurement(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myMeasurements)
  {
      return false;
  }
  
  //------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(const GroundStation &theStation, 
//		const Spacecraft &theSat, const Rvector &myMeasurements); 
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------

  virtual Integer MeasurementModel::ComputeCartesianPartialDerivative(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myCartDerivatives);
  {
      return false;
  }
  
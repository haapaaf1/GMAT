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
// Real MeasurementModel::GetMeasurement(std::string modName)
//
// This routine will compute the measurement between the observation station
// and the object of interest.
//
//------------------------------------------------------------------------------
void MeasurementModel::GetMeasurement(std::string modName)
{
 
  Integer modelID = GetModelID(modName);
  
  switch(modelID) {
    
    case RANGE_ID:

      Range();
      
      break;
      
    case RANGERATE_ID:

      RangeRate();
      
      break;
      
    case AO_RADEC_ID:

      AO_RaDec();
      
      break;
      
    case RANGERADEC_ID:

      RangeRaDec();
      
      break;
      
    case AO_AZEL_ID:

      AO_AzEl();
      
      break;
      
    case RANGEAZEL_ID:

      RangeAzEl();
      
      break;
      
    case LIGHTTIME_ID:

      LightTime();
      
      break;
      
    default:

      UtilityException ex;
      
      ex.SetDetails("The model you specified (%s) has not yet been implemented", modName.c_str());
      throw ex;
      
  }  

}


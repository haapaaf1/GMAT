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
#ifndef _MEASUREMENTMODEL_HPP
#define	_MEASUREMENTMODEL_HPP

#include <valarray>
#include "Observer.hpp"
#include "Rvector6.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"
#include "TimeSystemConverter.hpp"
#include "StateConverter.hpp"
#include <map>

class GMAT_API MeasurementModel
{
    
public:
    
  MeasurementModel();
  MeasurementModel(const MeasurementModel &MeasurementModel);
  MeasurementModel& operator=(const MeasurementModel &MeasurementModel);
  virtual ~MeasurementModel();  

  // Friend function
  friend std::ostream& operator<<(std::ostream& output, MeasurementModel &mm);
  friend std::istream& operator>>(std::istream& input, MeasurementModel &mm;

  
  const std::string* GetModelDescriptions() const;
  std::string GetModelNameText(const Integer &id) const;
  Integer GetModelID(const std::string &label);
  
  Integer GetName() const;
  Integer GetNumMeasurements() const;
  std::string GetMeasurementNameText(Integer id) const;
  std::string GetMeasurementUnitText(Integer id) const;
  const Rvector& GetMeasurements();
  
  Integer GetMeasurements() const;	  
	
  virtual Integer MeasurementModel::ComputeMeasurement(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myMeasurements);
  virtual Integer MeasurementModel::ComputeCartesianPartialDerivative(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myCartDerivatives);
  
  
private:

  static const Integer NUM_MODELS = 20;
  static const std::string MODEL_DESCRIPTIONS[NUM_MODELS];
    
protected:
  
    enum MODEL_REPS {
    RANGE_ID = 0,
    RANGERATE_ID,
    LIGHTTIME_ID,
    VARIABLETRANSMITTERRANGE_ID,
    ANTENNATRACKING_ID,
    SUNSENSOR_ID,
    STARSENSOR_ID,
    GYROPACKAGE_ID,
    HORIZONSENSOR_ID,
    VIDEOMETERS_ID,
    COHERENTDOPPLER_ID,
    NONCOHERENTDOPPLER_ID,
    VARIABLETRANSMITTERDOPPLER_ID,
    INTEGRATEDDOPPLERCOUNT_ID,
    IMU_ID,
    MAGNETOMETER_ID,
    AO_AZEL_ID,
    RANGEAZEL_ID,
    AO_RADEC_ID,
    RANGERADEC_ID,
    EndModelReps
            
  };
  
  // Name of the measurement model being used
  std::string modelName;
  // Total number of measurements returned by the model
  int numMeasurements;
  // Name of the measurements returned
  std::string measurementNames[numMeasurements];
  // Units of each measurement returned
  std::string measurementUnits[numMeasurements];
  // Measurement returned by the model
  Rvector measurements[numMeasurements];

  CoordinateConverter ccvtr;  

  
}


#endif	/* _MEASUREMENTMODEL_HPP */


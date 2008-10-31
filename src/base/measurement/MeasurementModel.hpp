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

#include "GmatBase.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"
#include "TimeSystemConverter.hpp"
#include "StateConverter.hpp"
#include "GroundStation.hpp"
#include "Spacecraft.hpp"
#include "RealUtilities.hpp" // For sin, cos, etc etc
#include "lapackpp.h"

class GMAT_API MeasurementModel : public GmatBase
{

public:

  MeasurementModel(const std::string typeName, const std::string name = "");
  MeasurementModel(const MeasurementModel &MeasurementModel);
  MeasurementModel& operator=(const MeasurementModel &MeasurementModel);
  virtual ~MeasurementModel();

  // Friend function
  friend std::ostream& operator<<(std::ostream& output, MeasurementModel &mm);
  friend std::istream& operator>>(std::istream& input, MeasurementModel &mm);

  virtual void Initialize() const;
  
  const std::string* GetModelDescriptions() const;
  std::string GetModelNameText(const Integer &id) const;
  Integer GetModelID(const std::string &label);

  void SetModelID(Integer mName);
  Integer GetModelID() const;
  Integer GetNumMeasurements() const;
  std::string GetMeasurementNameText(Integer id) const;
  std::string GetMeasurementUnitText(Integer id) const;
  const Real* GetMeasurements() const;

  // Compute measurements
  virtual bool ComputeMeasurement(ObjectArray participants, 
				  LaVectorDouble &myMeasurements);
  virtual bool ComputeMeasurement(GroundStation *theStation,
        Spacecraft *theSat, LaVectorDouble &myMeasurements);
 
  // Compute partial derivatives
  virtual bool ComputeCartesianPartialDerivative(ObjectArray participants, 
				  LaGenMatDouble &myMeasurements);

   virtual bool ComputeCartesianPartialDerivative(
        GroundStation *theStation, Spacecraft *theSat,
        LaGenMatDouble &myCartDerivatives);


private:

  static const Integer NUM_MODELS = 21;
  static const std::string MODEL_DESCRIPTIONS[NUM_MODELS];

protected:

    enum MODEL_REPS {
	DEFAULT_ID = 0,
	RANGE_ID,
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
  Integer modelID;
  // Total number of measurements returned by the model
  Integer numMeasurements;
  // Name of the measurements returned
  std::string* measurementNames;
  // Units of each measurement returned
  std::string* measurementUnits;
  // Measurement returned by the model
  Real* measurements;

  CoordinateConverter ccvtr;

  Real GetDegree(const Real angle, const Real minAngle, 
			    const Real maxAngle);

};


#endif	/* _MEASUREMENTMODEL_HPP */


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
#include "MeasurementModelException.hpp"
#include "ObType.hpp"
//#include "DataFile.hpp"
//#include "ProcessB3Data.hpp"
//#include "ProcessSLRData.hpp"
//#include "ProcessTLEData.hpp"

// Forward references for GMAT core objects
class Moderator;
class GroundStation;

class GMAT_API MeasurementModel : public GmatBase
{

public:

  MeasurementModel(const std::string typeName, const std::string name = "");
  MeasurementModel(const MeasurementModel &MeasurementModel);
  MeasurementModel& operator=(const MeasurementModel &MeasurementModel);
  virtual ~MeasurementModel();

  virtual std::string     GetParameterText(const Integer id) const;
  virtual Integer         GetParameterID(const std::string &str) const;
  virtual Gmat::ParameterType
                          GetParameterType(const Integer id) const;
  virtual Integer     GetDependentParamID(const std::string &str) const;
  
  // Friend function
  friend std::ostream& operator<<(std::ostream& output, MeasurementModel &mm);
  friend std::istream& operator>>(std::istream& input, MeasurementModel &mm);

  // Methods overridden from the GmatBase clase
  virtual GmatBase *Clone() const;
  virtual void      Copy(const GmatBase* orig);      

  std::string GetStringParameter(const Integer id) const;
  bool SetStringParameter(const Integer id, const std::string &value);
  const StringArray& GetStringArrayParameter(const Integer id) const;
  bool GetBooleanParameter(const Integer id) const;
  bool SetBooleanParameter(const Integer id, const bool &value);
   
  virtual bool Initialize() const;

  friend class DataFile;
  
  const std::string* GetModelDescriptions() const;
  std::string GetModelNameText(const Integer &id) const;
  Integer GetModelID(const std::string &label);

  void SetModelID(Integer mName);
  Integer GetModelID() const;
  Integer GetNumMeasurements() const;
  std::string GetMeasurementNameText(Integer id) const;
  std::string GetMeasurementUnitText(Integer id) const;

  void SetGroundStation(GroundStation* gs);
  GroundStation* GetGroundStation();

  // Obtain the measurements

  // This function takes a spacecraft and computes a measurement
  // between the associated ground station a space point
  virtual bool GetTheMeasurements(SpacePoint* theSpacePoint,
                                  const A1Mjd &atTime,
                                  LaGenMatDouble &theMeasurements);

  // Obtain the partials
  virtual bool GetThePartials(const std::string &param,
                              SpacePoint* theSpacePoint,
                              const A1Mjd &atTime,
                              LaGenMatDouble &theDerivatives);
  virtual bool GetThePartials(const Integer &paramID,
                              SpacePoint* theSpacePoint,
                              const A1Mjd &atTime,
                              LaGenMatDouble &theDerivatives);
   
protected:
    
    enum MODEL_REPS
    {
	DEFAULT_ID = 0,
	RANGE_ID,
	RANGERATE_ID,
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
   
   enum
   {
      DATASOURCE_ID   = GmatBaseParamCount,
      MEASUREMENTTYPES_ID,
      LIGHTTIMEFLAG_ID,
      IONOSPHEREFLAG_ID,
      TROPOSPHEREFLAG_ID,
      LIGHTTIME_ID,
      IONOSPHERE_ID,
      TROPOSPHERE_ID,
      MeasurementModelParamCount
   };

   enum DEPENDENT_PARAMS
   {
      MMDependentParamCount = 0,
   };


   static const std::string    DEPENDENT_PARAMETER_TEXT[MMDependentParamCount];

   static const std::string    PARAMETER_TEXT[MeasurementModelParamCount -
                                              GmatBaseParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[MeasurementModelParamCount -
                                              GmatBaseParamCount];
   
   static const std::string MODEL_DESCRIPTIONS[EndModelReps];

  // Name of the measurement model being used
  Integer modelID;
  // Name of the measurement model being used
  std::string ionoModelName;
  // Name of the measurement model being used
  std::string tropoModelName;
  // Name of the measurement model being used
  std::string lightTimeModelName;
  
  // Flag for corrections
  bool isIonoON;
  bool isTropoON;
  bool isLightTimeON;
  
  // Total number of measurements returned by the model
  Integer numMeasurements;
  // Name of the measurements returned
  StringArray measurementNames;
  // Units of each measurement returned
  StringArray measurementUnits;

  // This array of datatypes can be used to select a specific
  // subset of available data from a given dataFormat
  // TODO: Should all measurement types be default ON or OFF?
  StringArray measurementTypesAllowed;
  
  CoordinateConverter ccvtr;

  Real GetDegree(const Real angle, const Real minAngle, 
			    const Real maxAngle);
    
  // TODO: Is this the best way for the measurement model
  // to know what ground station it is associated with?
  GroundStation* theStation;

};


#endif	/* _MEASUREMENTMODEL_HPP */


//$Id$
//------------------------------------------------------------------------------
//                                 GroundStation
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
//
/**
 * Definition of the Ground Station class base
 */
//------------------------------------------------------------------------------

#ifndef GroundStation_hpp
#define GroundStation_hpp

#include <valarray>
#include "Observer.hpp"
#include "Rvector6.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"
#include "TimeSystemConverter.hpp"
#include "StateConverter.hpp"
#include <map>

class GMAT_API GroundStation : public Observer
{
public:
   GroundStation(const std::string &name, 
      const std::string &typeStr = "GroundStation");
   GroundStation(const GroundStation &a);
   GroundStation&          operator=(const GroundStation &a);
   
   // Destructor
   virtual              ~GroundStation();
         
   virtual bool         Initialize();
   
   virtual const std::string&  
                        GetGeneratingString(Gmat::WriteMode mode = Gmat::SCRIPTING,
                                            const std::string &prefix = "",
                                            const std::string &useName = "");
   
   std::string GetEpochString();
   void SetDateFormat(const std::string &dateType);   
   void SetEpoch(const std::string &ep);
   void SetEpoch(const std::string &type, const std::string &ep, Real a1mjd);
   void SetState(const std::string &type, const Rvector6 &cartState);
   void SetAnomaly(const std::string &type, const Anomaly &ta);

   
protected:

   enum GS_Param_ID 
   {
      GS_EPOCH_ID = GroundStationObjectParamCount,
      ELEMENT1_ID, 
      ELEMENT2_ID, 
      ELEMENT3_ID, 
      ELEMENT4_ID, 
      ELEMENT5_ID, 
      ELEMENT6_ID, 
      ELEMENT1UNIT_ID, 
      ELEMENT2UNIT_ID, 
      ELEMENT3UNIT_ID, 
      ELEMENT4UNIT_ID, 
      ELEMENT5UNIT_ID, 
      ELEMENT6UNIT_ID, 
      STATE_TYPE_ID,           // deprecated
      DISPLAY_STATE_TYPE_ID, 
      COORD_SYS_ID,
      DATE_FORMAT_ID, 
      ATTITUDE,
      GroundStationParamCount
   };
   
   enum MultipleReps  // these are IDs for the different representations
   {
      CART_X = 10000,      // Cartesian
      CART_Y,
      CART_Z,
      CART_VX,
      CART_VY,
      CART_VZ,
      AZFPA_RMAG,          // SphericalAZFPA
      AZFPA_RA,
      AZFPA_DEC,
      AZFPA_VMAG,
      AZFPA_AZI,
      AZFPA_FPA,
      RADEC_RAV,           // SphericalRADEC
      RADEC_DECV,
      LATLONHGT_LAT,
      LATLONHGT_LON,
      LATLONHGT_HGT,
      EndMultipleReps    
   };

   // these are the corresponding strings
   static const std::string MULT_REP_STRINGS[EndMultipleReps - CART_X];

   /// GroundStation parameter types
   static const Gmat::ParameterType 
                  PARAMETER_TYPE[GroundStationParamCount - GroundStationObjectParamCount];
   /// GroundStation parameter labels
   static const std::string 
                  PARAMETER_LABEL[GroundStationParamCount - GroundStationObjectParamCount];
   
   enum STATE_REPS
   {
      CARTESIAN_ID = 0,
      SPHERICAL_AZFPA_ID,
      SPHERICAL_RADEC_ID,
      LATLONHGT_ID
   };
   
   // for non-internal spacecraft information
   StateConverter    stateConverter;
   CoordinateConverter coordConverter;
   
   // New constructs needed to preserve interfaces
   Rvector6          rvState;

   // protected methods

   bool              initialDisplay;
   bool              csSet;

   virtual void      WriteParameters(Gmat::WriteMode mode, std::string &prefix, 
                        std::stringstream &stream);
                                
   virtual void      UpdateElementLabels();
   Rvector6          GetStateInRepresentation(std::string rep = "");
   Rvector6          GetStateInRepresentation(Integer rep = CARTESIAN_ID);
   void              SetStateFromRepresentation(std::string rep, Rvector6 &st);
   
   Real              GetElement(const std::string &label);
   bool              SetElement(const std::string &label, const Real &value);
   Integer           LookUpLabel(const std::string &label, std::string &rep);
   Integer           LookUpID(const Integer id, std::string &label, std::string &rep);
   void              BuildElementLabelMap();

  // These variables allow us to flag which measurement model to use and
  // what data format the I/O of the sensor should be expected
  
  std::string       measurementModel;
  std::string       inputDataFormat;
  std::string       outputDataFormat;
  
  // These variable help define which way the ground station sensor is pointing

  Real                  sensorAzimuth;
  Real                  sensorElevation;
  Real                  sensorRightAscension;
  Real                  sensorDeclination;

  // These variables help define the limits of the sensor data collection capabilities

  Rvector               sensorMaskAzimuth;
  Rvector               sensorMaskElevation;
  Rvector               sensorMaskRightAscension;
  Rvector               sensorMaskDeclination;
  Rvector               sensorAreaMaskAzimuth;
  Rvector               sensorAreaMaskElevation;
  Rvector               sensorAreaMaskRightAscension;
  Rvector               sensorAreaMaskDeclination;

  // These variables define the rate at which the sensor can send and receive signals

  Real                  transmitFrequency;
  Real                  receiveFrequency;

  virtual Real frequencyRampingFunction();

  // These are the possible bias terms needed for orbit determination

  Real                  longitudeBias;
  Real                  latitudeBias;
  Real                  geodeticHeightBias;
  Real                  geocentricHeightBias;
  Real                  sensorAzimuthBias;
  Real                  sensorElevationBias;
  Real                  sensorRightAscensionBias;
  Real                  sensorDeclinationBias;
  Real                  sensorTransmitFrequencyBias;
  Real                  sensorReceiveFrequencyBias;
  Real                  sensorFrequencyRampingFunctionBias;
  Real                  sensorTimeBias;

  // These are the possible standard deviation terms needed for orbit determination

  Real                  longitudeSTD;
  Real                  latitudeSTD;
  Real                  geodeticHeightSTD;
  Real                  geocentricHeightSTD;
  Real                  sensorAzimuthSTD;
  Real                  sensorElevationSTD;
  Real                  sensorRightAscensionSTD;
  Real                  sensorDeclinationSTD;
  Real                  sensorTransmitFrequencySTD;
  Real                  sensorReceiveFrequencySTD;
  Real                  sensorFrequencyRampingFunctionSTD;
  Real                  sensorTimeSTD;

};

#endif // GroundStation_hpp

//------------------------------------------------------------------------------
//                            MeasurementModelFactory
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
 *  Implementation code for the MeasurementModelFactory class, responsible 
 *  for creating MeasurementModel objects.
 */
//------------------------------------------------------------------------------

#ifndef _MEASUREMENTMODELFACTORY_HPP
#define	_MEASUREMENTMODELFACTORY_HPP

#include "Factory.hpp"

// These measurment data types were taken from the CCSDS Report
  // concerning navigation data - definitions and conventions
    
  class Range();                    // km
  //class RangeRate();                // km/s
  //class LightTime();                // s
  //class VariableTransmitterRange(); // Range units
  //class AntennaTracking();          // deg
  //class SunSensor();                // deg
  //class StarSensor();               // deg
  //class GyroPackage();
  //class HorizonSensor();
  //class Videometers();
  //class CoherentDoppler();          // Hz
  //class NonCoherentDoppler();       // Hz
  //class VariableTransmitterDoppler(); // Hz
  //class IntegratedDopplerCount();   // Hz
  //class IMU();                      // deg/s
  //class Magnetometer();             // microTessla
    
  // A few more that might come in handy
    
  //class AO_AzEl();
  //class RangeAzEl();
  class AO_RaDec();
  class RangeRaDec();
    

class MeasurementModelFactory : public Factory
{
public:
   virtual MeasurementModel* CreateMeasurementModel(const std::string &ofType,
                                const std::string &withName /* = "" */);
   
   // default constructor
   MeasurementModelFactory();
   // constructor
   MeasurementModelFactory(StringArray createList);
   // copy constructor
   MeasurementModelFactory(const MeasurementModelFactory& fact);
   // assignment operator
   MeasurementModelFactory& operator=(const MeasurementModelFactory& fact);

   virtual ~MeasurementModelFactory();
   
};

#endif	/* _MEASUREMENTMODELFACTORY_HPP */

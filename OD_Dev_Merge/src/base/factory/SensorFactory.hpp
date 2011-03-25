//$Header$
//------------------------------------------------------------------------------
//                            SensorFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/11/30
//
/**
 *  Implementation code for the SensorFactory class, responsible
 *  for creating Sensor objects.
 */
//------------------------------------------------------------------------------

#ifndef SensorFactory_hpp
#define SensorFactory_hpp

#include "Factory.hpp"

// Forward References for the supported Sensors
// class MechanicalRadar
// class SpaceFence
// class

class SensorFactory : public Factory
{
public:
   virtual Sensor* CreateSensor(const std::string &ofType,
                                const std::string &withName /* = "" */);

   // default constructor
   SensorFactory();
   // constructor
   SensorFactory(StringArray createList);
   // copy constructor
   SensorFactory(const SensorFactory& fact);
   // assignment operator
   SensorFactory& operator=(const SensorFactory& fact);

   virtual ~SensorFactory();

};

#endif	/* _SENSOR_HPP */


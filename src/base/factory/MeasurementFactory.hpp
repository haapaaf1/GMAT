//$Id$
//------------------------------------------------------------------------------
//                         MeasurementFactory
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/07/09
//
/**
 * MeasurementFactory class definition
 */
//------------------------------------------------------------------------------


#ifndef MeasurementFactory_hpp
#define MeasurementFactory_hpp

#include "Factory.hpp"
#include "GeometricMeasurement.hpp" // eventually "CoreMeasurement.hpp"


/**
 * Factory class used to create core measurement objects
 */
class MeasurementFactory : public Factory
{
public:
   MeasurementFactory();
   virtual ~MeasurementFactory();
   MeasurementFactory(StringArray createList);
   MeasurementFactory(const MeasurementFactory& fact);
   MeasurementFactory& operator= (const MeasurementFactory& fact);

   CoreMeasurement *CreateMeasurement(const std::string &ofType,
         const std::string &withName = "");
};

#endif /* MeasurementFactory_hpp */

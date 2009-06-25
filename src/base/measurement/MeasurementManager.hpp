//$Id$
//------------------------------------------------------------------------------
//                         MeasurementManager
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/24
//
/**
 * MeasurementManager declaration used in GMAT's estimators and simulator
 */
//------------------------------------------------------------------------------

#ifndef MeasurementManager_hpp
#define MeasurementManager_hpp

#include "gmatdefs.hpp"
#include "EstimationDefs.hpp"
#include "MeasurementModel.hpp"

/**
 * The mediator between the estimators/simulator and measurement models. 
 */
class MeasurementManager
{
public:
   MeasurementManager();
   virtual ~MeasurementManager();
   MeasurementManager(const MeasurementManager &mm);
   MeasurementManager& operator=(const MeasurementManager &mm);
   
   Integer Calculate(const Integer measurementToCalc);
   const MeasurementData* GetMeasurement(const Integer measurementToGet);
   bool WriteMeasurement(const Integer measurementToWrite);

protected:

};

#endif /*MeasurementManager_hpp*/

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

// We'll want something like this eventually:
//#include "MeasurementBase.hpp"
// But for now we go with this:
#include "GeometricMeasurement.hpp"

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
   
   bool CalculateMeasurements();
   bool CalculateMeasurementsAndDerivatives();
   bool WriteMeasurements();

   Integer Calculate(const Integer measurementToCalc);
   Integer AddMeasurement(MeasurementModel *meas);
   const MeasurementData* GetMeasurement(const Integer measurementToGet);
   bool WriteMeasurement(const Integer measurementToWrite);

protected:
   /// List of the managed measurement models
   StringArray                      modelNames;
   /// Pointers to the measurements
   std::vector<MeasurementModel*>   models;

};

#endif /*MeasurementManager_hpp*/

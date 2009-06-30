//$Id$
//------------------------------------------------------------------------------
//                          MeasurementModel
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
 * MeasurementModel declaration used in GMAT's estimators and simulator
 */
//------------------------------------------------------------------------------

#ifndef MeasurementModel_hpp
#define MeasurementModel_hpp

#include "GmatBase.hpp"
#include "EstimationDefs.hpp"
#include "GeometricMeasurement.hpp"

class MeasurementModel : public GmatBase
{
public:
   MeasurementModel(const std::string &nomme = "");
   virtual ~MeasurementModel();
   MeasurementModel(const MeasurementModel &mm);
   MeasurementModel& operator=(const MeasurementModel &mm);

   virtual const MeasurementData &CalculateMeasurement();
   virtual const MeasurementData &CalculateMeasurementDerivatives();

protected:
   /// Accumulated list of participants used in the contained measurement
   StringArray          participants;
   /// The core measurement component
   CoreMeasurement      *measurement;
};

#endif /* MeasurementModel_hpp */

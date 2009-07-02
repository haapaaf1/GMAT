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

   virtual GmatBase* Clone() const;

   virtual const MeasurementData &CalculateMeasurement();
   virtual const MeasurementData &CalculateMeasurementDerivatives();

protected:
   /// Accumulated list of participants used in the contained measurement
   StringArray          participants;
   /// The core measurement component
   CoreMeasurement      *measurement;
   /// Current measurement data
   MeasurementData      currentMeasurement;

   enum
   {
       MeasurementType = GmatBaseParamCount,
       ParticipantNames,
       Bias,
       NoiseSigma,
       TimeConstant,
       MeasurementModelParamCount
   };

   // Start with the parameter IDs and associates strings
   static const std::string
                PARAMETER_TEXT[MeasurementModelParamCount - GmatBaseParamCount];
   static const Gmat::ParameterType
                PARAMETER_TYPE[MeasurementModelParamCount - GmatBaseParamCount];

};

#endif /* MeasurementModel_hpp */

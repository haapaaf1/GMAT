//$Header$
//------------------------------------------------------------------------------
//                              RangeMeasurementModel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/08/27
//
/**
 *
 * Implements the geometric right ascension and declination measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef RangeAzElMeasurementModel_hpp
#define RangeAzElMeasurementModel_hpp

#include "MeasurementModel.hpp"

class GMAT_API RangeAzElMeasurementModel : public MeasurementModel
{
public:
   RangeAzElMeasurementModel(const std::string name);
   RangeAzElMeasurementModel(const RangeAzElMeasurementModel &raeModel);
   RangeAzElMeasurementModel& operator=(const RangeAzElMeasurementModel &raeModel);
   virtual ~RangeAzElMeasurementModel();

   GmatBase *Clone() const;

   bool ComputeCartesianPartialDerivative(GroundStation &theStation,
          Spacecraft &theSat, Rvector &myCartDerivatives);
   bool ComputeMeasurement(GroundStation &theStation, Spacecraft &theSat,
          Rvector &myMeasurements);

private:

protected:


};

#endif	/* RangeAzElMeasurementModel_hpp */


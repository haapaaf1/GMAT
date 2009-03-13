//$Header$
//------------------------------------------------------------------------------
//                              GeometricRangeAzElMeasurementModel
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
 * Implements the geometric range, azimuth and elevation measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef GeometricRangeAzElMeasurementModel_hpp
#define GeometricRangeAzElMeasurementModel_hpp

#include "MeasurementModel.hpp"
#include "CoordinateConverter.hpp"
#include "BodyFixedAxes.hpp"

class GMAT_API GeometricRangeAzElMeasurementModel : public MeasurementModel
{
public:
    
   GeometricRangeAzElMeasurementModel(const std::string name);
   GeometricRangeAzElMeasurementModel(const GeometricRangeAzElMeasurementModel &raeModel);
   GeometricRangeAzElMeasurementModel& operator=(const GeometricRangeAzElMeasurementModel &raeModel);
   virtual ~GeometricRangeAzElMeasurementModel();

   virtual bool Initialize();
   
   GmatBase *Clone() const;
   
   bool ComputeMeasurement(Spacecraft *theSat);
   bool ComputeMeasurement(GroundStation *myStation, Spacecraft *theSat,
          LaVectorDouble &myMeasurements);
   bool ComputeCartesianPartialDerivative(Spacecraft *theSat);
   bool ComputeCartesianPartialDerivative(GroundStation *myStation,
          Spacecraft *theSat, LaGenMatDouble &myCartDerivatives);

private:

protected:
    
   Real           bodySpinRate;
   

};

#endif	/* GeometricRangeAzElMeasurementModel_hpp */



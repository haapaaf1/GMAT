//$Header$
//------------------------------------------------------------------------------
//                              RangeAzElMeasurementModel
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

#ifndef RangeAzElMeasurementModel_hpp
#define RangeAzElMeasurementModel_hpp

#include "MeasurementModel.hpp"
#include "CoordinateConverter.hpp"
#include "BodyFixedAxes.hpp"

class GMAT_API RangeAzElMeasurementModel : public MeasurementModel
{
public:
    
   RangeAzElMeasurementModel(const std::string name);
   RangeAzElMeasurementModel(const RangeAzElMeasurementModel &raeModel);
   RangeAzElMeasurementModel& operator=(const RangeAzElMeasurementModel &raeModel);
   virtual ~RangeAzElMeasurementModel();

   virtual bool Initialize();
   
   GmatBase *Clone() const;
   
   void SetGroundStation(GroundStation* gs);
   GroundStation* GetGroundStation();

   bool ComputeMeasurement(GroundStation *theStation, Spacecraft *theSat,
          LaVectorDouble &myMeasurements);
   bool ComputeCartesianPartialDerivative(GroundStation *theStation,
          Spacecraft *theSat, LaGenMatDouble &myCartDerivatives);

private:

protected:
    
   Real           bodySpinRate;
   
   // TODO: Is this the best way for the measurement model
   // to know what ground station it is associated with?
   GroundStation* theStation;

};

#endif	/* RangeAzElMeasurementModel_hpp */



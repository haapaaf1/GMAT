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

   Integer     GetDependentParamID(const std::string &str) const;


   // This function takes a spacecraft and computes a measurement
   // between the associated ground station a space point
   bool GetTheMeasurements(SpacePoint* theSpacePoint,
                                  const A1Mjd &atTime,
                                  LaGenMatDouble &theMeasurements);

   // Obtain the partials
   //bool GetThePartials(const std::string &param,
   //                           SpacePoint* theSpacePoint,
   //                           const A1Mjd &atTime,
   //                           LaGenMatDouble &theDerivatives);
   bool GetThePartials(const Integer &paramID,
                              SpacePoint* theSpacePoint,
                              const A1Mjd &atTime,
                              LaGenMatDouble &theDerivatives);

   bool ComputeCartesianPartialDerivative(SpacePoint* theSpacePoint,
                                          const A1Mjd &atTime,
                                          LaGenMatDouble &theDerivatives);

private:

protected:

   enum DEPENDENT_PARAMS
   {
      CARTESIAN_ID = MMDependentParamCount,
      EndDependentParams
   };


   Real           bodySpinRate;

   static const std::string  DEPENDENT_PARAMETER_TEXT[EndDependentParams-MMDependentParamCount];
   

};

#endif	/* GeometricRangeAzElMeasurementModel_hpp */



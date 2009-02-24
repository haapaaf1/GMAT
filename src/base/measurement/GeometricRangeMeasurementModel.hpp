//$Header$
//------------------------------------------------------------------------------
//                              GeometricRangeMeasurementModel
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
 * Implements the geometric range measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef _GeometricRangeMeasurementModel_HPP
#define	_GeometricRangeMeasurementModel_HPP

#include "MeasurementModel.hpp"
#include "CoordinateConverter.hpp"

class GMAT_API GeometricRangeMeasurementModel : public MeasurementModel
{
public:
    GeometricRangeMeasurementModel(const std::string name = "");
    GeometricRangeMeasurementModel(const GeometricRangeMeasurementModel &RMM);
    GeometricRangeMeasurementModel& operator=(const GeometricRangeMeasurementModel &RMM);
    virtual ~GeometricRangeMeasurementModel();

    void Initialize() const;
    
    virtual bool ComputeMeasurement(GroundStation *theStation,
          Spacecraft *theSat, LaVectorDouble &myMeasurements);
    virtual bool ComputeCartesianPartialDerivative(
          GroundStation *theStation, Spacecraft *theSat,
          LaGenMatDouble &myCartDerivatives);


    virtual GmatBase *Clone() const;

private:

protected:

};

#endif	/* _GeometricRangeMeasurementModel_HPP */


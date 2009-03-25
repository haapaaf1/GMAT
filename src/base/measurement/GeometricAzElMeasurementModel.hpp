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
 * Implements the geometric azimuth and elevation measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef _GeometricAzElMeasurementModel_HPP
#define	_GeometricAzElMeasurementModel_HPP

class GMAT_API GeometricAzElMeasurementModel : public MeasurmentModel
{
public:
    GeometricAzElMeasurementModel();
    GeometricAzElMeasurementModel(const GeometricAzElMeasurementModel &GeometricAzElMeasurementModel);
    GeometricAzElMeasurementModel& operator=(const GeometricAzElMeasurementModel &GeometricAzElMeasurementModel);
    virtual ~GeometricAzElMeasurementModel();

    bool ComputeMeasurement(Spacecraft* theSat);
    bool ComputeCartesianPartialDerivative(Spacecraft* theSat);

private:
    
protected:
    
}

#endif	/* _GeometricAzElMeasurementModel_HPP */


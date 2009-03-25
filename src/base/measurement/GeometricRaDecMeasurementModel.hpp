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

#ifndef _GeometricRaDecMeasurementModel_HPP
#define	_GeometricRaDecMeasurementModel_HPP

class GMAT_API GeometricRaDecMeasurementModel : public MeasurmentModel
{
public:
    GeometricRaDecMeasurementModel();
    GeometricRaDecMeasurementModel(const GeometricRaDecMeasurementModel &GeometricRaDecMeasurementModel);
    GeometricRaDecMeasurementModel& operator=(const GeometricRaDecMeasurementModel &GeometricRaDecMeasurementModel);
    virtual ~GeometricRaDecMeasurementModel();

    bool ComputeMeasurement(Spacecraft *theSat);
    bool ComputeCartesianPartialDerivative(Spacecraft *theSat);

private:
    
protected:
    
}

#endif	/* _GeometricRaDecMeasurementModel_HPP */


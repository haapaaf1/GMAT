//$Header$
//------------------------------------------------------------------------------
//                              GeometricRangeRaDecMeasurementModel
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
 * Implements the geometric range, right ascension and declination measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef _GeometricRangeRaDecMeasurementModel_HPP
#define	_GeometricRangeRaDecMeasurementModel_HPP

class GMAT_API GeometricRangeRaDecMeasurementModel : public MeasurmentModel
{
public:

   GeometricRangeRaDecMeasurementModel(const std::string name);
   GeometricRangeRaDecMeasurementModel(const GeometricRangeRaDecMeasurementModel &rrdModel);
   GeometricRangeRaDecMeasurementModel& operator=(const GeometricRangeRaDecMeasurementModel &rrdModel);
   virtual ~GeometricRangeRaDecMeasurementModel();

   virtual bool Initialize();

   GmatBase *Clone() const;

   bool ComputeMeasurement(Spacecraft *theSat);
   bool ComputeCartesianPartialDerivative(Spacecraft *theSat);

private:

protected:

   Real           bodySpinRate;

}

#endif	/* _GeometricRangeRaDecMeasurementModel_HPP */


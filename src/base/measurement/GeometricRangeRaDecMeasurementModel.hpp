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

}

#endif	/* _GeometricRangeRaDecMeasurementModel_HPP */

